{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Icicle.Command (
    DictionaryFile(..)
  , InputFile(..)
  , InputFormatFlag (..)
  , OutputFile(..)
  , OutputFormatFlag (..)
  , Scope(..)
  , Query(..)
  , QueryStatistics(..)
  , QueryOptions(..)

  , CompilerFlags(..)
  , defaultCompilerFlags

  , IcicleError(..)
  , renderIcicleError

  , compileDictionary

  , createQuery
  , createPsvQuery
  , createZebraQuery
  , runPsvQuery
  , runZebraQuery
  , releaseQuery
  ) where

import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Avalanche.Program (Program)
import           Icicle.Common.Annot (Annot)
import qualified Icicle.Compiler as Compiler
import qualified Icicle.Compiler.Source as Compiler
import           Icicle.Data (Attribute(..))
import           Icicle.Data.Time (Time(..))
import           Icicle.Dictionary
import           Icicle.Internal.Pretty (pretty)
import qualified Icicle.Sea.IO as Sea
import qualified Icicle.Sea.Eval as Sea
import qualified Icicle.Source.Checker as Source
import qualified Icicle.Storage.Dictionary.Toml as Toml

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Morph (hoist)

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import           Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

import           System.FilePath (FilePath, dropExtension)
import           System.IO (IO, IOMode(..), withFile, hFileSize)

import           Text.Printf (printf)
import           Text.Show.Pretty (ppShow)

import           Prelude (putStrLn)

import           P

import           X.Control.Monad.Trans.Either


data IcicleError =
    IcicleDictionaryImportError Toml.DictionaryImportError
  | IcicleCompileError          (Compiler.ErrorCompile Compiler.Var)
  | IcicleSeaError              Sea.SeaError
    deriving (Show)

renderIcicleError :: IcicleError -> Text
renderIcicleError = \case
  IcicleDictionaryImportError x ->
    Text.pack $ ppShow x
  IcicleCompileError x ->
    Text.pack $ ppShow x
  IcicleSeaError err ->
    Text.pack . show $ pretty err

data Query a = Query {
    querySource          :: Text
  , queryFleet           :: Sea.SeaFleet a
  , queryInputPath       :: FilePath
  , queryOutputPath      :: FilePath
  , queryDropPath        :: FilePath
  , queryChordPath       :: Maybe FilePath
  , queryCompilationTime :: NominalDiffTime
  , queryFactsLimit      :: Int
  -- ^ only applies to psv input
  , queryUseDrop         :: Sea.FlagUseDrop
  -- ^ only applies to psv input
  , queryChunkFactCount       :: Sea.ZebraChunkFactCount
  -- ^ only applies to zebra input
  , queryAllocLimitGB      :: Sea.ZebraAllocLimitGB
  -- ^ only applies to zebra input
  , queryOutputFormatFlag :: OutputFormatFlag
  }

data QueryStatistics = QueryStatistics {
    queryTime     :: NominalDiffTime
  , queryEntities :: Int64
  , queryFacts    :: Int64
  , queryBytes    :: Int64
  } deriving (Eq, Ord, Show)

data QueryOptions = QueryOptions {
    optDictionary   :: DictionaryFile
  , optInput        :: InputFile
  , optOutput       :: OutputFile
  , optOutputCode   :: Maybe FilePath
  , optScope        :: Scope FilePath
  , optFactsLimit   :: Int
  -- ^ only applies to psv input
  , optDrop         :: Maybe FilePath
  , optUseDrop      :: Sea.FlagUseDrop
  , optChunkFactCount :: Sea.ZebraChunkFactCount
  -- ^ only applies to zebra input
  , optAllocLimitGB :: Sea.ZebraAllocLimitGB
  -- ^ only applies to zebra input
  } deriving (Eq, Ord, Show)

data DictionaryFile =
    DictionaryToml FilePath
  | DictionaryCode FilePath
    deriving (Eq, Ord, Show)

data InputFile =
  InputFile {
      inputFormat :: InputFormatFlag
    , inputPath :: FilePath
    } deriving (Eq, Ord, Show)

data InputFormatFlag =
    InputSparsePsv
  | InputDensePsv
  | InputZebra
    deriving (Eq, Ord, Show)

data OutputFile =
  OutputFile {
      outputFormat :: OutputFormatFlag
    , outputPath :: FilePath
    } deriving (Eq, Ord, Show)

data OutputFormatFlag =
    OutputSparsePsv
  | OutputDensePsv
  | OutputZebra
    deriving (Eq, Ord, Show)

data Scope a =
    ScopeSnapshot Time
  | ScopeChord a
    deriving (Eq, Ord, Show, Functor)

data CompilerFlags =
  CompilerFlags {
    compilerMaximumQueriesPerKernel :: Int
  }
  deriving (Eq, Ord, Show)

defaultCompilerFlags :: CompilerFlags
defaultCompilerFlags = CompilerFlags 100


chordPathOfScope :: Scope a -> Maybe a
chordPathOfScope = \case
  ScopeSnapshot _ ->
    Nothing
  ScopeChord path ->
    Just path

modeOfScope :: Scope a -> Sea.Mode
modeOfScope = \case
  ScopeSnapshot time ->
    Sea.Snapshot time
  ScopeChord _ ->
    Sea.Chords

createPsvQuery :: QueryOptions -> EitherT IcicleError (ResourceT IO) (Query Sea.PsvState)
createPsvQuery = createQuery

createZebraQuery :: QueryOptions -> EitherT IcicleError (ResourceT IO) (Query Sea.ZebraState)
createZebraQuery = createQuery

createQuery :: QueryOptions -> EitherT IcicleError (ResourceT IO) (Query a)
createQuery c = do
  let dropPath = fromMaybe (dropExtension (outputPath (optOutput c)) <> "_dropped.txt") (optDrop c)
  let chordPath = chordPathOfScope $ optScope c
  chordStart <- liftIO getCurrentTime

  when (isJust chordPath) $
    liftIO (putStrLn "icicle: preparing chord")
  chordEnd <- liftIO getCurrentTime
  let chordSecs = realToFrac (chordEnd `diffUTCTime` chordStart) :: Double
  when (isJust chordPath) $
    liftIO (printf "icicle: chord preparation time = %.2fs\n" chordSecs)

  start <- liftIO getCurrentTime

  (code, fleet) <- case optDictionary c of
    DictionaryCode dictionaryPath ->
       mkQueryFleet (optInput c) (optOutput c) chordPath dictionaryPath

    DictionaryToml dictionaryPath -> do
      (dictionary, inputFormat', outputFormat') <-
        hoist liftIO $ loadDictionary
          dictionaryPath
          (inputFormat $ optInput c)
          (outputFormat $ optOutput c)
          (optScope c)

      compileFleet dictionary inputFormat' outputFormat' (inputPath (optInput c)) chordPath

  end <- liftIO getCurrentTime

  return Query {
      querySource          = code
    , queryFleet           = fleet
    , queryInputPath       = inputPath $ optInput c
    , queryOutputPath      = outputPath $ optOutput c
    , queryChordPath       = chordPath
    , queryCompilationTime = end `diffUTCTime` start
    , queryFactsLimit      = optFactsLimit c
    , queryDropPath        = dropPath
    , queryUseDrop         = optUseDrop c
    , queryChunkFactCount = optChunkFactCount c
    , queryAllocLimitGB = optAllocLimitGB c
    , queryOutputFormatFlag = outputFormat (optOutput c)
    }

-- FIXME Using dummy formats here as we are not using it to generate C
-- At this stage we actually only need to differentiate between Psv and Zebra
mkQueryFleet ::
     InputFile
  -> OutputFile
  -> Maybe FilePath
  -> FilePath
  -> EitherT IcicleError (ResourceT IO) (Text, Sea.SeaFleet s)
mkQueryFleet input output chord source = do
  -- FIXME using a dummy format here as we are not using it to generate C
  -- we actually only need to differentiate between psv/zebra, make this better
  let
    inputFormat' =
      case inputFormat input of
        InputSparsePsv ->
          Sea.InputFormatPsv . Sea.PsvInputConfig Sea.Chords $ Sea.PsvInputSparse
        InputDensePsv ->
          Sea.InputFormatPsv . Sea.PsvInputConfig Sea.Chords $ Sea.PsvInputSparse
        InputZebra ->
          Sea.InputFormatZebra Sea.defaultZebraInputConfig $ Sea.Chords
    outputFormat' =
      case outputFormat output of
        OutputSparsePsv ->
          Sea.OutputFormatPsv . Sea.PsvOutputConfig Sea.Chords Sea.PsvOutputSparse $ Sea.defaultOutputMissing
        OutputDensePsv ->
          Sea.OutputFormatPsv . Sea.PsvOutputConfig Sea.Chords Sea.PsvOutputSparse $ Sea.defaultOutputMissing
        OutputZebra ->
          Sea.OutputFormatZebra

  let
    hasInput =
      Sea.HasInput inputFormat' (Sea.InputOpts Sea.AllowDupTime Map.empty) (inputPath input)
    hasOutput =
      Sea.HasOutput outputFormat'

  code  <- liftIO $ Text.readFile source
  fleet <- firstEitherT IcicleSeaError (Sea.seaCreate Sea.CacheSea hasInput hasOutput chord code [])
  return (code, fleet)

compileFleet ::
     Dictionary
  -> Sea.InputFormat
  -> Sea.OutputFormat
  -> FilePath
  -> Maybe FilePath
  -> EitherT IcicleError (ResourceT IO) (Text, Sea.SeaFleet s)
compileFleet dictionary inputFormat0 outputFormat0 input chords = do
  let
    hasInput =
      Sea.HasInput inputFormat0 (Sea.InputOpts Sea.AllowDupTime (tombstonesOfDictionary dictionary)) input
    hasOutput =
      Sea.HasOutput outputFormat0

  avalanche <- hoistEither $ compileAvalanche dictionary defaultCompilerFlags
  let avalancheL = Map.toList avalanche

  let attrs = List.sort $ getConcreteFeatures dictionary

  code  <- firstEitherT IcicleSeaError (hoistEither (Sea.codeOfPrograms hasInput hasOutput attrs avalancheL))
  fleet <- firstEitherT IcicleSeaError (Sea.seaCompile Sea.CacheSea hasInput hasOutput attrs avalanche chords)

  return (code, fleet)

loadDictionary ::
     FilePath
  -> InputFormatFlag
  -> OutputFormatFlag
  -> Scope a
  -> EitherT IcicleError IO (Dictionary, Sea.InputFormat, Sea.OutputFormat)
loadDictionary path inputFormat0 outputFormat0 scope = do
  let
    mode =
      modeOfScope scope

  (dictionary, inputFormat1) <- case inputFormat0 of
    InputSparsePsv -> do
      d <- firstEitherT IcicleDictionaryImportError $ Toml.loadDictionary Source.optionSmallData Toml.ImplicitPrelude path
      let f = Sea.InputFormatPsv (Sea.PsvInputConfig mode Sea.PsvInputSparse)
      return (d, f)

    InputDensePsv -> do
      (d, dense) <- firstEitherT IcicleDictionaryImportError $ Toml.loadDenseDictionary Source.optionSmallData Toml.ImplicitPrelude path Nothing
      let f = Sea.InputFormatPsv . Sea.PsvInputConfig mode . Sea.PsvInputDense dense . Sea.denseSelectedFeed $ dense
      return (d, f)

    InputZebra -> do
      d <- firstEitherT IcicleDictionaryImportError $ Toml.loadDictionary Source.optionSmallData Toml.ImplicitPrelude path
      let f = Sea.InputFormatZebra Sea.defaultZebraInputConfig mode
      return (d, f)

  let
    outputFormat1 =
      case outputFormat0 of
        OutputSparsePsv ->
          Sea.OutputFormatPsv . Sea.PsvOutputConfig mode Sea.PsvOutputSparse $ Sea.defaultOutputMissing
        OutputDensePsv ->
          Sea.OutputFormatPsv . Sea.PsvOutputConfig mode Sea.PsvOutputDense $ Sea.defaultOutputMissing
        OutputZebra ->
          Sea.OutputFormatZebra

  return (dictionary, inputFormat1, outputFormat1)

compileDictionary ::
     FilePath
  -> InputFormatFlag
  -> OutputFormatFlag
  -> Scope a
  -> CompilerFlags
  -> EitherT IcicleError IO Text
compileDictionary dictionaryPath iformat oformat scope cflags = do
  -- FIXME We really need to include InputFormat/OutputFormat/Scope in the compiled
  -- FIXME code so that we don't accidentally run with the wrong options.
  (dictionary, inputFormat1, outputFormat1) <- loadDictionary dictionaryPath iformat oformat scope

  let
    hasInput =
      Sea.HasInput inputFormat1 (Sea.InputOpts Sea.AllowDupTime (tombstonesOfDictionary dictionary)) ()
    hasOutput =
      Sea.HasOutput outputFormat1

  avalanche <- fmap Map.toList . hoistEither $ compileAvalanche dictionary cflags
  let attrs = List.sort $ getConcreteFeatures dictionary
  firstT IcicleSeaError . hoistEither $ Sea.codeOfPrograms hasInput hasOutput attrs avalanche

compileAvalanche ::
     Dictionary
  -> CompilerFlags
  -> Either IcicleError (Map Attribute (NonEmpty (Program (Annot Compiler.AnnotUnit) Compiler.Var Prim)))
compileAvalanche dictionary cflags =
  first IcicleCompileError $ Compiler.avalancheOfDictionary (compileOptionsOfCompilerFlags cflags) dictionary


compileOptionsOfCompilerFlags :: CompilerFlags -> Compiler.IcicleCompileOptions
compileOptionsOfCompilerFlags cflags =
  Compiler.defaultCompileOptions
  { Compiler.icicleFusionOptions = Compiler.FusionOptions $ compilerMaximumQueriesPerKernel cflags }


releaseQuery :: Query a -> EitherT IcicleError IO ()
releaseQuery b =
  Sea.seaRelease (queryFleet b)

runPsvQuery :: Query Sea.PsvState -> EitherT IcicleError IO QueryStatistics
runPsvQuery b = do
  let fleet      = queryFleet        b
      input      = queryInputPath    b
      output     = queryOutputPath   b
      dropPath   = queryDropPath     b
      chordPath  = queryChordPath    b
      discard    = queryUseDrop b
      constants  = Sea.defaultPsvConstants { Sea.psvFactsLimit = queryFactsLimit b }

  start <- liftIO getCurrentTime
  stats <- firstEitherT IcicleSeaError
         $ Sea.seaPsvSnapshotFilePath fleet input output dropPath chordPath discard constants
  end   <- liftIO getCurrentTime
  size  <- liftIO (withFile input ReadMode hFileSize)

  return QueryStatistics {
      queryTime     = end `diffUTCTime` start
    , queryFacts    = Sea.psvFactsRead    stats
    , queryEntities = Sea.psvEntitiesRead stats
    , queryBytes    = fromIntegral size
    }

runZebraQuery :: Query Sea.ZebraState -> EitherT IcicleError IO QueryStatistics
runZebraQuery b = do
  let fleet      = queryFleet        b
      input      = queryInputPath    b
      output     = queryOutputPath   b
      chordPath  = queryChordPath    b
      dropPath   = queryDropPath     b
      chunkSize  = queryChunkFactCount b
      allocLimit = queryAllocLimitGB   b

  start <- liftIO getCurrentTime
  stats <- firstEitherT IcicleSeaError $
    Sea.seaZebraSnapshotFilePath fleet input output dropPath chordPath (Sea.ZebraInputConfig chunkSize allocLimit) (queryOutputFormatFlag b == OutputZebra)

  end   <- liftIO getCurrentTime
  size  <- liftIO (withFile input ReadMode hFileSize)

  return QueryStatistics {
      queryTime     = end `diffUTCTime` start
    , queryFacts    = Sea.zebraFactsRead    stats
    , queryEntities = Sea.zebraEntitiesRead stats
    , queryBytes    = fromIntegral size
    }
