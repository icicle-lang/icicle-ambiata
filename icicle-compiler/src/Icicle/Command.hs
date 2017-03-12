{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Icicle.Command (
    DictionaryFile(..)
  , InputFile(..)
  , InputFormat(..)
  , OutputFile(..)
  , OutputFormat(..)
  , Scope(..)
  , Query(..)
  , QueryStatistics(..)
  , QueryOptions(..)

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
import           Icicle.Sea.IO
import           Icicle.Sea.Eval
import qualified Icicle.Source.Checker as Source
import qualified Icicle.Storage.Dictionary.Toml as Toml

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import           Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

import           System.FilePath (FilePath)
import           System.IO (IO, IOMode(..), withFile, hFileSize)

import           Text.Printf (printf)
import           Text.Show.Pretty (ppShow)

import           Prelude (putStrLn)

import           P

import           X.Control.Monad.Trans.Either


data IcicleError =
    IcicleDictionaryImportError Toml.DictionaryImportError
  | IcicleCompileError          (Compiler.ErrorCompile Compiler.Var)
  | IcicleSeaError              SeaError
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
  , queryFleet           :: SeaFleet a
  , queryInputPath       :: FilePath
  , queryOutputPath      :: FilePath
  , queryDropPath        :: FilePath
  , queryChordPath       :: Maybe FilePath
  , queryCompilationTime :: NominalDiffTime
  , queryFactsLimit      :: Int
  , queryUseDrop         :: FlagUseDrop
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
  , optDrop         :: Maybe FilePath
  , optUseDrop      :: FlagUseDrop
  } deriving (Eq, Ord, Show)

data DictionaryFile =
    DictionaryToml FilePath
  | DictionaryCode FilePath
    deriving (Eq, Ord, Show)

data InputFile =
  InputFile {
      inputFormat :: InputFormat
    , inputPath :: FilePath
    } deriving (Eq, Ord, Show)

data InputFormat =
    InputSparsePsv
  | InputDensePsv
  | InputZebra
    deriving (Eq, Ord, Show)

data OutputFile =
  OutputFile {
      outputFormat :: OutputFormat
    , outputPath :: FilePath
    } deriving (Eq, Ord, Show)

data OutputFormat =
    OutputSparsePsv
  | OutputDensePsv
    deriving (Eq, Ord, Show)

data Scope a =
    ScopeSnapshot Time
  | ScopeChord a
    deriving (Eq, Ord, Show, Functor)

psvOfOutputFormat :: OutputFormat -> PsvOutputFormat
psvOfOutputFormat = \case
  OutputSparsePsv ->
    PsvOutputSparse
  OutputDensePsv ->
    PsvOutputDense

chordPathOfScope :: Scope a -> Maybe a
chordPathOfScope = \case
  ScopeSnapshot _ ->
    Nothing
  ScopeChord path ->
    Just path

modeOfScope :: Scope a -> Mode
modeOfScope = \case
  ScopeSnapshot time ->
    Snapshot time
  ScopeChord _ ->
    Chords

createPsvQuery :: QueryOptions -> EitherT IcicleError IO (Query PsvState)
createPsvQuery = createQuery

createZebraQuery :: QueryOptions -> EitherT IcicleError IO (Query ZebraState)
createZebraQuery = createQuery

createQuery :: QueryOptions -> EitherT IcicleError IO (Query a)
createQuery c = do
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
       mkQueryFleet (optInput c) chordPath dictionaryPath

    DictionaryToml dictionaryPath -> do
      (dictionary, format) <-
        loadDictionary
          dictionaryPath
          (inputFormat $ optInput c)
          (outputFormat $ optOutput c)
          (optScope c)

      compileFleet dictionary format (inputPath $ optInput c) chordPath

  end <- liftIO getCurrentTime

  return Query {
      querySource          = code
    , queryFleet           = fleet
    , queryInputPath       = inputPath $ optInput c
    , queryOutputPath      = outputPath $ optOutput c
    , queryChordPath       = chordPath
    , queryCompilationTime = end `diffUTCTime` start
    , queryFactsLimit      = optFactsLimit c
    , queryDropPath        = fromMaybe (outputPath (optOutput c) <> "-dropped.txt") (optDrop c)
    , queryUseDrop         = optUseDrop c
    }

mkQueryFleet ::
     InputFile
  -> Maybe FilePath
  -> FilePath
  -> EitherT IcicleError IO (Text, SeaFleet s)
mkQueryFleet input chord source = do
  -- FIXME using a dummy format here as we are not using it to generate C
  -- we actually only need to differentiate between psv/zebra, make this better
  let format = case inputFormat input of
        InputSparsePsv ->
          FormatPsv $ PsvConfig
            (PsvInputConfig Chords PsvInputSparse)
            (PsvOutputConfig Chords PsvOutputSparse defaultOutputMissing)
        InputDensePsv ->
          FormatPsv $ PsvConfig
            (PsvInputConfig Chords PsvInputSparse)
            (PsvOutputConfig Chords PsvOutputSparse defaultOutputMissing)
        InputZebra ->
          FormatZebra defaultZebraConfig Chords
            (PsvOutputConfig Chords PsvOutputSparse defaultOutputMissing)

  let cfg = HasInput format (InputOpts AllowDupTime Map.empty) (inputPath input)

  code  <- liftIO $ Text.readFile source
  fleet <- firstEitherT IcicleSeaError (seaCreate CacheSea cfg chord 0 code)
  return (code, fleet)

compileFleet ::
     Dictionary
  -> IOFormat
  -> FilePath
  -> Maybe FilePath
  -> EitherT IcicleError IO (Text, SeaFleet s)
compileFleet dictionary format input chords = do
  let cfg = HasInput format (InputOpts AllowDupTime (tombstonesOfDictionary dictionary)) input

  avalanche <- hoistEither $ compileAvalanche dictionary
  let avalancheL = Map.toList avalanche

  let attrs = List.sort $ getConcreteFeatures dictionary

  code  <- firstEitherT IcicleSeaError (hoistEither (codeOfPrograms cfg attrs avalancheL))
  fleet <- firstEitherT IcicleSeaError (seaCompile CacheSea cfg attrs avalanche chords)

  return (code, fleet)

loadDictionary ::
     FilePath
  -> InputFormat
  -> OutputFormat
  -> Scope a
  -> EitherT IcicleError IO (Dictionary, IOFormat)
loadDictionary path iformat oformat0 scope =
  let
    mode =
      modeOfScope scope

    oformat =
      psvOfOutputFormat oformat0

    oconfig =
      PsvOutputConfig mode oformat defaultOutputMissing

  in
    case iformat of
      InputSparsePsv -> do
        d <- firstEitherT IcicleDictionaryImportError $
          Toml.loadDictionary Source.optionSmallData Toml.ImplicitPrelude path

        let f = FormatPsv $ PsvConfig
                  (PsvInputConfig mode PsvInputSparse)
                  oconfig

        return (d, f)

      InputDensePsv -> do
        (d, dense) <- firstEitherT IcicleDictionaryImportError $
          Toml.loadDenseDictionary Source.optionSmallData Toml.ImplicitPrelude path Nothing

        let f = FormatPsv $ PsvConfig
                  (PsvInputConfig mode (PsvInputDense dense (denseSelectedFeed dense)))
                  oconfig

        return (d, f)

      InputZebra -> do
        d <- firstEitherT IcicleDictionaryImportError $
          Toml.loadDictionary Source.optionSmallData Toml.ImplicitPrelude path

        let f = FormatZebra defaultZebraConfig mode oconfig

        return (d, f)

compileDictionary ::
     FilePath
  -> InputFormat
  -> OutputFormat
  -> Scope a
  -> EitherT IcicleError IO Text
compileDictionary dictionaryPath iformat oformat scope = do
  -- FIXME We really need to include InputFormat/OutputFormat/Scope in the compiled
  -- FIXME code so that we don't accidentally run with the wrong options.
  (dictionary, format) <- loadDictionary dictionaryPath iformat oformat scope

  let cfg = HasInput format (InputOpts AllowDupTime (tombstonesOfDictionary dictionary)) ()
  avalanche <- fmap Map.toList . hoistEither $ compileAvalanche dictionary

  let attrs = List.sort $ getConcreteFeatures dictionary

  firstT IcicleSeaError . hoistEither $ codeOfPrograms cfg attrs avalanche

compileAvalanche ::
     Dictionary
  -> Either IcicleError (Map Attribute (NonEmpty (Program (Annot Compiler.AnnotUnit) Compiler.Var Prim)))
compileAvalanche dictionary =
  first IcicleCompileError $
    Compiler.avalancheOfDictionary Compiler.defaultCompileOptions dictionary

releaseQuery :: Query a -> EitherT IcicleError IO ()
releaseQuery b =
  seaRelease (queryFleet b)

runPsvQuery :: Query PsvState -> EitherT IcicleError IO QueryStatistics
runPsvQuery b = do
  let fleet      = queryFleet        b
      input      = queryInputPath    b
      output     = queryOutputPath   b
      dropPath   = queryDropPath     b
      chordPath  = queryChordPath    b
      discard    = queryUseDrop b
      constants  = defaultPsvConstants { psvFactsLimit = queryFactsLimit b }

  start <- liftIO getCurrentTime
  stats <- firstEitherT IcicleSeaError
         $ seaPsvSnapshotFilePath fleet input output dropPath chordPath discard constants
  end   <- liftIO getCurrentTime
  size  <- liftIO (withFile input ReadMode hFileSize)

  return QueryStatistics {
      queryTime     = end `diffUTCTime` start
    , queryFacts    = psvFactsRead    stats
    , queryEntities = psvEntitiesRead stats
    , queryBytes    = fromIntegral size
    }

runZebraQuery :: Query ZebraState -> EitherT IcicleError IO QueryStatistics
runZebraQuery b = do
  let fleet      = queryFleet        b
      input      = queryInputPath    b
      output     = queryOutputPath   b
      chordPath  = queryChordPath    b

  start <- liftIO getCurrentTime
  stats <- firstEitherT IcicleSeaError
         $ seaZebraSnapshotFilePath fleet input output chordPath
  end   <- liftIO getCurrentTime
  size  <- liftIO (withFile input ReadMode hFileSize)

  return QueryStatistics {
      queryTime     = end `diffUTCTime` start
    , queryFacts    = zebraFactsRead    stats
    , queryEntities = zebraEntitiesRead stats
    , queryBytes    = fromIntegral size
    }
