{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Icicle.Benchmark (
    Command (..)
  , DictionaryFile (..)
  , InputFile (..)
  , InputFormat (..)
  , OutputFile (..)
  , Scope (..)
  , BenchError(..)
  , Benchmark(..)
  , BenchmarkResult(..)
  , optInputFormat
  , optInputPath
  , optOutputPath
  , optOutputPsv
  , createBenchmark
  , createPsvBench
  , createZebraBench
  , runPsvBench
  , runZebraBench
  , releaseBenchmark
  ) where

import           Icicle.Dictionary

import           Icicle.Data.Time (Time(..))

import qualified Icicle.Compiler.Source as P
import qualified Icicle.Compiler        as P

import           Icicle.Sea.Eval

import qualified Icicle.Source.Checker as SC

import           Icicle.Storage.Dictionary.Toml

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Map as Map
import qualified Data.Text.IO as Text
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

import           System.FilePath (FilePath)
import           System.IO (IO, IOMode(..), withFile, hFileSize)

import           Text.Printf (printf)

import           Prelude (putStrLn)

import           P

import           X.Control.Monad.Trans.Either


------------------------------------------------------------------------

data BenchError =
    BenchDictionaryImportError DictionaryImportError
  | BenchCompileError          (P.ErrorCompile P.Var)
  | BenchSeaError              SeaError
  deriving (Show)

data Benchmark a = Benchmark {
    benchSource          :: Text
  , benchFleet           :: SeaFleet a
  , benchInputPath       :: FilePath
  , benchOutputPath      :: FilePath
  , benchDropPath        :: FilePath
  , benchChordPath       :: Maybe FilePath
  , benchCompilationTime :: NominalDiffTime
  , benchFactsLimit      :: Int
  , benchUseDrop         :: FlagUseDrop
  }

data BenchmarkResult = BenchmarkResult {
    benchTime     :: NominalDiffTime
  , benchEntities :: Int64
  , benchFacts    :: Int64
  , benchBytes    :: Int64
  } deriving (Eq, Ord, Show)

data Command = Command {
    optDictionary   :: DictionaryFile
  , optInput        :: InputFile
  , optOutput       :: OutputFile
  , optOutputCode   :: Maybe FilePath
  , optScope        :: Scope
  , optFactsLimit   :: Int
  , optDrop         :: Maybe FilePath
  , optUseDrop      :: FlagUseDrop
  }

data DictionaryFile =
    DictionaryToml FilePath
  | DictionaryCode FilePath

data InputFile =
    InputSparsePsv FilePath
  | InputDensePsv FilePath
  | InputZebra FilePath

data InputFormat =
    InputFormatPsv
  | InputFormatZebra

data OutputFile =
    OutputSparsePsv FilePath
  | OutputDensePsv FilePath

data Scope =
    ScopeSnapshot Time
  | ScopeChord FilePath

optInputFormat :: Command -> InputFormat
optInputFormat c =
  case optInput c of
    InputSparsePsv _ ->
      InputFormatPsv
    InputDensePsv _ ->
      InputFormatPsv
    InputZebra _ ->
      InputFormatZebra

optInputPath :: Command -> FilePath
optInputPath c =
  case optInput c of
    InputSparsePsv path ->
      path
    InputDensePsv path ->
      path
    InputZebra path ->
      path

optOutputPath :: Command -> FilePath
optOutputPath c =
  case optOutput c of
    OutputSparsePsv path ->
      path
    OutputDensePsv path ->
      path

optOutputPsv :: Command -> PsvOutputFormat
optOutputPsv c =
  case optOutput c of
    OutputSparsePsv _ ->
      PsvOutputSparse
    OutputDensePsv _ ->
      PsvOutputDense

createPsvBench :: Command -> EitherT BenchError IO (Benchmark PsvState)
createPsvBench = createBenchmark

createZebraBench :: Command -> EitherT BenchError IO (Benchmark ZebraState)
createZebraBench = createBenchmark

createBenchmark :: Command -> EitherT BenchError IO (Benchmark a)
createBenchmark c = do
  let
    (mode, chordPath) = case optScope c of
      ScopeSnapshot time
        -> (Snapshot time, Nothing)
      ScopeChord path
        -> (Chords, Just path)

  chordStart <- liftIO getCurrentTime

  when (isJust chordPath) $
    liftIO (putStrLn "icicle-bench: preparing chord")
  chordEnd <- liftIO getCurrentTime
  let chordSecs = realToFrac (chordEnd `diffUTCTime` chordStart) :: Double
  when (isJust chordPath) $
    liftIO (printf "icicle-bench: chord preparation time = %.2fs\n" chordSecs)

  start <- liftIO getCurrentTime

  (code, fleet) <- case optDictionary c of
    DictionaryCode dictionaryPath ->
       mkBenchFleet (optInput c) chordPath dictionaryPath

    DictionaryToml dictionaryPath -> do
      (dictionary, format) <- case optInput c of
        InputSparsePsv _ -> do
          d <- firstEitherT BenchDictionaryImportError $
            loadDictionary SC.optionSmallData ImplicitPrelude dictionaryPath

          let f = FormatPsv $ psvDefaultConstants
                    (PsvInputConfig  mode PsvInputSparse)
                    (PsvOutputConfig mode (optOutputPsv c) defaultOutputMissing)

          return (d, f)

        InputDensePsv _ -> do
          (d, dense) <- firstEitherT BenchDictionaryImportError $
            loadDenseDictionary SC.optionSmallData ImplicitPrelude dictionaryPath Nothing

          let f = FormatPsv $ psvDefaultConstants
                    (PsvInputConfig  mode (PsvInputDense dense (denseSelectedFeed dense)))
                    (PsvOutputConfig mode (optOutputPsv c) defaultOutputMissing)

          return (d, f)

        InputZebra _ -> do
          d <- firstEitherT BenchDictionaryImportError $
            loadDictionary SC.optionSmallData ImplicitPrelude dictionaryPath

          let f = FormatZebra mode (PsvOutputConfig mode (optOutputPsv c) defaultOutputMissing)

          return (d, f)

      compileBench dictionary format (optInputPath c) chordPath


  end <- liftIO getCurrentTime

  return Benchmark {
      benchSource          = code
    , benchFleet           = fleet
    , benchInputPath       = optInputPath c
    , benchOutputPath      = optOutputPath c
    , benchChordPath       = chordPath
    , benchCompilationTime = end `diffUTCTime` start
    , benchFactsLimit      = optFactsLimit c
    , benchDropPath        = fromMaybe (optOutputPath c <> "-dropped.txt") (optDrop c)
    , benchUseDrop         = optUseDrop c
    }

mkBenchFleet ::
     InputFile
  -> Maybe FilePath
  -> FilePath
  -> EitherT BenchError IO (Text, SeaFleet s)
mkBenchFleet input chord source = do
  -- FIXME using a dummy format here as we are not using it to generate C
  -- we actually only need to differentiate between psv/zebra, make this better
  let (path, format) = case input of
        InputSparsePsv p ->
          (p, FormatPsv $ psvDefaultConstants
                (PsvInputConfig Chords PsvInputSparse)
                (PsvOutputConfig Chords PsvOutputSparse defaultOutputMissing))
        InputDensePsv p ->
          (p, FormatPsv $ psvDefaultConstants
                (PsvInputConfig Chords PsvInputSparse)
                (PsvOutputConfig Chords PsvOutputSparse defaultOutputMissing))
        InputZebra p ->
          (p, FormatZebra Chords
                (PsvOutputConfig Chords PsvOutputSparse defaultOutputMissing))

  let cfg = HasInput format (InputOpts AllowDupTime Map.empty) path
  code  <- liftIO $ Text.readFile source
  fleet <- firstEitherT BenchSeaError (seaCreate CacheSea cfg chord code)
  return (code, fleet)

compileBench ::
     Dictionary
  -> IOFormat
  -> FilePath
  -> Maybe FilePath
  -> EitherT BenchError IO (Text, SeaFleet s)
compileBench dictionary format input chords = do
  avalanche  <- hoistEither
              $ first BenchCompileError
              $ P.avalancheOfDictionary P.defaultCompileOptions dictionary

  let cfg = HasInput format (InputOpts AllowDupTime (tombstonesOfDictionary dictionary)) input
  let avalancheL = Map.toList avalanche

  code  <- firstEitherT BenchSeaError (hoistEither (codeOfPrograms cfg avalancheL))
  fleet <- firstEitherT BenchSeaError (seaCompile CacheSea cfg avalanche chords)

  return (code, fleet)

releaseBenchmark :: Benchmark a -> EitherT BenchError IO ()
releaseBenchmark b =
  seaRelease (benchFleet b)

runPsvBench :: Benchmark PsvState -> EitherT BenchError IO BenchmarkResult
runPsvBench b = do
  let fleet      = benchFleet        b
      inputPath  = benchInputPath    b
      outputPath = benchOutputPath   b
      dropPath   = benchDropPath     b
      chordPath  = benchChordPath    b
      limit      = benchFactsLimit   b
      discard    = benchUseDrop b

  start <- liftIO getCurrentTime
  stats <- firstEitherT BenchSeaError
         $ seaPsvSnapshotFilePath fleet inputPath outputPath dropPath chordPath limit discard
  end   <- liftIO getCurrentTime
  size  <- liftIO (withFile inputPath ReadMode hFileSize)

  return BenchmarkResult {
      benchTime     = end `diffUTCTime` start
    , benchFacts    = psvFactsRead    stats
    , benchEntities = psvEntitiesRead stats
    , benchBytes    = fromIntegral size
    }

runZebraBench :: Benchmark ZebraState -> EitherT BenchError IO BenchmarkResult
runZebraBench b = do
  let fleet      = benchFleet        b
      inputPath  = benchInputPath    b
      outputPath = benchOutputPath   b
      chordPath  = benchChordPath    b

  start <- liftIO getCurrentTime
  stats <- firstEitherT BenchSeaError
         $ seaZebraSnapshotFilePath fleet inputPath outputPath chordPath
  end   <- liftIO getCurrentTime
  size  <- liftIO (withFile inputPath ReadMode hFileSize)

  return BenchmarkResult {
      benchTime     = end `diffUTCTime` start
    , benchFacts    = zebraFactsRead    stats
    , benchEntities = zebraEntitiesRead stats
    , benchBytes    = fromIntegral size
    }
