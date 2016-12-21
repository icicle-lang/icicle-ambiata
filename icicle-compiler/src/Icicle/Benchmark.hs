{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Icicle.Benchmark (
    Command (..)
  , FlagMode (..)
  , FlagInput (..)
  , FlagInputPsv (..)
  , FlagOutputPsv
  , BenchError(..)
  , Benchmark(..)
  , BenchmarkResult(..)
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
    optDictionary   :: FilePath
  , optInput        :: FilePath
  , optOutput       :: FilePath
  , optC            :: FilePath
  , optFlagMode     :: FlagMode
  , optSnapshot     :: Maybe Time
  , optChords       :: Maybe FilePath
  , optFactsLimit   :: Int
  , optDrop         :: Maybe FilePath
  , optUseDrop      :: FlagUseDrop
  , optInputFormat  :: FlagInput
  , optInputPsv     :: FlagInputPsv
  , optOutputPsv    :: FlagOutputPsv
  }

data FlagMode = FlagSnapshot | FlagChords
data FlagInput = FlagInputPsv | FlagInputZebra
data FlagInputPsv = FlagInputPsvSparse | FlagInputPsvDense
type FlagOutputPsv = PsvOutputFormat


createPsvBench :: Command -> EitherT BenchError IO (Benchmark PsvState)
createPsvBench = createBenchmark

createZebraBench :: Command -> EitherT BenchError IO (Benchmark ZebraState)
createZebraBench = createBenchmark

createBenchmark :: Command -> EitherT BenchError IO (Benchmark a)
createBenchmark c = do
  let errNoSnapshotDate
        = fail "icicle-bench: need a snapshot date"
  let errNoChordFile
        = fail "icicle-bench: need a chord file"

  (mode, chords) <- case optFlagMode c of
    FlagSnapshot
      -> maybe errNoSnapshotDate (pure . (, Nothing) . Snapshot) (optSnapshot c)
    FlagChords
      -> maybe errNoChordFile (pure . (Chords, ) . Just) (optChords c)

  chordStart <- liftIO getCurrentTime

  when (isJust chords) $
    liftIO (putStrLn "icicle-bench: preparing chords")
  chordEnd <- liftIO getCurrentTime
  let chordSecs = realToFrac (chordEnd `diffUTCTime` chordStart) :: Double
  when (isJust chords) $
    liftIO (printf "icicle-bench: chord preparation time = %.2fs\n" chordSecs)

  start <- liftIO getCurrentTime

  (dictionary, format) <- case optInputFormat c of
    FlagInputPsv
      -> do (d, i) <- mkPsvInputConfig (optDictionary c) (optInputPsv c)
            let f   = FormatPsv
                    $ PsvConfig
                        (PsvInputConfig  mode i)
                        (PsvOutputConfig mode (optOutputPsv c) defaultOutputMissing)
                        defaultPsvMaxRowCount
                        defaultPsvInputBufferSize
                        defaultPsvOutputBufferSize
            return (d, f)
    FlagInputZebra
      -> do d <- firstEitherT BenchDictionaryImportError
               $ loadDictionary SC.optionSmallData ImplicitPrelude (optDictionary c)
            let f = FormatZebra mode (PsvOutputConfig mode (optOutputPsv c) defaultOutputMissing)
            return (d, f)

  (code, fleet) <- compileBench dictionary format chords
  end <- liftIO getCurrentTime

  return Benchmark {
      benchSource          = code
    , benchFleet           = fleet
    , benchInputPath       = optInput c
    , benchOutputPath      = optOutput c
    , benchChordPath       = chords
    , benchCompilationTime = end `diffUTCTime` start
    , benchFactsLimit      = optFactsLimit c
    , benchDropPath        = fromMaybe (optOutput c <> "-dropped.txt") (optDrop c)
    , benchUseDrop         = optUseDrop c
    }


mkPsvInputConfig :: FilePath -> FlagInputPsv -> EitherT BenchError IO (Dictionary, PsvInputFormat)
mkPsvInputConfig dictionaryPath x = firstEitherT BenchDictionaryImportError $ case x of
  FlagInputPsvDense
    -> do (dict, dense) <- loadDenseDictionary SC.optionSmallData ImplicitPrelude dictionaryPath Nothing
          return (dict, PsvInputDense dense (denseSelectedFeed dense))
  FlagInputPsvSparse
    -> do dict <- loadDictionary SC.optionSmallData ImplicitPrelude dictionaryPath
          return (dict, PsvInputSparse)


compileBench :: Dictionary -> IOFormat -> Maybe FilePath -> EitherT BenchError IO (Text, SeaFleet s)
compileBench dictionary format chords = do
  avalanche  <- hoistEither
              $ first BenchCompileError
              $ P.avalancheOfDictionary P.defaultCompileOptions dictionary

  let cfg = HasInput format
          $ InputOpts AllowDupTime (tombstonesOfDictionary dictionary)

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
