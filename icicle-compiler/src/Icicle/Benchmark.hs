{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Icicle.Benchmark (
    BenchError(..)
  , Benchmark(..)
  , BenchmarkResult(..)
  , BenchInputPsv (..)
  , BenchOutputPsv (..)
  , createBenchmark
  , releaseBenchmark
  , runBenchmark
  , withChords
  ) where

import           Icicle.Dictionary

import qualified Icicle.Compiler.Source as P
import qualified Icicle.Compiler        as P

import           Icicle.Sea.Chords.File (writeChordFile)
import           Icicle.Sea.Chords.Parse (ChordParseError(..), parseChordFile)
import           Icicle.Sea.Eval

import qualified Icicle.Source.Checker as SC

import           Icicle.Storage.Dictionary.Toml

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as TL
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

import           System.FilePath (FilePath)
import           System.IO (IO, IOMode(..), withFile, hFileSize)
import           System.IO.Temp (createTempDirectory)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)

import           P

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

data BenchError =
    BenchDictionaryImportError DictionaryImportError
  | BenchCompileError          (P.ErrorCompile P.Var)
  | BenchSeaError              SeaError
  | BenchChordParseError       ChordParseError
  deriving (Show)

------------------------------------------------------------------------

data Benchmark = Benchmark {
    benchSource          :: Text
  , benchAssembly        :: Text
  , benchFleet           :: SeaFleet PsvState
  , benchInputPath       :: FilePath
  , benchOutputPath      :: FilePath
  , benchDropPath        :: FilePath
  , benchChordPath       :: Maybe FilePath
  , benchCompilationTime :: NominalDiffTime
  , benchFactsLimit      :: Int
  , benchLimitDiscard    :: PsvDrop
  }

data BenchmarkResult = BenchmarkResult {
    benchTime     :: NominalDiffTime
  , benchEntities :: Int64
  , benchFacts    :: Int64
  , benchBytes    :: Int64
  } deriving (Eq, Ord, Show)

data BenchInputPsv  = BenchInputSparse  | BenchInputDense
data BenchOutputPsv = BenchOutputSparse | BenchOutputDense

createBenchmark
  :: PsvMode
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> Maybe FilePath
  -> Maybe Int
  -> Maybe PsvDrop
  -> Maybe BenchInputPsv
  -> Maybe BenchOutputPsv
  -> EitherT BenchError IO Benchmark

createBenchmark mode dictionaryPath inputPath outputPath dropPath packedChordPath limit discard input output = do
  start <- liftIO getCurrentTime

  (dictionary, input') <- firstEitherT BenchDictionaryImportError $ inputCfg input
  let output'           = outputCfg output

  avalanche  <- hoistEither
              $ first BenchCompileError
              $ P.avalancheOfDictionary P.defaultCompileOptions dictionary

  let cfg = HasInput
          ( FormatPsv
          $ PsvConfig (PsvInputConfig  mode input')
                      (PsvOutputConfig mode output' defaultOutputMissing)
                      defaultPsvMaxRowCount
                      defaultPsvInputBufferSize
                      defaultPsvOutputBufferSize )
          ( InputOpts AllowDupTime
                     (tombstonesOfDictionary dictionary))

  let avalancheL = Map.toList avalanche

  code  <- firstEitherT BenchSeaError (hoistEither (codeOfPrograms cfg avalancheL))
  asm   <- firstEitherT BenchSeaError (assemblyOfPrograms cfg avalancheL)
  fleet <- firstEitherT BenchSeaError (seaCompile NoCacheSea cfg avalanche)

  end <- liftIO getCurrentTime

  return Benchmark {
      benchSource          = code
    , benchAssembly        = asm
    , benchFleet           = fleet
    , benchInputPath       = inputPath
    , benchOutputPath      = outputPath
    , benchDropPath        = dropPath
    , benchChordPath       = packedChordPath
    , benchCompilationTime = end `diffUTCTime` start
    , benchFactsLimit      = fromMaybe (1024*1024) limit
    , benchLimitDiscard    = fromMaybe PsvHasDropFile discard
    }
  where
    inputCfg x = case x of
      Just BenchInputDense -> do
        (dict, dense) <- loadDenseDictionary SC.optionSmallData ImplicitPrelude dictionaryPath Nothing
        return (dict, PsvInputDense dense (denseSelectedFeed dense))
      Just BenchInputSparse -> do
        dict <- loadDictionary SC.optionSmallData ImplicitPrelude dictionaryPath
        return (dict, PsvInputSparse)
      Nothing -> inputCfg $ Just BenchInputSparse

    outputCfg x = case x of
      Just BenchOutputDense  -> PsvOutputDense
      Just BenchOutputSparse -> PsvOutputSparse
      Nothing                -> outputCfg $ Just BenchOutputSparse

releaseBenchmark :: Benchmark -> EitherT BenchError IO ()
releaseBenchmark b =
  seaRelease (benchFleet b)

runBenchmark :: Benchmark -> EitherT BenchError IO BenchmarkResult
runBenchmark b = do
  let fleet      = benchFleet        b
      inputPath  = benchInputPath    b
      outputPath = benchOutputPath   b
      dropPath   = benchDropPath     b
      chordPath  = benchChordPath    b
      limit      = benchFactsLimit   b
      discard    = benchLimitDiscard b

  start <- liftIO getCurrentTime
  stats <- firstEitherT BenchSeaError
         $ seaPsvSnapshotFilePath fleet inputPath outputPath dropPath chordPath limit discard
  end   <- liftIO getCurrentTime

  size <- liftIO (withFile inputPath ReadMode hFileSize)

  return BenchmarkResult {
      benchTime     = end `diffUTCTime` start
    , benchFacts    = psvFactsRead    stats
    , benchEntities = psvEntitiesRead stats
    , benchBytes    = fromIntegral size
    }

------------------------------------------------------------------------

withChords :: Maybe FilePath -> (Maybe FilePath -> EitherT BenchError IO a) -> EitherT BenchError IO a
withChords Nothing     io = io Nothing
withChords (Just path) io = do
  let acquire = liftIO (getTemporaryDirectory >>= \tmp -> createTempDirectory tmp "icicle-chords-")
      release = liftIO . removeDirectoryRecursive
  bracketEitherT' acquire release $ \dir -> do

    chordFile <- liftIO (TL.readFile path)
    chordMap  <- firstEitherT BenchChordParseError . hoistEither $ parseChordFile chordFile

    let packedChords = dir <> "/chords"
    liftIO (writeChordFile chordMap packedChords)

    io (Just packedChords)
