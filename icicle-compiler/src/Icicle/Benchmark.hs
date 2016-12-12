{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Icicle.Benchmark (
    Command (..)
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
import           Icicle.Sea.Chords.File (writeChordFile)
import           Icicle.Sea.Chords.Parse (ChordParseError(..), parseChordFile)

import qualified Icicle.Source.Checker as SC

import           Icicle.Storage.Dictionary.Toml

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Map as Map
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import qualified Data.Text.Lazy.IO as TL

import           System.IO.Temp (createTempDirectory)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
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
  | BenchChordParseError       ChordParseError
  deriving (Show)

data Benchmark a = Benchmark {
    benchSource          :: Text
  , benchAssembly        :: Text
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
  , optChords       :: Either Time FilePath
  , optFactsLimit   :: Int
  , optDrop         :: Maybe FilePath
  , optUseDrop      :: FlagUseDrop
  , optInputFormat  :: FlagInput
  , optInputPsv     :: FlagInputPsv
  , optOutputPsv    :: FlagOutputPsv
  }

data FlagInput = FlagInputPsv | FlagInputZebra
data FlagInputPsv = FlagInputPsvSparse | FlagInputPsvDense
type FlagOutputPsv = PsvOutputFormat


createPsvBench :: Command -> EitherT BenchError IO (Benchmark PsvState)
createPsvBench = createBenchmark

createZebraBench :: Command -> EitherT BenchError IO (Benchmark ZebraState)
createZebraBench = createBenchmark

createBenchmark :: Command -> EitherT BenchError IO (Benchmark a)
createBenchmark c = do
  let mode = either Snapshot (const Chords) (optChords c)
  let chords = either (const Nothing) Just (optChords c)

  chordStart <- liftIO getCurrentTime
  withChords chords $ \packedChords -> do

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

    (code, asm, fleet) <- compileBench dictionary format
    end <- liftIO getCurrentTime

    return Benchmark {
        benchSource          = code
      , benchAssembly        = asm
      , benchFleet           = fleet
      , benchInputPath       = optInput c
      , benchOutputPath      = optOutput c
      , benchChordPath       = packedChords
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


compileBench :: Dictionary -> IOFormat -> EitherT BenchError IO (Text, Text, SeaFleet s)
compileBench dictionary format = do
  avalanche  <- hoistEither
              $ first BenchCompileError
              $ P.avalancheOfDictionary P.defaultCompileOptions dictionary

  let cfg = HasInput format
          $ InputOpts AllowDupTime (tombstonesOfDictionary dictionary)

  let avalancheL = Map.toList avalanche

  code  <- firstEitherT BenchSeaError (hoistEither (codeOfPrograms cfg avalancheL))
  asm   <- firstEitherT BenchSeaError (assemblyOfPrograms cfg avalancheL)
  fleet <- firstEitherT BenchSeaError (seaCompile NoCacheSea cfg avalanche)

  return (code, asm, fleet)

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
