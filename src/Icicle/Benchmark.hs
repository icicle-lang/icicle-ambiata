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

import           Control.Monad.IO.Class (liftIO)
import           Control.Parallel.Strategies (withStrategy, parTraversable, rparWith, rseq)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Text.Lazy.IO as TL
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

import qualified Icicle.Avalanche.Prim.Flat as A
import qualified Icicle.Avalanche.Program as A
import           Icicle.Common.Annot (Annot)
import           Icicle.Core.Program.Fusion (FusionError)
import qualified Icicle.Core.Program.Fusion as C
import qualified Icicle.Core.Program.Program as C
import           Icicle.Data
import           Icicle.Dictionary
import           Icicle.Pipeline
import           Icicle.Sea.Chords.File (writeChordFile)
import           Icicle.Sea.Chords.Parse (ChordParseError(..), parseChordFile)
import           Icicle.Sea.Eval
import qualified Icicle.Source.Parser as S
import qualified Icicle.Source.Query as S
import qualified Icicle.Source.Checker as SC
import           Icicle.Storage.Dictionary.Toml

import           P

import           System.FilePath (FilePath)
import           System.IO (IO, IOMode(..), withFile, hFileSize)
import           System.IO.Temp (createTempDirectory)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)

import           Text.ParserCombinators.Parsec (SourcePos)

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

data BenchError =
    BenchDictionaryImportError DictionaryImportError
  | BenchSourceError     (CompileError SourcePos S.Variable ())
  | BenchFusionError     (FusionError S.Variable)
  | BenchAvalancheError  (CompileError () S.Variable A.Prim)
  | BenchSeaError        SeaError
  | BenchChordParseError ChordParseError
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
  , benchLimitDiscard    :: SeaFlagDiscard
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
  -> Maybe SeaFlagDiscard
  -> Maybe BenchInputPsv
  -> Maybe BenchOutputPsv
  -> EitherT BenchError IO Benchmark

createBenchmark mode dictionaryPath inputPath outputPath dropPath packedChordPath limit discard input output = do
  start <- liftIO getCurrentTime

  (dictionary, input') <- firstEitherT BenchDictionaryImportError $ inputCfg input
  let output'           = outputCfg output

  avalanche  <- hoistEither (avalancheOfDictionary dictionary)

  let cfg = HasInput
          ( FormatPsv (PsvInputConfig  mode input')
                      (PsvOutputConfig mode output'))
          ( InputOpts AllowDupTime
                     (tombstonesOfDictionary dictionary))

  let avalancheL = Map.toList avalanche

  code  <- firstEitherT BenchSeaError (hoistEither (codeOfPrograms cfg avalancheL))
  asm   <- firstEitherT BenchSeaError (assemblyOfPrograms cfg avalancheL)
  fleet <- firstEitherT BenchSeaError (seaCompile cfg avalanche)

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
    , benchLimitDiscard    = fromMaybe SeaWriteOverLimit discard
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
      Just BenchOutputDense  -> PsvOutputDense defaultMissingValue
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

tombstonesOfDictionary :: Dictionary -> Map Attribute (Set Text)
tombstonesOfDictionary dict =
  let go (DictionaryEntry a (ConcreteDefinition _ ts) _) = [(a, ts)]
      go _                                               = []
  in Map.fromList (concatMap go (dictionaryEntries dict))

------------------------------------------------------------------------

avalancheOfDictionary :: Dictionary -> Either BenchError (Map Attribute (A.Program (Annot ()) S.Variable A.Prim))
avalancheOfDictionary dict = do
  let virtuals = fmap (second unVirtual) (getVirtualFeatures dict)

  core      <- parTraverse (coreOfSource dict) virtuals
  fused     <- parTraverse fuseCore (Map.unionsWith (<>) core)
  avalanche <- parTraverse avalancheOfCore fused

  return avalanche

avalancheOfCore :: C.Program () S.Variable -> Either BenchError (A.Program (Annot ()) S.Variable A.Prim)
avalancheOfCore core = do
  flat    <- first BenchAvalancheError (coreFlatten core)
  checked <- first BenchAvalancheError (checkAvalanche flat)
  return checked

fuseCore :: [(S.Variable, C.Program () S.Variable)] -> Either BenchError (C.Program () S.Variable)
fuseCore programs =
  first BenchFusionError $ do
    fused <- C.fuseMultiple () programs
    pure (coreSimp fused)

coreOfSource
  :: Dictionary
  -> (Attribute, QueryTop'T SourceVar)
  -> Either BenchError (Map Attribute [(S.Variable, C.Program () S.Variable)])
coreOfSource dict (Attribute attr, virtual) =
  first BenchSourceError $ do
    let inlined = sourceInline dict virtual

    desugared    <- sourceDesugarQT inlined
    (checked, _) <- sourceCheckQT SC.optionSmallData dict desugared

    let reified = sourceReifyQT checked

    core <- sourceConvert dict reified
    let simplified = coreSimp core

    let baseattr  = (Attribute . unVar . unName) (S.feature virtual)

    pure (Map.singleton baseattr [(S.Variable attr, simplified)])

parTraverse  :: Traversable t => (a -> Either e b) -> t a -> Either e (t b)
parTraverse f = sequenceA . parallel . fmap f
 where
  parallel = withStrategy (parTraversable (rparWith rseq))

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
