{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Icicle.Benchmark (
    BenchError(..)
  , Benchmark(..)
  , BenchmarkResult(..)
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
import           Data.Text (Text)
import qualified Data.Text.Lazy.IO as TL
import           Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)

import qualified Icicle.Avalanche.Prim.Flat as A
import qualified Icicle.Avalanche.Program as A
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
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
  , benchFleet           :: SeaFleet
  , benchInputPath       :: FilePath
  , benchOutputPath      :: FilePath
  , benchChordPath       :: Maybe FilePath
  , benchCompilationTime :: NominalDiffTime
  }

data BenchmarkResult = BenchmarkResult {
    benchTime     :: NominalDiffTime
  , benchEntities :: Int64
  , benchFacts    :: Int64
  , benchBytes    :: Int64
  } deriving (Eq, Ord, Show)

createBenchmark
  :: PsvMode
  -> FilePath
  -> FilePath
  -> FilePath
  -> Maybe FilePath
  -> EitherT BenchError IO Benchmark

createBenchmark mode dictionaryPath inputPath outputPath packedChordPath = do
  start <- liftIO getCurrentTime

  dictionary <- firstEitherT BenchDictionaryImportError (loadDictionary ImplicitPrelude dictionaryPath)
  avalanche  <- hoistEither (avalancheOfDictionary dictionary)

  let cfg = Psv (PsvInputConfig  mode (tombstonesOfDictionary dictionary) PsvInputSparse AllowDupTime)
                (PsvOutputConfig mode PsvOutputSparse)

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
    , benchChordPath       = packedChordPath
    , benchCompilationTime = end `diffUTCTime` start
    }

releaseBenchmark :: Benchmark -> EitherT BenchError IO ()
releaseBenchmark b =
  seaRelease (benchFleet b)

runBenchmark :: Benchmark -> EitherT BenchError IO BenchmarkResult
runBenchmark b = do
  let fleet      = benchFleet      b
      inputPath  = benchInputPath  b
      outputPath = benchOutputPath b
      chordPath  = benchChordPath  b

  start <- liftIO getCurrentTime
  stats <- firstEitherT BenchSeaError (seaPsvSnapshotFilePath fleet inputPath outputPath chordPath)
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
  let go (DictionaryEntry a (ConcreteDefinition _ ts)) = [(a, ts)]
      go _                                             = []
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
    (checked, _) <- sourceCheckQT dict desugared

    let reified = sourceReifyQT checked

    core <- sourceConvert dict reified
    let simplified = coreSimp core

    let baseattr  = (Attribute . unVar . unName) (S.feature virtual)

    pure (Map.singleton baseattr [(S.Variable attr, simplified)])

unVar :: S.Variable -> Text
unVar (S.Variable x) = x

unName :: Name a -> a
unName = go . nameBase
  where
   go (NameBase  x) = x
   go (NameMod _ x) = go x

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
