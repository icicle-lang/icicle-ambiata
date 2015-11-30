{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import           Data.Time (getCurrentTime, diffUTCTime)

import qualified Icicle.Avalanche.Prim.Flat as A
import qualified Icicle.Avalanche.Program as A
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Core.Program.Fusion (FusionError)
import qualified Icicle.Core.Program.Fusion as C
import qualified Icicle.Core.Program.Program as C
import           Icicle.Data
import           Icicle.Data.DateTime (dateOfText)
import           Icicle.Dictionary
import           Icicle.Internal.Pretty (pretty)
import           Icicle.Pipeline
import           Icicle.Sea.Chords.File (writeChordFile)
import           Icicle.Sea.Chords.Parse (ChordParseError(..), parseChordFile)
import           Icicle.Sea.Eval
import qualified Icicle.Source.Parser as S
import qualified Icicle.Source.Query as S
import           Icicle.Storage.Dictionary.Toml

import           P

import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import           System.Environment (getArgs)
import           System.FilePath (replaceExtension)
import           System.IO (IO, FilePath, putStrLn, print)
import           System.IO (IOMode(..), withFile, hFileSize)
import           System.IO.Temp (createTempDirectory)

import           Text.ParserCombinators.Parsec (SourcePos)
import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either
import           X.Control.Monad.Catch

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dict, inp, out, src, modestr] -> do
      let (mode, mchords) = modeOfString modestr
      xx <- runEitherT (runBench mode dict inp out src mchords)
      case xx of
        Left (BenchSeaError err) -> print (pretty err)
        Left err                 -> print err
        Right _                  -> return ()

    _ -> do
      putStrLn "usage: icicle-bench DICTIONARY INPUT_PSV OUTPUT_PSV OUTPUT_C SNAPSHOT_DATE"
      putStrLn "  -or- icicle-bench DICTIONARY INPUT_PSV OUTPUT_PSV OUTPUT_C CHORD_PSV"
      putStrLn ("invalid args: " <> show args)

modeOfString :: String -> (PsvMode, Maybe FilePath)
modeOfString str =
  case dateOfText (T.pack str) of
    Nothing   -> (PsvChords, Just str)
    Just date -> (PsvSnapshot date, Nothing)

------------------------------------------------------------------------

runBench
  :: PsvMode
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> Maybe FilePath
  -> EitherT BenchError IO ()

runBench mode dictionaryPath inputPath outputPath sourcePath chordPath =
  withChords chordPath $ \packedChordPath -> do
    liftIO (putStrLn "icicle-bench: starting compilation")
    compStart <- liftIO getCurrentTime

    dictionary <- firstEitherT BenchDictionaryImportError (loadDictionary dictionaryPath)
    avalanche  <- hoistEither (avalancheOfDictionary dictionary)

    let cfg = Psv (PsvConfig mode (tombstonesOfDictionary dictionary))

    let avalancheL = Map.toList avalanche

    code <- firstEitherT BenchSeaError (hoistEither (codeOfPrograms cfg avalancheL))
    liftIO (T.writeFile sourcePath code)

    asm <- firstEitherT BenchSeaError (assemblyOfPrograms cfg avalancheL)
    liftIO (T.writeFile (replaceExtension sourcePath ".s") asm)

    let acquireFleet = firstEitherT BenchSeaError (seaCompile cfg avalanche)
        releaseFleet = seaRelease

    bracketEitherT' acquireFleet releaseFleet $ \fleet -> do
      compEnd <- liftIO getCurrentTime
      let compSecs = realToFrac (compEnd `diffUTCTime` compStart) :: Double
      liftIO (printf "icicle-bench: compilation time = %.2fs\n" compSecs)
      liftIO (putStrLn "icicle-bench: starting snapshot")

      psvStart <- liftIO getCurrentTime
      stats    <- firstEitherT BenchSeaError (seaPsvSnapshotFilePath fleet inputPath outputPath packedChordPath)
      psvEnd   <- liftIO getCurrentTime

      size <- liftIO (withFile inputPath ReadMode hFileSize)

      let facts    = psvFactsRead    stats
          entities = psvEntitiesRead stats

      let psvSecs = realToFrac (psvEnd `diffUTCTime` psvStart) :: Double
          mbps    = (fromIntegral size / psvSecs) / (1024 * 1024)
          mfps    = (fromIntegral facts / psvSecs) / (1000 * 1000)

      liftIO (printf "icicle-bench: snapshot time   = %.2fs\n"                psvSecs)
      liftIO (printf "icicle-bench: total entities  = %d\n"                   entities)
      liftIO (printf "icicle-bench: total facts     = %d\n"                   facts)
      liftIO (printf "icicle-bench: fact throughput = %.2f million facts/s\n" mfps)
      liftIO (printf "icicle-bench: byte throughput = %.2f MB/s\n"            mbps)

------------------------------------------------------------------------

tombstonesOfDictionary :: Dictionary -> Map Attribute (Set Text)
tombstonesOfDictionary dict =
  let go (DictionaryEntry a (ConcreteDefinition _ ts)) = [(a, ts)]
      go _                                             = []
  in Map.fromList (concatMap go (dictionaryEntries dict))

withChords :: Maybe FilePath -> (Maybe FilePath -> EitherT BenchError IO a) -> EitherT BenchError IO a
withChords Nothing     io = io Nothing
withChords (Just path) io = do
  let acquire = liftIO (getTemporaryDirectory >>= \tmp -> createTempDirectory tmp "icicle-bench")
      release = liftIO . removeDirectoryRecursive
  bracketEitherT' acquire release $ \dir -> do

    liftIO (putStrLn "icicle-bench: preparing chords")
    chordStart <- liftIO getCurrentTime

    chordFile <- liftIO (TL.readFile path)
    chordMap  <- firstEitherT BenchChordParseError . hoistEither $ parseChordFile chordFile

    let packedChords = dir <> "/chords"
    liftIO (writeChordFile chordMap packedChords)

    chordEnd <- liftIO getCurrentTime
    let chordSecs = realToFrac (chordEnd `diffUTCTime` chordStart) :: Double
    liftIO (printf "icicle-bench: chord preparation time = %.2fs\n" chordSecs)

    io (Just packedChords)

------------------------------------------------------------------------

avalancheOfDictionary :: Dictionary -> Either BenchError (Map Attribute (A.Program (Annot ()) S.Variable A.Prim))
avalancheOfDictionary dict = do
  let virtuals = fmap (second unVirtual) (getVirtualFeatures dict)

  core      <- traverse (coreOfSource dict) virtuals
  fused     <- traverse fuseCore (Map.unionsWith (<>) core)
  avalanche <- traverse avalancheOfCore fused

  return avalanche

avalancheOfCore :: C.Program () S.Variable -> Either BenchError (A.Program (Annot ()) S.Variable A.Prim)
avalancheOfCore core = do
  flat    <- first BenchAvalancheError (coreFlatten core)
  checked <- first BenchAvalancheError (checkAvalanche flat)
  return checked

fuseCore :: [(S.Variable, C.Program () S.Variable)] -> Either BenchError (C.Program () S.Variable)
fuseCore =
  first BenchFusionError . C.fuseMultiple ()

coreOfSource
  :: Dictionary
  -> (Attribute, QueryTop'T SourceVar)
  -> Either BenchError (Map Attribute [(S.Variable, C.Program () S.Variable)])
coreOfSource dict (getAttribute -> attr, virtual) =
  first BenchSourceError $ do
    let inlined = sourceInline dict virtual

    desugared    <- sourceDesugarQT inlined
    (checked, _) <- sourceCheckQT dict desugared

    let reified = sourceReifyQT checked

    core <- sourceConvert dict reified
    let simplified = coreSimp core

    let baseattr  = (mkAttribute . unVar . unName) (S.feature virtual)

    pure (Map.singleton baseattr [(S.Variable attr, simplified)])

unVar :: S.Variable -> Text
unVar (S.Variable x) = x

unName :: Name a -> a
unName (Name x)      = x
unName (NameMod _ x) = unName x
