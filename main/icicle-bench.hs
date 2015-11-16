{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.IO.Class (liftIO)

import           Data.Time (getCurrentTime, diffUTCTime)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.IO as T

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
import           Icicle.Internal.Pretty (pretty)
import           Icicle.Sea.Eval
import qualified Icicle.Source.Parser as S
import qualified Icicle.Source.Query as S
import           Icicle.Storage.Dictionary.Toml

import           Jetski

import           P

import           System.Environment (getArgs)
import           System.IO (IO, FilePath, putStrLn, print)
import           System.IO (IOMode(..), withFile, hFileSize)

import           Text.ParserCombinators.Parsec (SourcePos)
import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either
import           X.Control.Monad.Catch

------------------------------------------------------------------------

data BenchError =
    BenchDictionaryImportError DictionaryImportError
  | BenchSourceError    (CompileError SourcePos S.Variable ())
  | BenchFusionError    (FusionError S.Variable)
  | BenchAvalancheError (CompileError () S.Variable A.Prim)
  | BenchSeaError       SeaError
  deriving (Show)

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dict, inp, out, src] -> do
      xx <- runEitherT (runBench dict inp out src)
      case xx of
        Left (BenchSeaError err) -> print (pretty err)
        Left err                 -> print err
        Right _                  -> return ()

    _ -> do
      putStrLn "usage: icicle-bench DICTIONARY INPUT_PSV OUTPUT_PSV OUTPUT_C"
      putStrLn ("invalid args: " <> show args)

------------------------------------------------------------------------

runBench :: FilePath -> FilePath -> FilePath -> FilePath -> EitherT BenchError IO ()
runBench dictionaryPath inputPath outputPath sourcePath = do
  liftIO (putStrLn "icicle-bench: starting compilation")
  compStart <- liftIO getCurrentTime

  dictionary <- firstEitherT BenchDictionaryImportError (loadDictionary dictionaryPath)
  avalanche  <- hoistEither (avalancheOfDictionary dictionary)

  let acquireFleet = firstEitherT BenchSeaError (seaCompile Psv avalanche)
      releaseFleet = seaRelease

  bracketEitherT' acquireFleet releaseFleet $ \fleet -> do
    compEnd <- liftIO getCurrentTime
    let compSecs = realToFrac (compEnd `diffUTCTime` compStart) :: Double
    liftIO (printf "icicle-bench: compilation time = %.2fs\n" compSecs)

    let src = libSource (sfLibrary fleet)
    liftIO (T.writeFile sourcePath src)

    liftIO (putStrLn "icicle-bench: starting snapshot")

    psvStart <- liftIO getCurrentTime
    firstEitherT BenchSeaError (seaPsvSnapshotFilePath fleet inputPath outputPath)
    psvEnd   <- liftIO getCurrentTime

    size <- liftIO (withFile inputPath ReadMode hFileSize)

    let psvSecs = realToFrac (psvEnd `diffUTCTime` psvStart) :: Double
        mbps    = (fromIntegral size / psvSecs) / (1024 * 1024)
    liftIO (printf "icicle-bench: snapshot time = %.2fs (%.2fMB/s)\n" psvSecs mbps)

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
unName (Name x)      = x
unName (NameMod _ x) = unName x
