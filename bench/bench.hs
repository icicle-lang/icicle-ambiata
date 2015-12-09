{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (liftIO)

import           Criterion.Main

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromJust)
import           Data.String (String)

import qualified Icicle.Benchmark as I
import           Icicle.Benchmark.Generator (FileSpec(..), AttributeType(..), generateSparse)
import           Icicle.Data.Time (timeOfText)
import qualified Icicle.Sea.Eval as I

import           P

import           System.Directory (copyFile)
import           System.Exit (ExitCode(..))
import           System.FilePath (FilePath, (</>))
import           System.IO (IO, putStrLn)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (system)

import           X.Control.Monad.Trans.Either
import           X.Control.Monad.Catch

------------------------------------------------------------------------

main :: IO ()
main =
  withSystemTempDirectory "icicle-bench-" $ \base -> do
    putStrLn "Generating repos"
    generateRepo repoSpec base

    putStrLn "Compiling"
    _ <- runEitherT . bracketEitherT' (createBenchmark base) I.releaseBenchmark $ \b -> do
      liftIO $ putStrLn "Running benchmarks"
      liftIO $ defaultMain
        [ bgroup "entities=10"
          [ group "years=2" b
          ]
        ]

    return ()

------------------------------------------------------------------------

group :: String -> I.Benchmark -> Benchmark
group name b = do
  bgroup name
    [ bench "icicle" $ nfIO (icicle b)
    , bench "cat"    $ nfIO (cat (I.benchInputPath b))
    , bench "wc -l"  $ nfIO (wcl (I.benchInputPath b))
    , bench "wc"     $ nfIO (wc  (I.benchInputPath b))
    ]

cat :: FilePath -> IO ()
cat path = do
  ExitSuccess <- system ("cat " <> path <> " >/dev/null")
  return ()

wc :: FilePath -> IO ()
wc path = do
  ExitSuccess <- system ("wc " <> path)
  return ()

wcl :: FilePath -> IO ()
wcl path = do
  ExitSuccess <- system ("wc -l " <> path)
  return ()

icicle :: I.Benchmark -> IO ()
icicle b = do
  Right stats <- runEitherT (I.runBenchmark b)
  putStrLn ("  " <> show stats)

------------------------------------------------------------------------

generateRepo :: FileSpec -> FilePath -> IO ()
generateRepo spec path = do
  putStrLn ("  " <> show spec)
  generateSparse spec (path </> "data.psv")
  T.writeFile (path </> "dictionary.toml") dictionary
  copyFile "data/libs/prelude.icicle" (path </> "prelude.icicle")

createBenchmark :: FilePath -> EitherT I.BenchError IO I.Benchmark
createBenchmark path =
  I.createBenchmark mode (path </> "dictionary.toml")
                         (path </> "data.psv")
                         (path </> "out.psv")
                         Nothing

mode :: I.PsvMode
mode = I.PsvSnapshot (fromJust (timeOfText "2015-10-01"))

repoSpec :: FileSpec
repoSpec = FileSpec {
    specSeed         = 12345678
  , specEntityCount  = 10
  , specAttributes   = Map.fromList [("wibble", Int)]
  , specStart        = 2014
  , specEnd          = 2015
  }

dictionary :: Text
dictionary = T.unlines
  [ "title     = \"bench\""
  , "version   = 1"
  , "namespace = \"bench\""
  , "import    = [ \"prelude.icicle\" ]"
  , "chapter   = [ ]"
  , "tombstone = \"NA\""
  , ""
  , "[fact.wibble]"
  , "  encoding=\"int\""
  , ""
  , "[feature.mean_wibble]"
  , "  expression=\"feature wibble ~> mean value\""
  ]
