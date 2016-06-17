{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (liftIO)

import           Criterion.Main
import           Criterion.Types (Config(..))

import           Data.Char (toLower)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Icicle.Benchmark as I
import           Icicle.Benchmark.Generator (AttributeType(..), Frequency(..))
import           Icicle.Benchmark.Generator (Generator(..), generateSparse)
import           Icicle.Data.Time (timeOfText)
import qualified Icicle.Sea.Eval as I

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.Exit (ExitCode(..))
import           System.FilePath (FilePath, (</>))
import           System.IO (IO, hPutStrLn, stderr)
import           System.IO.Temp (withSystemTempDirectory)
import           System.Process (system)

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

main :: IO ()
main =
  withSystemTempDirectory "icicle-bench-" $ \base -> do
    hPutStrLn stderr "Generating repos"
    repos <- generateRepos base
      -- frequency / duration (yrs) / entities
      [ (PerMinute,  2,      10)
      , (PerHour,    5,     200)
      , (PerDay,    10,    1000)
      , (PerDay,    10,    2000)
      , (PerDay,    10,    3000)
      , (PerMonth,   1, 1000000)
      ]

    hPutStrLn stderr "Compiling"
    _ <- runEitherT . bracketEitherT' (createBenchmarks repos) releaseBenchmarks $ \bs -> do
      liftIO $ hPutStrLn stderr "Running benchmarks"
      liftIO $ defaultMainWith benchConfig (fmap group bs)

    return ()

benchConfig :: Config
benchConfig =
  defaultConfig {
      reportFile = Just "dist/build/icicle-bench.html"
    , csvFile    = Just "dist/build/icicle-bench.csv"
    }

------------------------------------------------------------------------

group :: (String, I.Benchmark) -> Benchmark
group (name, b) = do
  bgroup name
    [ bench "icicle" $ nfIO (icicle b)
    , bench "cat"    $ nfIO (cat (I.benchInputPath b))
    , bench "wc -l"  $ nfIO (wcl (I.benchInputPath b))
    , bench "wc"     $ nfIO (wc  (I.benchInputPath b))
    ]

icicle :: I.Benchmark -> IO ()
icicle b = do
  Right stats <- runEitherT (I.runBenchmark b)
  hPutStrLn stderr ("  " <> show stats)

cat :: FilePath -> IO ()
cat path = do
  ExitSuccess <- system ("cat " <> path <> " >/dev/null")
  return ()

wcl :: FilePath -> IO ()
wcl path = do
  ExitSuccess <- system ("LANG=C LC_ALL=C wc -l " <> path <> " >/dev/null")
  return ()

wc :: FilePath -> IO ()
wc path = do
  ExitSuccess <- system ("LANG=C LC_ALL=C wc " <> path <> " >/dev/null")
  return ()

------------------------------------------------------------------------

type Years    = Int
type Entities = Int32

generateRepos :: FilePath -> [(Frequency, Years, Entities)] -> IO [(String, FilePath)]
generateRepos root = traverse (generateRepo root)

generateRepo :: FilePath -> (Frequency, Years, Entities) -> IO (String, FilePath)
generateRepo root (freq, ys, es) = do
  let generator = Generator
        { genSeed        = 12345678
        , genEntityCount = es
        , genAttributes  = Map.fromList [("wibble", Int)]
        , genStart       = 2015 - (ys - 1)
        , genEnd         = 2015
        , genFrequency   = freq }

      name = "frequency=" <> fmap toLower (drop 3 (show freq))
         </> "duration="  <> show ys <> "yrs"
         </> "entities="  <> show es

      path = root </> name

  hPutStrLn stderr ("  " <> show generator)

  createDirectoryIfMissing True path
  generateSparse generator (path </> "data.psv")

  T.writeFile (path </> "dictionary.toml") dictionary

  return (name, path)

createBenchmarks :: [(String, FilePath)] -> EitherT I.BenchError IO [(String, I.Benchmark)]
createBenchmarks = traverse createBenchmark

createBenchmark :: (String, FilePath) -> EitherT I.BenchError IO (String, I.Benchmark)
createBenchmark (name, path) = do
  let dict   = (path </> "dictionary.toml")
      input  = (path </> "data.psv")
      output = (path </> "out.psv")
  b <- I.createBenchmark mode dict input output Nothing
  return (name, b)

releaseBenchmarks :: [(String, I.Benchmark)] -> EitherT I.BenchError IO ()
releaseBenchmarks = traverse_ I.releaseBenchmark . fmap snd

mode :: I.PsvMode
mode = I.PsvSnapshot (fromJust (timeOfText "2015-10-01"))

dictionary :: Text
dictionary = T.unlines
  [ "title     = \"bench\""
  , "version   = 1"
  , "namespace = \"bench\""
  , "import    = []"
  , "chapter   = []"
  , "tombstone = \"NA\""
  , ""
  , "[fact.wibble]"
  , "  encoding=\"int\""
  , ""
  , "[feature.mean_wibble]"
  , "  expression=\"feature wibble ~> mean value\""
  ]
