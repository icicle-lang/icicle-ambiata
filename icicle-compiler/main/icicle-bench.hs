{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Icicle.Benchmark
import           Icicle.Data.Time (timeOfText)
import           Icicle.Internal.Pretty (pretty)
import           Icicle.Sea.Eval

import           P

import           System.FilePath (replaceExtension)
import           System.IO (IO, FilePath, putStrLn, print)

import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either
import           X.Options.Applicative



pCommand :: Parser Command
pCommand = Command
  <$> pDictionary
  <*> pInput
  <*> pOutput
  <*> pC
  <*> pChords
  <*> pLimit
  <*> pDrop
  <*> pFlagDrop
  <*> pInputFormat
  <*> pInputPsv
  <*> pOutputPsv

  where
   pDictionary
     = argument str $ metavar "DICTIONARY"
   pInput
     = argument str $ metavar "INPUT_PATH"
   pOutput
     = argument str $ metavar "OUTPUT_PATH"
   pC
     = argument str $ metavar "C"
   pChords
     = flip argument (metavar "SNAPSHOT_DATE_OR_CHORD_PATH")
     $ readerAsk >>= \s -> return $ case timeOfText (T.pack s) of
         Just t  -> Left t
         Nothing -> Right s
   pLimit
     = flip option (long "facts-limit" <> value defaultFactsLimit)
     $ readerAsk >>= \s -> case readMaybe s of
         Just i  -> return i
         Nothing -> readerError "--facts-limit NUMBER"
   pDrop
     = strOption $ long "drop"
   pFlagDrop
     = flag FlagUseDropFile FlagNoUseDropFile
     $ long "drop-to-output" <> help "write partial results to dropped-X.txt or normal output"
   pInputFormat
     = flip option (long "input-format" <> help "psv or zebra" <> value FlagInputPsv)
     $ readerAsk >>= \case
          "psv" -> return FlagInputPsv
          "zebra" -> return FlagInputZebra
          _ -> readerError "--input-format <psv or zebra>"
   pInputPsv
     = flip option (long "input-psv" <> help "dense or sparse")
     $ readerAsk >>= \case
          "dense" -> return FlagInputPsvDense
          "sparse" -> return FlagInputPsvSparse
          _ -> readerError "--input-psv <sparse or dense"
   pOutputPsv
     = flip option (long "output-psv" <> help "dense or sparse")
     $ readerAsk >>= \case
          "dense" -> return PsvOutputDense
          "sparse" -> return PsvOutputSparse
          _ -> readerError "--output-psv <sparse or dense"

   defaultFactsLimit = 1024*1024

------------------------------------------------------------------------

main :: IO ()
main = dispatch pCommand >>= go
  where
    go cmd = do
     putStrLn $ "icicle-bench: facts_limit = " <> show (optFactsLimit cmd)
     xx <- runEitherT (runCommand cmd)
     case xx of
       Left (BenchSeaError err) -> print (pretty err)
       Left err                 -> print err
       Right _                  -> return ()

runCommand :: Command -> EitherT BenchError IO ()
runCommand c = case optInputFormat c of
  FlagInputPsv
    -> bracketEitherT' (createPsvBench c) releaseBenchmark
     $ runBenchmark runPsvBench (optC c)
  FlagInputZebra
    -> bracketEitherT' (createZebraBench c) releaseBenchmark
    $ runBenchmark runZebraBench (optC c)

runBenchmark :: MonadIO m => (Benchmark a -> m BenchmarkResult) -> FilePath -> Benchmark a -> m ()
runBenchmark f sourcePath bench = do
  liftIO (putStrLn "icicle-bench: starting compilation")

  let compSecs = realToFrac (benchCompilationTime bench) :: Double
  liftIO (printf "icicle-bench: compilation time = %.2fs\n" compSecs)

  liftIO (T.writeFile sourcePath (benchSource bench))
  liftIO (T.writeFile (replaceExtension sourcePath ".s") (benchAssembly bench))

  liftIO (putStrLn "icicle-bench: starting snapshot")

  stats <- f bench

  let entities = benchEntities stats
      facts    = benchFacts stats
      bytes    = benchBytes stats
      secs     = realToFrac (benchTime stats) :: Double
      mbps     = (fromIntegral bytes / secs) / (1024 * 1024)
      mfps     = (fromIntegral facts / secs) / (1000 * 1000)

  liftIO (printf "icicle-bench: snapshot time   = %.2fs\n"                secs)
  liftIO (printf "icicle-bench: total entities  = %d\n"                   entities)
  liftIO (printf "icicle-bench: total facts     = %d\n"                   facts)
  liftIO (printf "icicle-bench: fact throughput = %.2f million facts/s\n" mfps)
  liftIO (printf "icicle-bench: byte throughput = %.2f MB/s\n"            mbps)
