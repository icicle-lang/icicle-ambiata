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

import           System.IO (IO, FilePath, putStrLn, print)

import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either
import           X.Options.Applicative


pCommand :: Parser Command
pCommand = Command
  <$> (InputDictionary <$> pDictionary <|> InputCode <$> pInputCode)
  <*> pInput
  <*> pOutput
  <*> pOutputCode
  <*> pMode
  <*> pDate
  <*> pChords
  <*> pLimit
  <*> pDrop
  <*> pFlagDrop
  <*> pInputFormat
  <*> pInputPsv
  <*> pOutputPsv

  where
   pDictionary
     = strOption $ long "dictionary"
   pInput
     = strOption $ long "input"
   pInputCode
     = strOption $ long "input-code"
   pOutput
     = strOption $ long "output"
   pOutputCode
     = optional . strOption $ long "output-code"
   pMode
     = flip option (long "mode" <> value FlagSnapshot)
     $ readerAsk >>= \case
         "snapshot" -> return FlagSnapshot
         "chord" -> return FlagChords
         _ -> readerError "snapshot or chord"
   pDate
     = optional . flip option (long "snapshot-date")
     $ readerAsk >>= \s -> case timeOfText (T.pack s) of
         Just t  -> return t
         Nothing -> readerError "cannot parse snapshot date"
   pChords
     = optional . strOption $ long "chords"
   pLimit
     = flip option (long "facts-limit" <> value defaultFactsLimit)
     $ readerAsk >>= \s -> case readMaybe s of
         Just i  -> return i
         Nothing -> readerError "--facts-limit NUMBER"
   pDrop
     = optional $ strOption $ long "drop"
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
     = flip option (long "input-psv" <> value FlagInputPsvSparse <> help "dense or sparse")
     $ readerAsk >>= \case
          "dense" -> return FlagInputPsvDense
          "sparse" -> return FlagInputPsvSparse
          _ -> readerError "--input-psv <sparse or dense"
   pOutputPsv
     = flip option (long "output-psv" <> value PsvOutputSparse <> help "dense or sparse")
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
runCommand c = do
  case optInputFormat c of
    FlagInputPsv
      -> bracketEitherT' (createPsvBench c) releaseBenchmark
       $ runBenchmark runPsvBench (optOutputCode c)
    FlagInputZebra
      -> bracketEitherT' (createZebraBench c) releaseBenchmark
       $ runBenchmark runZebraBench (optOutputCode c)

runBenchmark ::
      MonadIO m
  => (Benchmark a -> m BenchmarkResult)
  -> Maybe FilePath
  -> Benchmark a
  -> m ()
runBenchmark f msrc bench = do
  liftIO (putStrLn "icicle-bench: starting compilation")

  let compSecs = realToFrac (benchCompilationTime bench) :: Double
  liftIO (printf "icicle-bench: compilation time = %.2fs\n" compSecs)

  liftIO (maybe (return ()) (flip T.writeFile (benchSource bench)) msrc)

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
