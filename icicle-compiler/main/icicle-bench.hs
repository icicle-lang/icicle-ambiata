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
  <$> (pDictionaryToml <|> pDictionaryCompiled)
  <*> (pInputSparse <|> pInputDense <|> pInputZebra)
  <*> (pOutputSparse <|> pOutputDense)
  <*> pOutputCode
  <*> (pSnapshot <|> pChord)
  <*> pLimit
  <*> pDrop
  <*> pFlagDrop

  where
   pDictionaryToml
     = fmap DictionaryToml
     $ strOption (long "dictionary-toml" <> metavar "DICTIONARY_TOML")
   pDictionaryCompiled
     = fmap DictionaryCode
     $ strOption (long "dictionary-code" <> metavar "DICTIONARY_C")
   pInputSparse
     = fmap InputSparsePsv
     $ strOption (long "input-sparse-psv" <> metavar "INPUT_PSV")
   pInputDense
     = fmap InputDensePsv
     $ strOption (long "input-dense-psv" <> metavar "INPUT_PSV")
   pInputZebra
     = fmap InputZebra
     $ strOption (long "input-zebra" <> metavar "INPUT_ZEBRA")
   pOutputSparse
     = fmap OutputSparsePsv
     $ strOption (long "output-sparse-psv" <> metavar "OUTPUT_PSV")
   pOutputDense
     = fmap OutputDensePsv
     $ strOption (long "output-dense-psv" <> metavar "OUTPUT_PSV")
   pOutputCode
     = optional $ strOption (long "output-code" <> metavar "DICTIONARY_C")
   pSnapshot
     = fmap ScopeSnapshot
     . flip option (long "snapshot" <> metavar "SNAPSHOT_DATE")
     $ readerAsk >>= \s -> case timeOfText (T.pack s) of
         Just t  -> return t
         Nothing -> readerError "cannot parse snapshot date"
   pChord
     = fmap ScopeChord
     $ strOption (long "chord" <> metavar "CHORD_DESCRIPTOR")
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
    InputFormatPsv
      -> bracketEitherT' (createPsvBench c) releaseBenchmark
       $ runBenchmark runPsvBench (optOutputCode c)
    InputFormatZebra
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
