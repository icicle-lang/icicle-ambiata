{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.IO.Class (liftIO)

import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime, diffUTCTime)

import           Icicle.Benchmark
import           Icicle.Data.Time (timeOfText)
import           Icicle.Internal.Pretty (pretty)
import           Icicle.Sea.Eval

import           P

import           System.FilePath (replaceExtension)
import           System.Environment (getArgs)
import           System.IO (IO, FilePath, putStrLn, print)

import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    argDict : argIn : argOut : argC : argDate : xs
      -> case xs of
           [a, argDrop, b]
             | argLimit   <- readMaybe a
             , argDiscard <- readDiscard b
             , isJust argLimit && isJust argDiscard
             -> go argDict argIn argOut argC argDate argLimit argDrop argDiscard
             | otherwise
             -> usage args
           _ -> go argDict argIn argOut argC argDate Nothing (argOut <> "_dropped.txt") Nothing
    _ -> usage args
  where
    usage as = do
      putStrLn "usage: icicle-bench DICTIONARY INPUT_PSV OUTPUT_PSV OUTPUT_C SNAPSHOT_DATE [FACTS_LIMIT] [DROP_TXT] [DISCARD]"
      putStrLn "  -or- icicle-bench DICTIONARY INPUT_PSV OUTPUT_PSV OUTPUT_C CHORD_PSV [FACTS_LIMIT] [DROP_TXT] [DISCARD]"
      putStrLn ("invalid args: " <> show as)

    readDiscard x
      | x == "--discard-over-limit" = Just SeaDiscardOverLimit
      | x == "--keep-over-limit"    = Just SeaWriteOverLimit
      | otherwise                   = Nothing

    go dict inp out src modestr limit dr discard = do
     putStrLn $ "icicle-bench: facts_limit = " <> show limit
     let (mode, mchords) = modeOfString modestr
     xx <- runEitherT (runBench mode dict inp out dr src mchords limit discard)
     case xx of
       Left (BenchSeaError err) -> print (pretty err)
       Left err                 -> print err
       Right _                  -> return ()

modeOfString :: String -> (PsvMode, Maybe FilePath)
modeOfString str =
  case timeOfText (T.pack str) of
    Nothing   -> (PsvChords, Just str)
    Just time -> (PsvSnapshot time, Nothing)

------------------------------------------------------------------------

runBench
  :: PsvMode
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> Maybe FilePath
  -> Maybe Int
  -> Maybe SeaFlagDiscard
  -> EitherT BenchError IO ()

runBench mode dictionaryPath inputPath outputPath dropPath sourcePath chordPath limit discard = do
  chordStart <- liftIO getCurrentTime
  when (isJust chordPath) $
    liftIO (putStrLn "icicle-bench: preparing chords")

  withChords chordPath $ \packedChordPath -> do

    chordEnd <- liftIO getCurrentTime
    let chordSecs = realToFrac (chordEnd `diffUTCTime` chordStart) :: Double
    when (isJust chordPath) $
      liftIO (printf "icicle-bench: chord preparation time = %.2fs\n" chordSecs)

    let create = createBenchmark mode
                                 dictionaryPath
                                 inputPath
                                 outputPath
                                 dropPath
                                 packedChordPath
                                 limit discard

    liftIO (putStrLn "icicle-bench: starting compilation")
    bracketEitherT' create releaseBenchmark $ \bench -> do
      let compSecs = realToFrac (benchCompilationTime bench) :: Double
      liftIO (printf "icicle-bench: compilation time = %.2fs\n" compSecs)

      liftIO (T.writeFile sourcePath (benchSource bench))
      liftIO (T.writeFile (replaceExtension sourcePath ".s") (benchAssembly bench))

      liftIO (putStrLn "icicle-bench: starting snapshot")
      stats <- runBenchmark bench

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
