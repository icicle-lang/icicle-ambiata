{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.String (String)
import qualified Data.Text as T

import           Icicle.Benchmark
import           Icicle.Data.Time (timeOfText)
import           Icicle.Internal.Pretty (pretty)
import           Icicle.Sea.Eval

import           P

import           System.Environment (getArgs)
import           System.IO (IO, FilePath, putStrLn, print)

import           X.Control.Monad.Trans.Either

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
  case timeOfText (T.pack str) of
    Nothing   -> (PsvChords, Just str)
    Just time -> (PsvSnapshot time, Nothing)
