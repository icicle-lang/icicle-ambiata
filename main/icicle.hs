{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import           Control.Monad.Trans.Either

import           Data.Text as T
import           Data.Text.IO as T

import           Icicle

import           P

import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import           X.Options.Applicative

main :: IO ()
main = getArgs >>= \args -> case args of
  ["-h"] ->
    usage
  ["--help"] ->
    usage
  [factset] ->
    orDie renderParseError $ run factset
  _ ->
    usage >> exitFailure


run :: FilePath -> EitherT ParseError IO ()
run p =
  EitherT $ (mapM parseEavt . T.lines <$> T.readFile p) >>= mapM print


usage :: IO ()
usage = T.putStrLn . T.unlines $ [
    "icicle FACTSET"
  , ""
  , "  FACTSET     A path to a factset in textual EAVT format, currently"
  , "              it is expected that this factset would abide by the"
  , "              predifined 'demographics' dictionary."
  ]
