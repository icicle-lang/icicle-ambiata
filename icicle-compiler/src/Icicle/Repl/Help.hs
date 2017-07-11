{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Help (
    showHelp

  , putAllHelp
  , putHelp
  , flagHelp
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List

import           Icicle.Repl.Data
import           Icicle.Repl.Flag
import           Icicle.Repl.Monad

import           P

import qualified System.IO as IO


flagHelp :: FlagInfo -> Help
flagHelp x =
  Help (":set +/-" <> flagName x) (flagDescription x)

putHelp :: Int -> Help -> Repl ()
putHelp n x =
  let
    m =
      length (helpCommand x)
  in
    liftIO . IO.putStrLn $
         "    "
      <> helpCommand x
      <> List.replicate (n - m) ' '
      <> "    "
      <> helpDescription x

putAllHelp :: [Help] -> Repl ()
putAllHelp xs =
  let
    n =
      List.maximum $ fmap (length . helpCommand) xs
  in
    mapM_ (putHelp n) xs

showHelp :: Repl ()
showHelp = do
  liftIO $ IO.putStrLn ""
  liftIO $ IO.putStrLn "  Commands available:"
  liftIO $ IO.putStrLn ""

  putAllHelp [
      Help ":help or :h"
        "Shows this message."

    , Help ":quit or :q"
        "Quits the REPL."

    , Help ":load <path>"
        "Loads a dictionary, a data file or additional icicle functions."

    , Help ":dictionary"
        "Displays the current dictionary, including the types or inputs, outputs and functions."

    , Help ":let <function>"
        "Define a new icicle function. For example, :let inc x = x + 1"

    , Help "<query>"
        "Execute an icicle query. For example, feature price ~> mean value"
    ]

  liftIO $ IO.putStrLn ""
  liftIO $ IO.putStrLn "  Commands for changing settings:"
  liftIO $ IO.putStrLn ""

  putAllHelp $ [
      Help ":set snapshot <date>"
        "Activate snapshot mode and set the date to use."

    , Help ":set max-map-size <int>"
        "Set the maximum map size."
    ] <>
    fmap flagHelp allFlags

  liftIO $ IO.putStrLn ""
