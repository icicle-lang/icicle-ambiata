{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Monad (
    Repl(..)
  , runRepl
  , withUserInput

  , getPrompt
  , getHaskelineSettings
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State (MonadState(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Data.List as List
import           Data.String (String)

import           Icicle.Repl.Data
import           Icicle.Repl.Pretty

import           P

import           System.Console.Haskeline (InputT, MonadException)
import qualified System.Console.Haskeline as Haskeline
import qualified System.Directory as Directory
import           System.IO (IO)


newtype Repl a =
  Repl {
      unRepl :: InputT (StateT State IO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadException)

instance MonadState State Repl where
  get =
    Repl $ lift get
  put x =
    Repl . lift $ put x
  state f =
    Repl . lift $ state f

prependReplacement :: String -> Haskeline.Completion -> Haskeline.Completion
prependReplacement prefix x =
  x { Haskeline.replacement = prefix <> Haskeline.replacement x }

completion :: MonadIO m => Haskeline.CompletionFunc m
completion (prefix0, suffix) =
  let
    prefix =
      List.reverse prefix0

    load =
      ":load "

    loadPrefix0 =
      List.reverse $
        List.drop (List.length load) prefix
  in
    if load `List.isPrefixOf` prefix then
      fmap (fmap (fmap (prependReplacement load))) $
        Haskeline.completeFilename (loadPrefix0, suffix)
    else
      Haskeline.noCompletion (prefix0, suffix)

getHaskelineSettings :: MonadIO m => IO (Haskeline.Settings m)
getHaskelineSettings = do
  home <- Directory.getHomeDirectory
  return .
    Haskeline.setComplete completion $
    Haskeline.defaultSettings {
        Haskeline.historyFile =
          Just $ home <> "/.icicle-repl.history"
      , Haskeline.autoAddHistory =
          True
      }

runRepl :: State -> Repl a -> IO a
runRepl initial x = do
  settings <- getHaskelineSettings
  flip evalStateT initial .
    Haskeline.runInputT settings .
    Haskeline.withInterrupt $
    unRepl x

getPrompt :: Repl String
getPrompt = do
  use <- getUseColor
  pure $ sgrColor use Dull Yellow "λ "

withUserInput :: (String -> Repl ()) -> Repl ()
withUserInput onInput =
  Haskeline.handleInterrupt (withUserInput onInput) $ do
    prompt <- getPrompt
    minput <- Repl $ Haskeline.getInputLine prompt
    case minput of
      Nothing ->
        pure ()
      Just ":quit" ->
        pure ()
      Just ":q" ->
        pure ()
      Just input -> do
        onInput input
        withUserInput onInput
