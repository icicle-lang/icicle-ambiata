{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
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

import           Data.List (dropWhileEnd)
import           Data.String (String)

import           Icicle.Repl.Completion
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

getHaskelineSettings :: IO (Haskeline.Settings (StateT State IO))
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
      Just input
        | let trimmed = dropWhileEnd (== ' ') input
        , trimmed == ":quit" || trimmed == ":q"
        -> pure ()
        | otherwise
        -> do onInput input
              withUserInput onInput
