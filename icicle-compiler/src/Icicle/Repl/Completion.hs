{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
module Icicle.Repl.Completion (
    completion
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State (MonadState)
import qualified Control.Monad.State as State

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.String (String)

import           Icicle.Internal.Pretty
import           Icicle.Common.Base
import           Icicle.Dictionary.Data
import           Icicle.Data
import           Icicle.Repl.Data
import           Icicle.Repl.Flag
import qualified Icicle.Source.ToCore.Context as STC
import           Icicle.Source.Lexer.Token (keywords, Variable)
import           Icicle.Source.Lexer.Lexer (lexerString)
import           Icicle.Source.Parser.Token (pUnresolvedInputId)
import           P
import           Text.Parsec (runParser)

import qualified System.Console.Haskeline as Haskeline

-- Tab completion system for the icicle repl.
--
-- This is a similar take as what GHC does,
-- and while we're using Parsec it's probably
-- a good enough implementation.
--
-- When we switch to MegaParsec, it'll be
-- plausible to use the ParseError's expected
-- record to better determine which tokens we
-- should suggest.

-- A association list of starting words to
-- completion functions.
completionCommands :: (MonadState State m, MonadIO m) => [(String, Haskeline.CompletionFunc m)]
completionCommands =
  [ (":load", Haskeline.completeFilename)
  , (":set",  completeFlags)
  , (":dictionary", Haskeline.noCompletion)
  , (":help", Haskeline.noCompletion)
  , (":quit", Haskeline.noCompletion)
  , (":let", completeLetExpression)
  , ("feature", completeFeatureExpression)
  ]

-- | Completer for the icicle REPL
completion :: (MonadState State m, MonadIO m) => Haskeline.CompletionFunc m
completion (prefix0, suffix) =
  let
    ccs = completionCommands

    (prefix, rest) =
      List.break Char.isSpace (List.reverse prefix0)

    -- If we haven't completed the first word yet:
    -- offer the keys of the assoc list; otherwise,
    -- the action it points to.
    action =
      if null rest
        then wrapCompleter " " $ \w ->
          let candidates = fmap fst ccs
          in  return $ filter (w `isPrefixOf`) candidates
        else fromMaybe Haskeline.noCompletion $ List.lookup prefix ccs

    in
      action (prefix0, suffix)

-- Completer for the `set` command.
-- Looks up all the flags and allows one to turn
-- them on or off.
completeFlags :: MonadIO m => Haskeline.CompletionFunc m
completeFlags =
  wrapPure " " $
    let candidates = fmap flagName allFlags
        onOff = fmap ('+':) candidates <> fmap ('-':) candidates
    in  onOff

-- Completing an expression after "feature"
completeFeatureExpression :: (MonadState State m, MonadIO m) => Haskeline.CompletionFunc m
completeFeatureExpression (prefix0, suffix) = do
  -- We need to know the the dictionary which
  -- is loaded in order to complete the fact
  -- and variables they define.
  dict     <- State.gets stateDictionary
  let
    -- The tells us if the user has "completed" the
    -- previous word. It only works for spaces,
    -- but that's ok, as we only use if for the
    -- fact name and the following `~>`, something
    -- better with the lexer could be used here I
    -- would imagine.
    new      = if head prefix0 == Just ' ' then 1 else 0
    -- Lex the input based on the user input.
    -- This is probably overkill
    lexed    = lexerString "repl" (List.reverse prefix0)
    -- The number of words we're up to.
    words    = length lexed + new
  case words of
    -- If we're on the second word, then we need to
    -- complete the input fact name (the stream to
    -- query over). Look it up from the dictionary.
    2 -> let i = Map.keys $ dictionaryInputs dict
             candidates = fmap (Text.unpack . renderInputName . inputName) i
         in  wrapPure " " candidates (prefix0, suffix)
    -- If we've written that, then we need a flows
    -- into before the query can begin.
    3 -> wrapPure " " ["~>"] (prefix0, suffix)
    -- Now we're inside the query, and the second
    -- item lexed is the fact name. So we will look
    -- up the fact in the dictionary so we can then
    -- offer the struct fields or `value` to complete
    -- with. We also gather `time` here.
    _inQuery
      | _ : tok : _ <- lexed
      , Right unresolvedInputId <- runParser pUnresolvedInputId () "" [tok]
      , let concretes  = STC.featuresConcretes $ featureMapOfDictionary dict
      , Just concrete <- lookupInputId unresolvedInputId concretes
      -> let context = STC.featureConcreteContext concrete
             names = Map.keys $ STC.featureContextVariables context
             time  = STC.featureContextFactTime context
             -- Call completeExpression to actually do the
             -- work, including the new names we're providing.
         in  completeExpression (time : names) (prefix0, suffix)
    -- If we couldn't find the fact, the query
    -- won't actually compile, but we can still complete
    -- keywords, as the user can go back and correct it.
    _otherwise ->
      completeExpression [] (prefix0, suffix)

completeLetExpression
 :: (MonadState State m, MonadIO m)
 => Haskeline.CompletionFunc m
completeLetExpression = completeExpression []

-- Completing an icicle expression.
completeExpression
 :: (MonadState State m, MonadIO m)
 => [Name Variable] -> Haskeline.CompletionFunc m
completeExpression extraNames =
  -- Completing expression components.
  -- We can break on all sorts of break chars here,
  -- as, e.g., `value+value` is legal icicle syntax.
  wrapCompleter word_break_chars $ \w -> do
    features <- State.gets (featureMapOfDictionary . stateDictionary)
    let
      env  = STC.featuresFunctions features
      now  = maybeToList $ STC.featureNow features
      dfun = fmap (show . pretty . nameBase) $ Map.keys env <> now <> extraNames
      kfun = fmap (Text.unpack . fst) $ keywords
      candidates = dfun <> kfun

    return $ filter (w `isPrefixOf`) candidates

wrapPure :: MonadIO m => String -> [String] -> Haskeline.CompletionFunc m
wrapPure breaks candidates =
  wrapCompleter breaks (\w ->
    return $ filter (w `isPrefixOf`) candidates)

-- From GHC
wrapCompleter :: MonadIO m => String -> (String -> m [String]) -> Haskeline.CompletionFunc m
wrapCompleter breakChars fun = Haskeline.completeWord Nothing breakChars
    $ fmap (fmap Haskeline.simpleCompletion . ordNub) . fun

word_break_chars :: String
word_break_chars = spaces <> specials <> symbols

symbols, specials, spaces :: String
symbols = "*+/<=>^|-"
specials = "(),.[]`"
spaces = " "
