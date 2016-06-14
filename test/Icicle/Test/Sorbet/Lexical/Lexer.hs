{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sorbet.Lexical.Lexer where

import           Icicle.Sorbet.Lexical.Lexer
import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Test.Arbitrary.Run
import           Icicle.Test.Sorbet.Lexical.Jack

import qualified Data.Text as T

import           Disorder.Jack (gamble, counterexample, listOf)

import           P

import           System.IO (IO)

import qualified Text.Megaparsec as Mega
import           Text.Show.Pretty (ppShow)


prop_lexer_roundtrip =
  gamble (listOf jToken) $ \tokens0 ->
    let
      code =
        T.unwords $ fmap renderToken tokens0

      tokens =
        fmap (fmap posTail) $
        Mega.parse lexProgram "qc.icicle" code
    in
      counterexample "" .
      counterexample "=== Original ===" .
      counterexample (ppShow tokens0) .
      counterexample "" .
      counterexample "=== Roundtrip ===" .
      counterexample (either Mega.parseErrorPretty ppShow tokens) $
        Right tokens0 == tokens

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore (checkArgsSized 100)
