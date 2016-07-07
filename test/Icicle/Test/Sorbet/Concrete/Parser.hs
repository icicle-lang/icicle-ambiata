{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Icicle.Test.Sorbet.Concrete.Parser where

import           Icicle.Sorbet.Concrete.Parser
import           Icicle.Sorbet.Concrete.Pretty
import           Icicle.Sorbet.Lexical.Layout
import           Icicle.Sorbet.Lexical.Lexer
import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           Icicle.Internal.Leijen (Doc, renderPlain)
import           Icicle.Test.Arbitrary.Run
import           Icicle.Test.Sorbet.Concrete.Jack

import           Data.String (String)
import qualified Data.Text as T

import           Disorder.Jack (Property)
import           Disorder.Jack (gamble, counterexample, property, failed)

import           P hiding (Alt)

import           System.IO (IO)

import qualified Text.Megaparsec as Mega
import           Text.Show.Pretty (ppShow)


prop_repl_whitespace :: Property
prop_repl_whitespace =
  gamble jRepl $
    law_parser_roundtrip (ppRepl Spaces) pRepl

prop_repl_braces :: Property
prop_repl_braces =
  gamble jRepl $
    law_parser_roundtrip (ppRepl Braces) pRepl

prop_extra_whitespace :: Property
prop_extra_whitespace =
  gamble jExtra $
    law_parser_roundtrip (ppExtra Spaces) pExtra

prop_extra_braces :: Property
prop_extra_braces =
  gamble jExtra $
    law_parser_roundtrip (ppExtra Braces) pExtra

law_parser_roundtrip ::
  forall f.
  Eq (f X) =>
  Show (f X) =>
  Functor f =>
  (f X -> Doc a) ->
  Mega.Parsec ParserError [Positioned Token] (f Position) ->
  f X ->
  Property
law_parser_roundtrip pp p ast0 =
  let
    code :: Text
    code =
      renderPlain $ pp ast0

    lex :: Text -> Either String [Positioned Token]
    lex =
      first Mega.parseErrorPretty .
      Mega.parse lexProgram "qc"

    layout :: [Positioned Token] -> Either String [Positioned Token]
    layout =
      first show .
      layoutProgram

    parse :: [Positioned Token] -> Either String (f Position)
    parse =
      first Mega.parseErrorPretty .
      Mega.parse p "qc"

    showTokens :: [Positioned Token] -> String
    showTokens = \case
      [] ->
        "<no tokens>"
      xs ->
        intercalate "\n" $ fmap show xs
  in
    counterexample "" .
    counterexample "=== Pretty ===" .
    counterexample (T.unpack code) .
    counterexample "" .
    counterexample "=== Original ===" .
    counterexample (ppShow ast0) .
    counterexample "" $
    case lex code of
      Left err ->
        counterexample "=== Lexing Failed ===" .
        counterexample err $
        property failed
      Right toks ->
        counterexample "=== Lexer Tokens ===" .
        counterexample (showTokens toks) .
        counterexample "" $
        case layout toks of
          Left err ->
            counterexample "=== Layout Failed ===" .
            counterexample err $
            property failed
          Right ltoks ->
            counterexample "=== Layout Tokens ===" .
            counterexample (showTokens ltoks) .
            counterexample "" $
            case fmap (X <$) (parse ltoks) of
              Left err ->
                counterexample "=== Parsing Failed ===" .
                counterexample err $
                property failed
              Right ast ->
                counterexample "=== Roundtrip ===" .
                counterexample (ppShow ast) $
                  ast0 == ast

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore (checkArgsSized 100)
