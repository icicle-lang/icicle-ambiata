{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Parser (
    parseQueryTop
  , parseFunctions
  , prettyParse
  , ParseError
  , SourcePos
  , Variable    (..)
  ) where

import Icicle.Source.Lexer.Lexer
import Icicle.Source.Lexer.Token
import Icicle.Source.Parser.Parser

import Icicle.Source.Query

import Icicle.Common.Base

import Icicle.Internal.Pretty

import Data.Text

import Text.Parsec

import P

parseFunctions :: Text -> Either ParseError [((SourcePos, Name Variable), (Function SourcePos Variable))]
parseFunctions inp
 = let toks = lexer "" inp
   in  runParser functions () "" toks

parseQueryTop :: Text -> Either ParseError (QueryTop SourcePos Variable)
parseQueryTop inp
 = let toks = lexer "" inp
   in  runParser top () "" toks

prettyParse :: Text -> [Char]
prettyParse inp
 = case parseQueryTop inp of
    Left e -> "Error: " <> show e
    Right r -> show (pretty r)

