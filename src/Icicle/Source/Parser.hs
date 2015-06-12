{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Parser (
    parseQueryTop
  , prettyParse
  ) where

import Icicle.Source.Lexer.Lexer
import Icicle.Source.Lexer.Token
import Icicle.Source.Parser.Parser

import Icicle.Source.Query

import Icicle.Internal.Pretty

import Data.Text

import Text.Parsec

import P


parseQueryTop :: Text -> Either ParseError (QueryTop SourcePos Variable)
parseQueryTop inp
 = let toks = lexer "" inp
   in  runParser top () "" toks

prettyParse :: Text -> [Char]
prettyParse inp
 = case parseQueryTop inp of
    Left e -> "Error: " <> show e
    Right r -> show (pretty r)

