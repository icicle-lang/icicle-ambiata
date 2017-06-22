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
import Icicle.Source.Parser.Token

import Icicle.Source.Query

import Icicle.Common.Base

import Icicle.Data.Name

import Icicle.Internal.Pretty

import Data.Text

import Text.Parsec

import P

parseFunctions :: SourceName -> Text -> Either ParseError [((SourcePos, Name Variable), (Function SourcePos Variable))]
parseFunctions source inp
 = let toks = lexer source inp
   in  runParser (consumeAll functions) () source toks

parseQueryTop :: OutputId -> Text -> Either ParseError (QueryTop SourcePos Variable)
parseQueryTop name inp
 = let toks = lexer "" inp
   in  runParser (consumeAll $ top name) () "" toks

consumeAll :: Parser a -> Parser a
consumeAll f = do
 r <- f
 eof
 return r

prettyParse :: OutputId -> Text -> [Char]
prettyParse name inp
 = case parseQueryTop name inp of
    Left e -> "Error: " <> show e
    Right r -> show (pretty r)

