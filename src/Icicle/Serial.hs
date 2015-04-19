{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Serial (
    ParseError (..)
  , renderParseError
  , renderEavt
  , parseEavt
  , eavtParser
  ) where

import           Data.Attoparsec.Text
import           Data.Bifunctor
import           Data.Text as T

import           Icicle.Data

import           P

data ParseError =
  ParseError Text
  deriving (Eq, Show)

renderParseError :: ParseError -> Text
renderParseError (ParseError err) =
  "Could not parse EAVT text line: " <> err

renderEavt :: AsAt Fact' -> Text
renderEavt f =
            (getEntity . entity' . fact) f
  <> "|" <> (getAttribute . attribute' . fact) f
  <> "|" <> (value' . fact) f
  <> "|" <> (getDateTime . time) f

parseEavt :: Text -> Either ParseError (AsAt Fact')
parseEavt =
  first (ParseError . T.pack) . parseOnly eavtParser

eavtParser :: Parser (AsAt Fact')
eavtParser =
  AsAt
   <$> (Fact'
         <$> (Entity <$> column)
         <* pipe
         <*> (Attribute <$> column)
         <* pipe
         <*> column
         <* pipe)
   <*> (DateTime <$> column)

column :: Parser Text
column =
  takeTill (== '|')

pipe :: Parser Char
pipe =
  char '|'
