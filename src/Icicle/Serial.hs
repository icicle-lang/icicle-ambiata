{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Serial (
    ParseError (..)
  , renderParseError
  , renderEavt
  , parseEavt
  , eavtParser
  , decodeEavt
  ) where

import           Data.Attoparsec.Text
import           Data.Bifunctor
import           Data.Either.Combinators
import           Data.Text as T

import           Icicle.Data
import           Icicle.Dictionary
import           Icicle.Encoding

import           P

data ParseError =
   ParseError Text
 | DecodeError DecodeError
  deriving (Eq, Show)

renderParseError :: ParseError -> Text
renderParseError (ParseError err) =
  "Could not parse EAVT text line: " <> err
renderParseError (DecodeError err) =
  "Could not decode EAVT according to dictionary: " <> renderDecodeError err

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

decodeEavt :: Dictionary -> Text -> Either ParseError (AsAt Fact)
decodeEavt dict t
 = do e  <- parseEavt t
      f' <- mapLeft DecodeError
          $ parseFact dict
          $ fact e
      return e { fact = f' }

