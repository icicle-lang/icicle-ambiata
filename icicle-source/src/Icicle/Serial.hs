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
import           Data.Text as T

import           Icicle.Data
import           Icicle.Data.Time
import           Icicle.Dictionary
import           Icicle.Encoding
import           Icicle.Storage.Encoding

import qualified Icicle.Internal.Pretty as PP

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

instance PP.Pretty ParseError where
 pretty d = PP.text $ T.unpack $ renderParseError d

renderEavt :: AsAt Fact' -> Text
renderEavt f =
            (getEntity . factEntity' . atFact) f
  <> "|" <> (takeAttributeName . factAttribute' . atFact) f
  <> "|" <> (factValue' . atFact) f
  <> "|" <> (renderTime . atTime) f

parseEavt :: Text -> Either ParseError (AsAt Fact')
parseEavt =
  first (ParseError . T.pack) . parseOnly eavtParser

eavtParser :: Parser (AsAt Fact')
eavtParser =
  AsAt
   <$> (Fact'
         <$> (Entity <$> column)
         <* pipe
         <*> (toAttributeName =<< column)
         <* pipe
         <*> column
         <* pipe)
   <*> pTime

column :: Parser Text
column =
  takeTill (== '|')

pipe :: Parser Char
pipe =
  char '|'

decodeEavt :: Dictionary -> Text -> Either ParseError (AsAt Fact)
decodeEavt dict t
 = do e  <- parseEavt t
      f' <- first DecodeError
          $ parseFact dict
          $ atFact e
      return e { atFact = f' }

