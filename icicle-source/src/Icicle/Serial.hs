{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE PatternGuards     #-}

module Icicle.Serial (
    SerialError (..)
  , renderSerialError
  , renderEavt
  , parseEavt
  , eavtParser
  , decodeEavt
  , decodeEavt'
  , readFacts
  , readFacts'
  , readNormalisedFacts
  ) where

import           Data.Attoparsec.Text
import           Data.Bifunctor
import           Data.Text as T

import           Icicle.Data
import           Icicle.Data.Time
import           Icicle.Dictionary
import           Icicle.Encoding

import qualified Icicle.Internal.Pretty as PP

import           P


data SerialError
 = SerialErrorParse  Text
 | SerialErrorDecode DecodeError
  deriving (Eq, Show)

renderSerialError :: SerialError -> Text
renderSerialError (SerialErrorParse err) =
  "Could not parse EAVT text line: " <> err
renderSerialError (SerialErrorDecode err) =
  "Could not decode EAVT according to dictionary: " <> renderDecodeError err

instance PP.Pretty SerialError where
 pretty d = PP.text $ T.unpack $ renderSerialError d


renderEavt :: AsAt (Fact Text) -> Text
renderEavt f =
            (getEntity    . factEntity    . atFact) f
  <> "|" <> (getAttribute . factAttribute . atFact) f
  <> "|" <> (factValue  . atFact) f
  <> "|" <> (renderTime . atTime) f

parseEavt :: Text -> Either SerialError (AsAt (Fact Text))
parseEavt =
  first (SerialErrorParse . T.pack) . parseOnly eavtParser

eavtParser :: Parser (AsAt (Fact Text))
eavtParser =
  AsAt
   <$> (Fact
         <$> (Entity <$> column)
         <* pipe
         <*> (Attribute <$> column)
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

decodeEavt :: Dictionary -> Text -> Either SerialError (AsAt (Fact Value, FactMode))
decodeEavt dict t
 = do e  <- parseEavt t
      f' <- first SerialErrorDecode
          $ parseFact dict
          $ atFact e
      return e { atFact = f' }

decodeEavt' :: Dictionary -> Text -> Either SerialError (AsAt (Fact Value))
decodeEavt' d s = fmap fst <$> decodeEavt d s

readFacts :: Dictionary -> Text -> Either SerialError [AsAt (Fact Value, FactMode)]
readFacts dict raw
  = traverse (decodeEavt dict) $ T.lines raw

readFacts' :: Dictionary -> Text -> Either SerialError [AsAt (Fact Value)]
readFacts' d s = fmap (fmap fst) <$> readFacts d s

-- | Read and normalise all facts. All raw input must be provided at once to normalise
--   facts correctly.
--
readNormalisedFacts :: Time -> Dictionary -> Text -> Either SerialError [AsAt NormalisedFact]
readNormalisedFacts now dict raw
  = readFacts dict raw >>= first SerialErrorDecode . normaliseFacts now
