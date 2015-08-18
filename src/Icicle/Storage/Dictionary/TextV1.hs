{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase       #-}
module Icicle.Storage.Dictionary.TextV1 (
    parseDictionaryLineV1
  , writeDictionaryLineV1
  ) where

import           Icicle.Data
import           Icicle.Dictionary.Data
import           Icicle.Serial (ParseError (..))
import           P hiding (concat, intercalate)

import           Data.Attoparsec.Text

import           Data.Either.Combinators
import           Data.Text hiding (takeWhile)

import           Icicle.Storage.Encoding

field :: Parser Text
field = append <$> takeWhile (not . isDelimOrEscape) <*> (concat <$> many (cons <$> escaped <*> field)) <?> "field"
  where
    escaped :: Parser Char
    escaped = repEscape =<< (char '\\' >> satisfy (inClass "|rn\\")) <?> "Escaped char"
    isDelimOrEscape c = c == '\\' || c == '|'
    repEscape '|'  = pure '|'
    repEscape 'n'  = pure '\n'
    repEscape 'r'  = pure '\r'
    repEscape '\\' = pure '\\'
    repEscape _    = mempty -- Unreachable

parseIcicleDictionaryV1 :: Parser DictionaryEntry
parseIcicleDictionaryV1 = do
  DictionaryEntry <$> (Attribute <$> field) <* p <*> (ConcreteDefinition <$> parseEncoding)
    where
      p = char '|'

parseDictionaryLineV1 :: Text -> Either ParseError DictionaryEntry
parseDictionaryLineV1 s =
  mapLeft (ParseError . pack) $ parseOnly parseIcicleDictionaryV1 s

writeDictionaryLineV1 :: DictionaryEntry -> Text
writeDictionaryLineV1 (DictionaryEntry (Attribute a) (ConcreteDefinition e)) =
  a <> "|" <> prettyConcrete e

writeDictionaryLineV1 (DictionaryEntry _ (VirtualDefinition _)) = "Virtual features not supported in V1"
