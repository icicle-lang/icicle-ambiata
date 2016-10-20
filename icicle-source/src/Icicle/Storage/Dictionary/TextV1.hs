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
import           Icicle.Storage.Encoding

import           P hiding (concat, intercalate)

import           Data.Attoparsec.Text
import           Data.Text hiding (takeWhile)
import qualified Data.Set as Set

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
parseIcicleDictionaryV1
 = DictionaryEntry
 <$> (Attribute <$> field)
 <*   p
 <*> (ConcreteDefinition <$> parseEncoding <*> pure (Set.singleton "NA") <*> pure unkeyed)
 -- No namespace in this legacy dictionary
 <*> pure (Namespace "default")
    where
      p = char '|'

parseDictionaryLineV1 :: Text -> Either ParseError DictionaryEntry
parseDictionaryLineV1 s =
  first (ParseError . pack) $ parseOnly parseIcicleDictionaryV1 s

writeDictionaryLineV1 :: DictionaryEntry -> Text
writeDictionaryLineV1 (DictionaryEntry (Attribute a) (ConcreteDefinition e _ _) _)
  = a <> "|" <> prettyConcrete e
writeDictionaryLineV1 (DictionaryEntry _ (VirtualDefinition _) _)
  = "Virtual features not supported in V1"
