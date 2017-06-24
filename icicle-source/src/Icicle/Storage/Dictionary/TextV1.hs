{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import qualified Data.Text as Text
import qualified Data.Set as Set

field :: Parser Text
field = Text.append <$> takeWhile (not . isDelimOrEscape) <*> (Text.concat <$> many (Text.cons <$> escaped <*> field)) <?> "field"
  where
    escaped :: Parser Char
    escaped = repEscape =<< (char '\\' >> satisfy (inClass "|rn\\")) <?> "Escaped char"
    isDelimOrEscape c = c == '\\' || c == '|'
    repEscape '|'  = pure '|'
    repEscape 'n'  = pure '\n'
    repEscape 'r'  = pure '\r'
    repEscape '\\' = pure '\\'
    repEscape _    = mempty -- Unreachable

parseIcicleDictionaryV1 :: Parser DictionaryInput
parseIcicleDictionaryV1
 = DictionaryInput
 <$> (InputId <$> pure [namespace|default|] <*> inputNameParser)
 <*   char '|'
 <*> parseEncoding
 <*> pure (Set.singleton "NA")
 <*> pure unkeyed

inputNameParser :: Parser InputName
inputNameParser = do
  x <- field
  case parseInputName x of
    Nothing ->
      fail $ "Invalid input name: " <> Text.unpack x
    Just a ->
      pure a

parseDictionaryLineV1 :: Text -> Either ParseError DictionaryInput
parseDictionaryLineV1 s =
  first (ParseError . Text.pack) $ parseOnly parseIcicleDictionaryV1 s

writeDictionaryLineV1 :: DictionaryInput -> Text
writeDictionaryLineV1 (DictionaryInput (InputId _ a) e _ _)
  = renderInputName a <> "|" <> prettyConcrete e
