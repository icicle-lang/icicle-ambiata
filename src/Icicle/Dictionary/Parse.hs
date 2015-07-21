{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary.Parse (
    parseDictionaryLineV1
  ) where

import           Icicle.Data
import           Icicle.Dictionary.Data
import           Icicle.Serial (ParseError (..))
import           P hiding (concat)

import           Data.Attoparsec.Text

import           Data.Either.Combinators
import           Data.Text hiding (takeWhile)

field :: Parser Text
field = append <$> takeWhile (not . isDelimOrEscape) <*> (concat <$> many (cons <$> escaped <*> field)) <?> "field"
  where
    escaped :: Parser Char
    escaped = repEscape <$> (char '\\' >> satisfy (inClass "|rn\\")) <?> "Escaped char"
    isDelimOrEscape c = c == '\\' || c == '|'
    repEscape '|' = '|'
    repEscape 'n' = '\n'
    repEscape 'r' = '\r'
    repEscape '\\' = '\\'
    repEscape a = repEscape a -- _|_

parseIcicleDictionaryV1 :: Parser DictionaryEntry
parseIcicleDictionaryV1 = do
  DictionaryEntry <$> (Attribute <$> field) <* p <*> (ConcreteDefinition <$> encoding)
    where
      p = char '|'
      encoding :: Parser Encoding
      encoding = StringEncoding  <$ string "string"
             <|> IntEncoding     <$ string "int"
             <|> IntEncoding     <$ string "long" -- Todo, change this once Longs are a thing
             <|> DoubleEncoding  <$ string "double"
             <|> DateEncoding    <$ string "date"
             <|> BooleanEncoding <$ string "boolean"
             <|> ListEncoding    <$ char '[' <*> encoding <* char ']'
             <|> StructEncoding  <$ char '(' <*> (structField `sepBy` char ',') <* char ')'
      structField = do
        n <- takeWhile (/= ':')
        _ <- char ':'
        e <- encoding
        o <- Optional <$ char '*' <|> pure Mandatory
        pure $ StructField o (Attribute n) e

parseDictionaryLineV1 :: Text -> Either ParseError DictionaryEntry
parseDictionaryLineV1 s =
  mapLeft (ParseError . pack) $ parseOnly parseIcicleDictionaryV1 s
