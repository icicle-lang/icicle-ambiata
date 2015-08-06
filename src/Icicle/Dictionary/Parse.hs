{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase       #-}
module Icicle.Dictionary.Parse (
    parseDictionaryLineV1
  , writeDictionaryLineV1
  , parsePrimitiveEncoding
  , parseEncoding
  , prettyConcrete
  ) where

import           Icicle.Data
import           Icicle.Dictionary.Data
import           Icicle.Serial (ParseError (..))
import           P hiding (concat, intercalate)

import           Data.Attoparsec.Text

import           Data.Either.Combinators
import           Data.Text hiding (takeWhile)

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

parsePrimitiveEncoding :: Parser Encoding
parsePrimitiveEncoding =
           StringEncoding  <$ string "string"
       <|> IntEncoding     <$ string "int"
       <|> IntEncoding     <$ string "long" -- Todo, change this once Longs are a thing
       <|> DoubleEncoding  <$ string "double"
       <|> DateEncoding    <$ string "date"
       <|> BooleanEncoding <$ string "boolean"

parseEncoding :: Parser Encoding
parseEncoding = parsePrimitiveEncoding
       <|> ListEncoding    <$ char '[' <*> parseEncoding <* char ']'
       <|> StructEncoding  <$ char '(' <*> (structField `sepBy` char ',') <* char ')'
  where
    structField = do
      n <- takeWhile (/= ':')
      _ <- char ':'
      e <- parsePrimitiveEncoding
      o <- Optional <$ char '*' <|> pure Mandatory
      pure $ StructField o (Attribute n) e

parseDictionaryLineV1 :: Text -> Either ParseError DictionaryEntry
parseDictionaryLineV1 s =
  mapLeft (ParseError . pack) $ parseOnly parseIcicleDictionaryV1 s

writeDictionaryLineV1 :: DictionaryEntry -> Text
writeDictionaryLineV1 (DictionaryEntry (Attribute a) (ConcreteDefinition e)) =
  a <> "|" <> prettyConcrete e

writeDictionaryLineV1 (DictionaryEntry _ (VirtualDefinition _)) = "Virtual features not supported in V1"

prettyConcrete :: Encoding -> Text
prettyConcrete = \case
  StringEncoding   -> "string"
  IntEncoding      -> "int"
  DoubleEncoding   -> "double"
  DateEncoding     -> "date"
  BooleanEncoding  -> "boolean"
  ListEncoding le  -> "[" <> prettyConcrete le <> "]"
  StructEncoding s -> "(" <> intercalate "," (prettyStructField <$> s) <> ")"

prettyStructField :: StructField -> Text
prettyStructField (StructField Mandatory (Attribute n) e) = n <> ":" <> prettyConcrete e
prettyStructField (StructField Optional (Attribute n) e) = n <> ":" <> prettyConcrete e <> "*"
