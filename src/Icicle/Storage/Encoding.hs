{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase       #-}
module Icicle.Storage.Encoding (
    parsePrimitiveEncoding
  , parseEncoding
  , prettyConcrete
  ) where

import           Icicle.Data
import           P hiding (concat, intercalate)

import           Data.Attoparsec.Text

import           Data.Text hiding (takeWhile)

parsePrimitiveEncoding :: Parser Encoding
parsePrimitiveEncoding =
           StringEncoding  <$ string "string"
       <|> IntEncoding     <$ string "int"
       <|> IntEncoding     <$ string "long"
       <|> DoubleEncoding  <$ string "double"
       <|> TimeEncoding    <$ string "time"
       <|> TimeEncoding    <$ string "date"
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

prettyConcrete :: Encoding -> Text
prettyConcrete = \case
  StringEncoding   -> "string"
  IntEncoding      -> "int"
  DoubleEncoding   -> "double"
  TimeEncoding     -> "time"
  BooleanEncoding  -> "boolean"
  ListEncoding le  -> "[" <> prettyConcrete le <> "]"
  StructEncoding s -> "(" <> intercalate "," (prettyStructField <$> s) <> ")"

prettyStructField :: StructField -> Text
prettyStructField (StructField Mandatory (Attribute n) e) = n <> ":" <> prettyConcrete e
prettyStructField (StructField Optional (Attribute n) e) = n <> ":" <> prettyConcrete e <> "*"
