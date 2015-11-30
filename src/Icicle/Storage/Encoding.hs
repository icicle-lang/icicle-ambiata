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
      pure $ StructField o (mkAttribute n) e

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
prettyStructField (StructField Mandatory n e) = getAttribute n <> ":" <> prettyConcrete e
prettyStructField (StructField Optional  n e) = getAttribute n <> ":" <> prettyConcrete e <> "*"
