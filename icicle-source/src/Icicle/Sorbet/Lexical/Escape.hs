{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sorbet.Lexical.Escape (
    escapeChars
  , unescapeChars

  , UnescapeError(..)
  , renderUnescapeError
  ) where

import           Data.Char (ord, chr)
import qualified Data.Text as T

import           P

import           Numeric (readHex)

import           Text.Printf (printf)


data UnescapeError =
    UnescapeCodepoint16TooShort ![Char]
  | UnescapeCodepoint16Invalid ![Char]
  | UnescapeCodepoint32TooShort ![Char]
  | UnescapeCodepoint32Invalid ![Char]
    deriving (Eq, Ord, Show)

renderUnescapeError :: UnescapeError -> Text
renderUnescapeError = \case
  UnescapeCodepoint16TooShort xs ->
    "16-bit unicode codepoint was not exactly 4 characters: \\u" <> T.pack xs
  UnescapeCodepoint16Invalid xs ->
    "16-bit unicode codepoint contained non-hex characters: \\u" <> T.pack xs
  UnescapeCodepoint32TooShort xs ->
    "32-bit unicode codepoint was not exactly 8 characters: \\u" <> T.pack xs
  UnescapeCodepoint32Invalid xs ->
    "32-bit unicode codepoint contained non-hex characters: \\u" <> T.pack xs

escapeChars :: [Char] -> [Char]
escapeChars = \case
  '\a' : xs -> '\\' :  'a' : escapeChars xs
  '\b' : xs -> '\\' :  'b' : escapeChars xs
  '\f' : xs -> '\\' :  'f' : escapeChars xs
  '\n' : xs -> '\\' :  'n' : escapeChars xs
  '\r' : xs -> '\\' :  'r' : escapeChars xs
  '\t' : xs -> '\\' :  't' : escapeChars xs
  '\v' : xs -> '\\' :  'v' : escapeChars xs
  '\"' : xs -> '\\' :  '"' : escapeChars xs
  '\\' : xs -> '\\' : '\\' : escapeChars xs

  x : xs ->
    if ' ' <= x && x <= '~' then
      x : escapeChars xs
    else
      escapeCodepoint (ord x) <> escapeChars xs

  [] ->
    []

unescapeChars :: [Char] -> Either UnescapeError [Char]
unescapeChars = \case
  '\\' :  'a' : xs -> ('\a' :) <$> unescapeChars xs
  '\\' :  'b' : xs -> ('\b' :) <$> unescapeChars xs
  '\\' :  'f' : xs -> ('\f' :) <$> unescapeChars xs
  '\\' :  'n' : xs -> ('\n' :) <$> unescapeChars xs
  '\\' :  'r' : xs -> ('\r' :) <$> unescapeChars xs
  '\\' :  't' : xs -> ('\t' :) <$> unescapeChars xs
  '\\' :  'v' : xs -> ('\v' :) <$> unescapeChars xs
  '\\' :  '"' : xs -> ('\"' :) <$> unescapeChars xs
  '\\' : '\\' : xs -> ('\\' :) <$> unescapeChars xs

  '\\' :  'u' : xs
    | (ys, zs) <- splitAt 4 xs
    -> (:) <$> unescapeCodepoint16 ys <*> unescapeChars zs

  '\\' :  'U' : xs
    | (ys, zs) <- splitAt 8 xs
    -> (:) <$> unescapeCodepoint32 ys <*> unescapeChars zs

  x : xs ->
    (x :) <$> unescapeChars xs

  [] ->
    pure []

escapeCodepoint :: Int -> [Char]
escapeCodepoint n =
  if n >= 0x10000 then
    printf "\\U%08x" n
  else
    printf "\\u%04x" n

unescapeCodepoint16 :: [Char] -> Either UnescapeError Char
unescapeCodepoint16 =
  unescapeCodepoint 4 UnescapeCodepoint16TooShort UnescapeCodepoint16Invalid

unescapeCodepoint32 :: [Char] -> Either UnescapeError Char
unescapeCodepoint32 =
  unescapeCodepoint 8 UnescapeCodepoint32TooShort UnescapeCodepoint32Invalid

unescapeCodepoint ::
  Int ->
  ([Char] -> UnescapeError) ->
  ([Char] -> UnescapeError) ->
  [Char] ->
  Either UnescapeError Char
unescapeCodepoint chars tooShort invalid xs =
  if length xs /= chars then
    Left $ tooShort xs
  else
    case readHex xs of
      (n, "") : _ ->
        pure . chr $ n
      _ ->
        Left $ invalid xs
