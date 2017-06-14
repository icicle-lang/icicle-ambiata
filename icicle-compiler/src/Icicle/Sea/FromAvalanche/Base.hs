{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Icicle.Sea.FromAvalanche.Base (
    SeaName
  , SeaString
  , takeSeaName
  , takeSeaString
  , asSeaName
  , mangleToSeaName
  , mangleToSeaNameIx
  , attributeAsSeaString
  , seaOfChar
  , seaOfString
  , seaOfEscaped
  , seaOfTime
  , seaError
  , seaError'
  , assign
  , suffix
  , tuple
  ) where

import           Data.Char (ord, isAlpha)
import qualified Data.List as List
import qualified Data.Text as Text

import           Icicle.Data
import           Icicle.Data.Time (packedOfTime)

import           Icicle.Internal.Pretty

import           Numeric (showHex)

import           P

import           Text.Printf (printf)

------------------------------------------------------------------------

-- | A legal C identifier.
newtype SeaName = SeaName {
    getSeaName :: Text
 } deriving (Eq, Ord, Show)

-- | A value that will need be quoted in C.
newtype SeaString = SeaString {
    getSeaString :: Text
 } deriving (Eq, Ord, Show)

takeSeaName :: SeaName -> Text
takeSeaName = getSeaName

takeSeaString :: SeaString -> Text
takeSeaString = getSeaString

seaNameValidHead :: Char -> Bool
seaNameValidHead c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c == '_')
{-# INLINE seaNameValidHead #-}

seaNameValidTail :: Char -> Bool
seaNameValidTail c =
  seaNameValidHead c ||
  (c >= '0' && c <= '9') ||
  (c == '_')
{-# INLINE seaNameValidTail #-}

asSeaName :: Text -> Maybe SeaName
asSeaName t =
  case Text.unpack t of
    x:xs | seaNameValidHead x && all seaNameValidTail xs ->
      Just (SeaName t)
    _ ->
      Nothing

mangleToSeaName :: Pretty n => n -> SeaName
mangleToSeaName (show . pretty -> n) =
  case n of
    [] ->
      -- Technically not right as "" is not a legal C identifier.
      -- This is just how we chose to treat "".
      SeaName ""
    x:_  ->
      let
        n'
          | isAlpha x = n
          | otherwise = "x_" <> n
        mangle c
          | c >= '0' && c <= '9'
          = [c]
          | c >= 'a' && c <= 'z'
          = [c]
          | c >= 'A' && c <= 'Z'
          = [c]
          | c == '_'
          = "__"
          | otherwise
          = "u_" <> showHex (ord c) "" <> "_"
      in
        SeaName . Text.pack . concatMap mangle $ n'

mangleToSeaNameIx :: Pretty n => n -> Int -> SeaName
mangleToSeaNameIx n ix = mangleToSeaName (pretty n <> text "$ix$" <> int ix)

attributeAsSeaString :: Attribute -> SeaString
attributeAsSeaString =
  SeaString . takeAttributeName

------------------------------------------------------------------------

seaOfChar :: Char -> Doc
seaOfChar c = "'" <> text (escapeChars [c]) <> "'"

seaOfString :: Text -> Doc
seaOfString txt = "\"" <> seaOfEscaped txt <> "\""

seaOfEscaped :: Text -> Doc
seaOfEscaped = text . escapeChars . Text.unpack

escapeChars :: [Char] -> [Char]
escapeChars = \case
  []        -> []
  ('\a':xs) -> "\\a"  <> escapeChars xs
  ('\b':xs) -> "\\b"  <> escapeChars xs
  ('\t':xs) -> "\\t"  <> escapeChars xs
  ('\r':xs) -> "\\r"  <> escapeChars xs
  ('\v':xs) -> "\\v"  <> escapeChars xs
  ('\f':xs) -> "\\f"  <> escapeChars xs
  ('\n':xs) -> "\\n"  <> escapeChars xs
  ('\\':xs) -> "\\\\" <> escapeChars xs
  ('\"':xs) -> "\\\"" <> escapeChars xs

  (x:xs)
   | ord x < 0x20 || ord x >= 0x7f && ord x <= 0xff
   -> printf "\\%03o" (ord x) <> escapeChars xs

   | ord x < 0x7f
   -> x : escapeChars xs

   | otherwise
   -> printf "\\U%08x" (ord x) <> escapeChars xs

------------------------------------------------------------------------

seaOfTime :: Time -> Doc
seaOfTime x = text ("0x" <> showHex (packedOfTime x) "")

------------------------------------------------------------------------

seaError :: Show a => Doc -> a -> Doc
seaError subject x = seaError' subject (string (List.take 100 (show x)))

seaError' :: Doc -> Doc -> Doc
seaError' subject msg = line <> "#error Failed during codegen (" <> subject <> ": " <> msg <> "..)" <> line

assign :: Doc -> Doc -> Doc
assign x y = x <> column (\k -> indent (45-k) " =") <+> y

suffix :: Doc -> Doc
suffix annot = column (\k -> indent (85-k) (" /*" <+> annot <+> "*/"))

tuple :: [Doc] -> Doc
tuple []  = "()"
tuple [x] = "(" <> x <> ")"
tuple xs  = "(" <> go xs
  where
    go []     = ")" -- impossible
    go (y:[]) = y <> ")"
    go (y:ys) = y <> ", " <> go ys
