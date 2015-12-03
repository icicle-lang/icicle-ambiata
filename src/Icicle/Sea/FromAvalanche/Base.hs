{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromAvalanche.Base (
    textOfName
  , seaOfName
  , seaOfNameIx
  , seaOfAttributeDesc
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

import           Data.Char (isLower, isUpper, ord)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T

import           Icicle.Data
import           Icicle.Data.Time (packedOfTime)

import           Icicle.Internal.Pretty

import           Numeric (showHex)

import           P

import           Text.Printf (printf)

------------------------------------------------------------------------

textOfName :: Pretty n => n -> Text
textOfName = T.pack . show . seaOfName

seaOfName :: Pretty n => n -> Doc
seaOfName = string . fmap mangle . show . pretty
  where
    mangle '$' = '_'
    mangle ' ' = '_'
    mangle  c  =  c

seaOfNameIx :: Pretty n => n -> Int -> Doc
seaOfNameIx n ix = seaOfName (pretty n <> text "$ix$" <> int ix)

seaOfAttributeDesc :: Attribute -> Doc
seaOfAttributeDesc (Attribute xs)
  | T.null xs = string ""
  | otherwise = pretty (T.filter isLegal xs)
 where
  isLegal c = isLower c || isUpper c || c == ' ' || c == '_'

------------------------------------------------------------------------

seaOfChar :: Char -> Doc
seaOfChar c = "'" <> text (escapeChars [c]) <> "'"

seaOfString :: Text -> Doc
seaOfString txt = "\"" <> seaOfEscaped txt <> "\""

seaOfEscaped :: Text -> Doc
seaOfEscaped = text . escapeChars . T.unpack

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
