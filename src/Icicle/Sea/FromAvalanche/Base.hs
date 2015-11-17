{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromAvalanche.Base (
    textOfName
  , seaOfName
  , seaOfNameIx
  , seaOfAttributeDesc
  , seaOfString
  , seaOfEscaped
  , seaError
  , seaError'
  , assign
  , suffix
  , tuple
  ) where

import qualified Data.ByteString as B
import           Data.Char (isLower, isUpper, chr)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word (Word8)

import           Icicle.Data

import           Icicle.Internal.Pretty

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

seaOfString :: Text -> Doc
seaOfString txt = "\"" <> seaOfEscaped txt <> "\""

seaOfEscaped :: Text -> Doc
seaOfEscaped = text . escapeWords . B.unpack . T.encodeUtf8

escapeWords :: [Word8] -> [Char]
escapeWords = \case
  []        -> []
  (0x07:xs) -> "\\a"  <> escapeWords xs
  (0x08:xs) -> "\\b"  <> escapeWords xs
  (0x09:xs) -> "\\t"  <> escapeWords xs
  (0x0a:xs) -> "\\r"  <> escapeWords xs
  (0x0b:xs) -> "\\v"  <> escapeWords xs
  (0x0c:xs) -> "\\f"  <> escapeWords xs
  (0x0d:xs) -> "\\n"  <> escapeWords xs
  (0x5c:xs) -> "\\\\" <> escapeWords xs
  (0x22:xs) -> "\\\"" <> escapeWords xs

  (x:xs)
   | x >= 0x1f && x /= 0x7f
   -> chr (fromIntegral x) : escapeWords xs

   | otherwise
   -> printf "\\%03od" x <> escapeWords xs

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
