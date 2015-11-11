{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Base (
    textOfName
  , seaOfName
  , seaOfNameIx
  , seaOfAttributeDesc
  , seaError
  , seaError'
  , assign
  , suffix
  , tuple
  ) where

import           Data.Char (isLower, isUpper)
import           Data.Text (Text)
import qualified Data.Text as T

import           Icicle.Data

import           Icicle.Internal.Pretty

import           P

import qualified Data.List as List


------------------------------------------------------------------------

textOfName :: Pretty n => n -> Text
textOfName = T.pack . show . seaOfName

seaOfName :: Pretty n => n -> Doc
seaOfName = string . fmap mangle . show . pretty
  where
    mangle '$' = '_'
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

seaError :: Show a => Doc -> a -> Doc
seaError subject x = seaError' subject (string (List.take 40 (show x)))

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
