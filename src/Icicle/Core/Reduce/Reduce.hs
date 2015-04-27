{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Reduce.Reduce (
      Reduce     (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp

import              P



data Reduce n
 = RFold   ValType ValType (Exp n) (Exp n) (Name n)
 | RLatest ValType         (Exp n)         (Name n)
 deriving (Eq,Ord,Show)


-- Pretty printing ---------------


instance (Pretty n) => Pretty (Reduce n) where
 pretty (RFold t a k z n)
  =   text "rfold  "
  <+> text "[" <> pretty t <> text "]"
  <+> text "[" <> pretty a <> text "]"
  <+> parens (pretty k) <+> parens (pretty z) <+> pretty n
 pretty (RLatest t x n)
  =   text "rlatest"
  <+> text "[" <> pretty t <> text "]"
  <+> parens (pretty x) <+> pretty n

