{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Reduce.Reduce (
      Reduce     (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Exp

import              P



data Reduce n
 = RFold   (Exp n) (Exp n) (Name n)
 | RLatest (Exp n)         (Name n)
 deriving (Eq,Ord,Show)


-- Pretty printing ---------------


instance (Pretty n) => Pretty (Reduce n) where
 pretty (RFold k z n) = text "rfold  " <+> parens (pretty k) <+> parens (pretty z) <+> pretty n
 pretty (RLatest x n) = text "rlatest" <+> parens (pretty x) <+> pretty n

