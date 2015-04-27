{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Stream.Stream (
      Stream          (..)
    , StreamTransform (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Base
import              Icicle.Core.Exp

import              P



data Stream n
 = Source
 | STrans StreamTransform (Exp n) (Name n)
 deriving (Eq,Ord,Show)

data StreamTransform
 = SFilter
 | SMap
 | STake
 deriving (Eq,Ord,Show)


-- Pretty printing ---------------


instance (Pretty n) => Pretty (Stream n) where
 pretty Source         = text "source"
 pretty (STrans t x n) = pretty t <+> parens (pretty x) <+> pretty n

instance Pretty StreamTransform where
 pretty SFilter = text "sfilter"
 pretty SMap    = text "smap   "
 pretty STake   = text "stake  "
