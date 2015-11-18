{-# LANGUAGE NoImplicitPrelude #-}

module Icicle.Common.Annot (
      Annot (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type

import              P


data Annot a
 = Annot
 { annType :: !Type
 , annTail :: !a
 }
 deriving (Eq, Ord, Show)


instance Pretty (Annot a) where
 pretty ann
  = pretty (annType ann)
