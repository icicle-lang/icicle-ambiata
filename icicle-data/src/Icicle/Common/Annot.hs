{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Annot (
      Annot (..)
    ) where

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty
import           Icicle.Common.Type

import           P


data Annot a
 = Annot
 { annType :: !Type
 , annTail :: !a
 }
 deriving (Eq, Ord, Show, Generic)

instance NFData a => NFData (Annot a)

instance Pretty (Annot a) where
 pretty ann
  = pretty (annType ann)
