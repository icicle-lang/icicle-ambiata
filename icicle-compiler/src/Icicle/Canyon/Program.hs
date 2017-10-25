{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Canyon.Program (
    Program         (..)
  ) where

import              GHC.Generics (Generic)

import              Icicle.Canyon.Statement
import              Icicle.Common.Base
import              Icicle.Common.Type

import              Icicle.Internal.Pretty

import              P


data Program a n =
  Program
  { input       :: !ValType
  , bindtime    :: !(Name n)
  , maxMapSize  :: !(Name n)
  , statements  :: !(Statement a n)
  }
 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n) => NFData (Program a n)


instance (Pretty n) => Pretty (Program a n) where
  pretty p =
    vsep [
        prettyBinding
          (prettyTypedFlat (pretty $ bindtime p) (pretty TimeT))
          (annotate AnnConstant $ text "TIME")
      , prettyBinding
          (prettyTypedFlat (pretty $ maxMapSize p) (pretty IntT))
          (annotate AnnConstant $ text "MAX_MAP_SIZE")
      , mempty
      , pretty (statements p)
      ]


