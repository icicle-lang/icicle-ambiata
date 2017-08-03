-- | Values that are common across all languages
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Value (
      Heap
    , Value     (..)
    , getBaseValue
    ) where

import              Icicle.Common.Base
import              Icicle.Common.NanEq
import              Icicle.Common.Exp.Exp
import              Icicle.Internal.Pretty

import              GHC.Generics (Generic)

import              P

import qualified    Data.Map as Map

-- | A heap is just a mapping from names to values.
type Heap a n p
 = Map.Map (Name n) (Value a n p)


-- | A top-level value can be a function/closure or a base value
data Value a n p
 = VBase  !BaseValue
 -- | A function carries its own heap, the name of its argument, and the expression to apply.
 -- Actually - we might want the type of the argument here too, for typeOfValue
 | VFun   !(Heap a n p)  !(Name n)  !(Exp a n p)
 deriving (Show, Ord, Eq, Generic, NanEq)

-- instance (NanEq a, NanEq n, NanEq p) => NanEq (Value a n p)

getBaseValue :: e -> Value a n p -> Either e BaseValue
getBaseValue e VFun{}    = Left e
getBaseValue _ (VBase v) = Right v

instance (Pretty n, Pretty p) => Pretty (Value a n p) where
 pretty (VBase b) = pretty b
 pretty (VFun h n x) = text "closure#" <+> pretty (Map.toList h) <+> text "\\" <> pretty n <> text ". " <> pretty x
