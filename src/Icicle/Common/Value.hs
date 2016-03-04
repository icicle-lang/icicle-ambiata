-- | Values that are common across all languages
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Value (
      Heap
    , Value     (..)
    , getBaseValue
    , collectFactIdentifiersFromBufs
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp.Exp
import              Icicle.Internal.Pretty

import              P

import qualified    Data.Map as Map
import qualified    Data.Set as Set

-- | A heap is just a mapping from names to values.
type Heap a n p
 = Map.Map (Name n) (Value a n p)


-- | A top-level value can be a function/closure or a base value
data Value a n p
 = VBase  !BaseValue
 -- | A function carries its own heap, the name of its argument, and the expression to apply.
 -- Actually - we might want the type of the argument here too, for typeOfValue
 | VFun   !(Heap a n p)  !(Name n)  !(Exp a n p)
 deriving (Show, Ord, Eq)


getBaseValue :: e -> Value a n p -> Either e BaseValue
getBaseValue e VFun{}    = Left e
getBaseValue _ (VBase v) = Right v

collectFactIdentifiersFromBufs :: Value a n p -> Set.Set FactIdentifier
collectFactIdentifiersFromBufs (VBase v) = collectFactIdentifiersFromBufs' v
collectFactIdentifiersFromBufs (VFun{})  = Set.empty

collectFactIdentifiersFromBufs' :: BaseValue -> Set.Set FactIdentifier
collectFactIdentifiersFromBufs' v
 = case v of
    VInt _ -> no
    VDouble _ -> no
    VUnit -> no
    VBool _ -> no
    VTime _ -> no
    VString _ -> no
    VArray vs -> Set.unions $ fmap go vs
    VPair a b -> go a `Set.union` go b
    VLeft a -> go a
    VRight a -> go a
    VSome a -> go a
    VNone -> no
    VMap m -> Set.unions ( (fmap go $ Map.keys m) <> (fmap go $ Map.elems m) )
    VStruct s -> Set.unions $ fmap go $ Map.elems s
    VBuf vs -> Set.unions $ fmap goBuf vs
    VError _ -> no
    -- Do not collect this one because it is not inside a buf!
    VFactIdentifier _ -> no
 where
  no = Set.empty
  go = collectFactIdentifiersFromBufs'

  goBuf (VPair (VFactIdentifier i) _) = Set.singleton i
  goBuf under = go under


instance (Pretty n, Pretty p) => Pretty (Value a n p) where
 pretty (VBase b) = pretty b
 pretty (VFun h n x) = text "closure#" <+> pretty (Map.toList h) <+> text "\\" <> pretty n <> text ". " <> pretty x
