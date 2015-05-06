-- | Values that are common across all languages
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Value (
      Heap
    , Value     (..)
    , BaseValue (..)
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp.Exp
-- import              Icicle.Internal.Pretty

import              P

import qualified    Data.Map as Map

-- | A heap is just a mapping from names to values.
type Heap n p
 = Map.Map (Name n) (Value n p)


-- | A top-level value can be a function/closure or a base value
data Value n p
 = VBase  BaseValue
 -- | A function carries its own heap, the name of its argument, and the expression to apply.
 -- Actually - we might want the type of the argument here too, for typeOfValue
 | VFun   (Heap n p)  (Name n)  (Exp n p)
 deriving (Show, Ord, Eq)


-- | Base values - real values that can be serialised and whatnot
-- Note that subvalues of these are also base values - so we can't have, say, a pair of functions.
data BaseValue
 = VInt   Int
 | VBool  Bool
 | VArray [BaseValue]
 | VPair  BaseValue BaseValue
 | VSome  BaseValue
 | VNone
 | VMap   (Map.Map BaseValue BaseValue)
 deriving (Show, Ord, Eq)


-- TODO: typeOfValue
-- TODO: pretty instance
