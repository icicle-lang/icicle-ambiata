-- | Primitive functions, constant values and so on
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Core.Exp.Prim (
      Prim          (..)
    , PrimFold      (..)
    , PrimArray     (..)
    , PrimMap       (..)
    , PrimLatest    (..)
    , typeOfPrim
    ) where

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty
import           Icicle.Common.Base
import           Icicle.Common.Type
import qualified Icicle.Common.Exp.Prim.Minimal as Min

import           P


-- | Top-level primitive for Core expressions
-- Includes folds etc that won't be present in Avalanche
data Prim
 -- | Include a bunch of basic things common across languages
 = PrimMinimal  !Min.Prim
 -- | Fold and return type
 | PrimFold     !PrimFold !ValType
 -- | Array primitives
 | PrimArray    !PrimArray
 -- | Map primitives
 | PrimMap      !PrimMap
 -- | Circular buffer for latest
 | PrimLatest   !PrimLatest
 | PrimWindow   !WindowUnit !(Maybe WindowUnit)
 deriving (Eq, Ord, Show, Generic)


-- | Folds and destructing things
data PrimFold
 = PrimFoldBool
 | PrimFoldArray  !ValType
 | PrimFoldOption !ValType
 | PrimFoldSum    !ValType !ValType
 | PrimFoldMap    !ValType !ValType
 deriving (Eq, Ord, Show, Generic)


-- | Array primitives
data PrimArray
 = PrimArrayMap !ValType !ValType
 deriving (Eq, Ord, Show, Generic)


-- | Map primitives
data PrimMap
 = PrimMapInsertOrUpdate !ValType !ValType
 | PrimMapDelete         !ValType !ValType
 | PrimMapMapValues      !ValType !ValType !ValType
 | PrimMapLookup         !ValType !ValType
 deriving (Eq, Ord, Show, Generic)


-- | Latest buffer primitives
data PrimLatest
 = PrimLatestPush !Int !ValType
 | PrimLatestRead !Int !ValType
 deriving (Eq, Ord, Show, Generic)

instance NFData Prim
instance NFData PrimFold
instance NFData PrimArray
instance NFData PrimMap
instance NFData PrimLatest


-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    -- All arithmetics are int to int for now
    PrimMinimal m
     -> Min.typeOfPrim m

    -- Folds
    PrimFold PrimFoldBool ret
     -> FunT [FunT [funOfVal UnitT] ret, FunT [funOfVal UnitT] ret, funOfVal BoolT] ret
    PrimFold (PrimFoldArray a) ret
     -> FunT [FunT [funOfVal ret, funOfVal a] ret, funOfVal ret, funOfVal (ArrayT a)] ret
    PrimFold (PrimFoldOption a) ret
     -> FunT [FunT [funOfVal a] ret, FunT [funOfVal UnitT] ret, funOfVal (OptionT a)] ret
    PrimFold (PrimFoldSum    a b) ret
     -> FunT [FunT [funOfVal a] ret, FunT [funOfVal b] ret, funOfVal (SumT a b)] ret
    PrimFold (PrimFoldMap k v) ret
     -> FunT [FunT [funOfVal ret, funOfVal k, funOfVal v] ret, funOfVal ret, funOfVal (MapT k v)] ret

    -- Array primitives
    PrimArray (PrimArrayMap a b)
     -> FunT [FunT [funOfVal a] b, funOfVal (ArrayT a)] (ArrayT b)

    -- Map primitives
    PrimMap (PrimMapInsertOrUpdate k v)
     -> FunT [FunT [funOfVal v] v, funOfVal v, funOfVal k, funOfVal (MapT k v)] (MapT k v)
    PrimMap (PrimMapDelete k v)
     -> FunT [funOfVal k, funOfVal (MapT k v)] (MapT k v)
    PrimMap (PrimMapMapValues k v v')
     -> FunT [FunT [funOfVal v] v', funOfVal (MapT k v)] (MapT k v')
    PrimMap (PrimMapLookup k v)
     -> FunT [funOfVal (MapT k v), funOfVal k] (OptionT v)

    -- Latest buffer primitives
    PrimLatest (PrimLatestPush i t)
     -> FunT [funOfVal (BufT i t), funOfVal t] (BufT i t)
    PrimLatest (PrimLatestRead i t)
     -> FunT [funOfVal (BufT i t)] (ArrayT t)

    PrimWindow _ _
     -> FunT [funOfVal TimeT, funOfVal TimeT] BoolT


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimMinimal m) = pretty m

 pretty (PrimFold f _ret)
  = let f' = case f of
              PrimFoldBool
               -> "if#"
              PrimFoldArray _a
               -> "Array_fold#"
              PrimFoldOption _a
               -> "Option_fold#"
              PrimFoldSum _a _b
               -> "Sum_fold#"
              PrimFoldMap _k _v
               -> "Map_fold#"
    in f'

 pretty (PrimArray (PrimArrayMap _a _b))
  = "Array_map#"

 pretty (PrimMap (PrimMapInsertOrUpdate _k _v))
  = "Map_insertOrUpdate#"

 pretty (PrimMap (PrimMapDelete _k _v))
  = "Map_delete#"

 pretty (PrimMap (PrimMapMapValues _k _v _v'))
  = "Map_mapValues#"

 pretty (PrimMap (PrimMapLookup _k _v))
  = "Map_lookup#"

 pretty (PrimLatest (PrimLatestPush _i _t))
  = "Latest_push#"

 pretty (PrimLatest (PrimLatestRead _ _t))
  = "Latest_read#"

 pretty (PrimWindow newer older)
  = "window# " <> pretty newer <> pretty older
