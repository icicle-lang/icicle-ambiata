-- | Flat primitives - after the folds are removed
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Prim.Flat (
      Prim        (..)
    , PrimProject (..)
    , PrimUnsafe  (..)
    , PrimArray   (..)
    , PrimMelt    (..)
    , PrimBuf     (..)
    , PrimMap     (..)
    , typeOfPrim
    , flatFragment
    , MeltLogical (..)
    , meltLogical
    , repOfMelt
    , meltType
    , tryMeltType
    , typeOfUnpack
  ) where

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty
import           Icicle.Common.Type
import qualified Icicle.Common.Exp.Prim.Minimal as Min

import qualified Icicle.Common.Fragment         as Frag

import           P

import qualified Data.Map as Map


flatFragment :: Frag.Fragment Prim
flatFragment
 = Frag.Fragment
 { Frag.typeOfPrim           = typeOfPrim
 , Frag.primsFullyApplied    = True
 , Frag.allowLambdas         = Frag.AllowLambdasAsPrimArgs
 }

-- | Primitives for flattened avalanche programs
-- Folds are converted to imperative accessors, loops and so on.
data Prim
 -- | Include a bunch of basic things common across languages
 = PrimMinimal !Min.Prim

 -- | Safe projections
 | PrimProject !PrimProject

 -- | Unsafe projections
 | PrimUnsafe  !PrimUnsafe

 -- | Array prims
 | PrimArray   !PrimArray

 -- | Packing/unpacking prims
 | PrimMelt    !PrimMelt

 -- | Packing and unpacking maps
 | PrimMap     !PrimMap

 -- | Abstract circular buffer prims
 | PrimBuf     !PrimBuf
 deriving (Eq, Ord, Show, Generic)


data PrimProject
 = PrimProjectArrayLength  !ValType
 | PrimProjectOptionIsSome !ValType
 | PrimProjectSumIsRight   !ValType !ValType
 deriving (Eq, Ord, Show, Generic)


data PrimUnsafe
 = PrimUnsafeArrayIndex  !ValType         -- ^ Unchecked array index
 | PrimUnsafeArrayCreate !ValType         -- ^ Create a new, uninitialised array. Not safe to read.
 | PrimUnsafeSumGetLeft  !ValType !ValType -- ^ Get the Left value, which may be garbage
 | PrimUnsafeSumGetRight !ValType !ValType -- ^ Get the Right value, which maybe be garbage
 | PrimUnsafeOptionGet   !ValType         -- ^ Get the Some value, which maybe be garbage
 deriving (Eq, Ord, Show, Generic)


data PrimArray
 = PrimArrayPutMutable   !ValType          -- ^ In-place update
 | PrimArrayPutImmutable !ValType          -- ^ Copy then update
 | PrimArrayZip          !ValType !ValType -- ^ Zip two arrays into one
 | PrimArraySwap         !ValType          -- ^ Swap two elements
 | PrimArrayDel          !ValType          -- ^ Delete a value
 deriving (Eq, Ord, Show, Generic)

data PrimMap
 = PrimMapPack         !ValType !ValType
 | PrimMapUnpackKeys   !ValType !ValType
 | PrimMapUnpackValues !ValType !ValType
 deriving (Eq, Ord, Show, Generic)

data PrimMelt
 = PrimMeltPack        !ValType
 | PrimMeltUnpack !Int !ValType
 deriving (Eq, Ord, Show, Generic)

-- | These correspond directly to the latest buffer primitives in Core.
data PrimBuf
 = PrimBufMake !Int !ValType
 | PrimBufPush !Int !ValType
 | PrimBufRead !Int !ValType
 deriving (Eq, Ord, Show, Generic)


instance NFData Prim
instance NFData PrimProject
instance NFData PrimUnsafe
instance NFData PrimArray
instance NFData PrimMap
instance NFData PrimMelt
instance NFData PrimBuf


-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    -- All arithmetics are int to int for now
    PrimMinimal m
     -> Min.typeOfPrim m


    PrimProject (PrimProjectArrayLength a)
     -> FunT [funOfVal (ArrayT a)] IntT

    PrimProject (PrimProjectOptionIsSome a)
     -> FunT [funOfVal (OptionT a)] BoolT

    PrimProject (PrimProjectSumIsRight a b)
     -> FunT [funOfVal (SumT a b)] BoolT


    PrimUnsafe  (PrimUnsafeArrayIndex a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT] a

    PrimUnsafe  (PrimUnsafeArrayCreate a)
     -> FunT [funOfVal IntT] (ArrayT a)

    PrimUnsafe  (PrimUnsafeOptionGet a)
     -> FunT [funOfVal (OptionT a)] a

    PrimUnsafe  (PrimUnsafeSumGetLeft a b)
     -> FunT [funOfVal (SumT a b)] a

    PrimUnsafe  (PrimUnsafeSumGetRight a b)
     -> FunT [funOfVal (SumT a b)] b


    PrimArray   (PrimArrayZip a b)
     -> FunT [funOfVal (ArrayT a), funOfVal (ArrayT b)] (ArrayT (PairT a b))

    PrimArray   (PrimArrayPutMutable a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT, funOfVal a] (ArrayT a)

    PrimArray   (PrimArrayPutImmutable a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT, funOfVal a] (ArrayT a)

    PrimArray   (PrimArraySwap a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT, funOfVal IntT] (ArrayT a)

    PrimArray   (PrimArrayDel a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT] (ArrayT a)


    PrimMelt    (PrimMeltPack t)
     | ts <- meltType t
     -> FunT (fmap funOfVal ts) t

    PrimMelt    (PrimMeltUnpack ix t)
     | tg <- typeOfUnpack ix t
     -> FunT [funOfVal t] tg


    PrimMap    (PrimMapPack k v)
     -> FunT [funOfVal (ArrayT k), funOfVal (ArrayT v)] (MapT k v)

    PrimMap    (PrimMapUnpackKeys k v)
     -> FunT [funOfVal (MapT k v)] (ArrayT k)

    PrimMap    (PrimMapUnpackValues k v)
     -> FunT [funOfVal (MapT k v)] (ArrayT v)


    PrimBuf     (PrimBufMake i t)
     -> FunT [funOfVal UnitT] (BufT i t)

    PrimBuf     (PrimBufPush i t)
     -> FunT [funOfVal (BufT i t), funOfVal t] (BufT i t)

    PrimBuf     (PrimBufRead i t)
     -> FunT [funOfVal (BufT i t)] (ArrayT t)

-- We need to distinguish between an error and an error which is the tag in (Sum Error t)
data MeltLogical = MeltRep ValType | MeltTagSumError -- | MeltTagSum | MeltTagOption

repOfMelt :: MeltLogical -> ValType
repOfMelt (MeltRep v)     = v
repOfMelt MeltTagSumError = ErrorT

meltLogical :: ValType -> [MeltLogical]
meltLogical t
 = case t of
    UnitT   -> rep t
    IntT    -> rep t
    DoubleT -> rep t
    BoolT   -> rep t
    TimeT   -> rep t
    StringT -> rep t
    ErrorT  -> rep t
    FactIdentifierT
            -> rep t

    PairT   a b -> meltLogical a <> meltLogical b

    SumT    a b
     | ErrorT <- a
     -> [MeltTagSumError]           <> meltLogical b
     | otherwise
     -> rep BoolT  <> meltLogical a <> meltLogical b

    OptionT a   -> rep BoolT <> meltLogical a

    ArrayT a -> nested ArrayT   (meltLogical a)
    BufT i a -> nested (BufT i) (meltLogical a)
    MapT k v -> meltLogical (ArrayT k) <> meltLogical (ArrayT v)

    StructT (StructType fs)
     | Map.null fs
     -> rep UnitT

     | otherwise
     -> concat $ fmap meltLogical (Map.elems fs)
 where
  rep t' = [MeltRep t']
  nested f = fmap (MeltRep . f . repOfMelt)

meltType :: ValType -> [ValType]
meltType = fmap repOfMelt . meltLogical

tryMeltType :: ValType -> Maybe [ValType]
tryMeltType t
 = let ts = meltType t
   in if ts == [t]
      then Nothing
      else Just ts

typeOfUnpack :: Int -> ValType -> ValType
typeOfUnpack ix t
 = case drop ix (meltType t) of
     []     -> UnitT
     (tg:_) -> tg

-- Pretty -------------

instance Pretty Prim where
 pretty (PrimMinimal m) = pretty m

 pretty (PrimProject (PrimProjectArrayLength _a))
  = "Array_length#"
 pretty (PrimProject (PrimProjectOptionIsSome a))
  = text "Option_isSome#" <+> brackets (pretty a)
 pretty (PrimProject (PrimProjectSumIsRight _a _b))
  = "Sum_isRight#"


 pretty (PrimUnsafe (PrimUnsafeArrayIndex _a))
  = "unsafe_Array_index#"

 pretty (PrimUnsafe (PrimUnsafeArrayCreate _a))
  = "unsafe_Array_create#"

 pretty (PrimUnsafe (PrimUnsafeOptionGet _a))
  = "unsafe_Option_get#"

 pretty (PrimUnsafe (PrimUnsafeSumGetLeft _a _b))
  = "unsafe_Sum_left#"

 pretty (PrimUnsafe (PrimUnsafeSumGetRight _a _b))
  = "unsafe_Sum_right#"


 pretty (PrimArray (PrimArrayZip _a _b))
  = "Array_zip#"

 pretty (PrimArray (PrimArrayPutMutable _a))
  = "Array_put_mutable#"

 pretty (PrimArray (PrimArrayPutImmutable _a))
  = "Array_put_immutable#"

 pretty (PrimArray (PrimArraySwap _a))
  = "Array_elem_swap#"

 pretty (PrimArray (PrimArrayDel _a))
  = "Array_elem_delete#"


 pretty (PrimMelt (PrimMeltPack _t))
  = "Melt_pack#"

 pretty (PrimMelt (PrimMeltUnpack i _t))
  = "Melt_unpack" <> pretty i <> "#"


 pretty (PrimMap (PrimMapPack _a _b))
  = "Map_pack#"

 pretty (PrimMap (PrimMapUnpackKeys _a _b))
  = "Map_unpack_keys#"
 pretty (PrimMap (PrimMapUnpackValues _a _b))
  = "Map_unpack_values#"


 pretty (PrimBuf    (PrimBufMake _i _t))
  = "Buf_make#"

 pretty (PrimBuf    (PrimBufPush _i _t))
  = "Buf_push#"

 pretty (PrimBuf    (PrimBufRead _ _t))
  = "Buf_read#"
