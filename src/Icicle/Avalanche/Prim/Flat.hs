-- | Flat primitives - after the folds are removed
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Prim.Flat (
      Prim        (..)
    , PrimProject (..)
    , PrimUnsafe  (..)
    , PrimUpdate  (..)
    , PrimArray   (..)
    , PrimMelt    (..)
    , PrimBuf     (..)
    , PrimMap     (..)
    , typeOfPrim
    , flatFragment
    , meltType
    , tryMeltType
    , typeOfGet
  ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import qualified    Icicle.Common.Fragment         as Frag

import              P

import qualified    Data.Map as Map


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
 = PrimMinimal         Min.Prim

 -- | Safe projections
 | PrimProject         PrimProject

 -- | Unsafe projections
 | PrimUnsafe          PrimUnsafe

 -- | Safe updates
 | PrimUpdate          PrimUpdate

 -- | Array prims
 | PrimArray           PrimArray

 -- | Packing/unpacking prims
 | PrimMelt            PrimMelt

 -- | Packing and unpacking maps
 | PrimMap             PrimMap

 -- | Abstract circular buffer prims
 | PrimBuf             PrimBuf
 deriving (Eq, Ord, Show)


data PrimProject
 = PrimProjectArrayLength ValType
 | PrimProjectOptionIsSome ValType
 | PrimProjectSumIsRight   ValType ValType
 deriving (Eq, Ord, Show)


data PrimUnsafe
 = PrimUnsafeArrayIndex ValType
 -- | Create a new, uninitialised array.
 -- This is unsafe because it's uninitialised:
 -- you need to promise me that you'll initialise it before reading from it.
 | PrimUnsafeArrayCreate ValType
 | PrimUnsafeSumGetLeft  ValType ValType
 | PrimUnsafeSumGetRight ValType ValType
 | PrimUnsafeOptionGet   ValType
 deriving (Eq, Ord, Show)


data PrimUpdate
 = PrimUpdateArrayPut  ValType
 deriving (Eq, Ord, Show)

data PrimArray
 = PrimArrayZip ValType ValType -- ^ zip two arrays into one
 deriving (Eq, Ord, Show)

data PrimMap
 = PrimMapPack         ValType ValType
 | PrimMapUnpackKeys   ValType ValType
 | PrimMapUnpackValues ValType ValType
 deriving (Eq, Ord, Show)

data PrimMelt
 = PrimMeltPack       ValType
 | PrimMeltUnpack Int ValType
 deriving (Eq, Ord, Show)

-- | These correspond directly to the latest buffer primitives in Core.
data PrimBuf
 = PrimBufPush Int ValType
 | PrimBufRead Int ValType
 deriving (Eq, Ord, Show)


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

    PrimUpdate  (PrimUpdateArrayPut a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT, funOfVal a] (ArrayT a)


    PrimArray   (PrimArrayZip a b)
     -> FunT [funOfVal (ArrayT a), funOfVal (ArrayT b)] (ArrayT (PairT a b))


    PrimMelt    (PrimMeltPack t)
     | ts <- meltType t
     -> FunT (fmap funOfVal ts) t

    PrimMelt    (PrimMeltUnpack ix t)
     | tg <- typeOfGet ix t
     -> FunT [funOfVal t] tg


    PrimMap    (PrimMapPack k v)
     -> FunT [funOfVal (ArrayT k), funOfVal (ArrayT v)] (MapT k v)

    PrimMap    (PrimMapUnpackKeys k v)
     -> FunT [funOfVal (MapT k v)] (ArrayT k)

    PrimMap    (PrimMapUnpackValues k v)
     -> FunT [funOfVal (MapT k v)] (ArrayT v)


    PrimBuf     (PrimBufPush i t)
     -> FunT [funOfVal (BufT i t), funOfVal t] (BufT i t)

    PrimBuf     (PrimBufRead i t)
     -> FunT [funOfVal (BufT i t)] (ArrayT t)



meltType :: ValType -> [ValType]
meltType t
 = case t of
    UnitT     -> [t]
    IntT      -> [t]
    DoubleT   -> [t]
    BoolT     -> [t]
    DateTimeT -> [t]
    StringT   -> [t]
    ErrorT    -> [t]

    PairT   a b -> meltType a <> meltType b
    SumT    a b -> [BoolT] <> meltType a <> meltType b
    OptionT a   -> [BoolT] <> meltType a

    ArrayT a -> fmap ArrayT   (meltType a)
    BufT i a -> fmap (BufT i) (meltType a)
    MapT k v -> meltType (ArrayT k) <> meltType (ArrayT v)

    StructT (StructType fs)
     | Map.null fs
     -> [UnitT]

     | otherwise
     -> concat $ fmap meltType (Map.elems fs)

tryMeltType :: ValType -> Maybe [ValType]
tryMeltType t
 = let ts = meltType t
   in if ts == [t]
      then Nothing
      else Just ts

typeOfGet :: Int -> ValType -> ValType
typeOfGet ix t
 = case drop ix (meltType t) of
     []     -> UnitT
     (tg:_) -> tg

-- Pretty -------------

instance Pretty Prim where
 pretty (PrimMinimal m) = pretty m

 pretty (PrimProject (PrimProjectArrayLength a))
  = annotate (AnnType a) "Array_length#"
 pretty (PrimProject (PrimProjectOptionIsSome a))
  = text "Option_isSome#" <+> brackets (pretty a)
 pretty (PrimProject (PrimProjectSumIsRight a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Sum_isRight#"


 pretty (PrimUnsafe (PrimUnsafeArrayIndex a))
  = annotate (AnnType a) "unsafe_Array_index#"

 pretty (PrimUnsafe (PrimUnsafeArrayCreate a))
  = annotate (AnnType a) "unsafe_Array_create#"

 pretty (PrimUnsafe (PrimUnsafeOptionGet a))
  = annotate (AnnType a) "unsafe_Option_get#"

 pretty (PrimUnsafe (PrimUnsafeSumGetLeft a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "unsafe_Sum_left#"

 pretty (PrimUnsafe (PrimUnsafeSumGetRight a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "unsafe_Sum_right#"


 pretty (PrimUpdate (PrimUpdateArrayPut a))
  = annotate (AnnType a) "Array_put#"


 pretty (PrimArray (PrimArrayZip a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Array_zip#"


 pretty (PrimMelt (PrimMeltPack t))
  = annotate (AnnType t) "Melt_pack#"

 pretty (PrimMelt (PrimMeltUnpack i t))
  = annotate (AnnType (typeOfGet i t)) ("Melt_unpack" <> int i <> "#")


 pretty (PrimMap (PrimMapPack a b))
  = annotate (AnnType $ (pretty a) <+> (pretty b)) "Map_pack#"

 pretty (PrimMap (PrimMapUnpackKeys a b))
  = annotate (AnnType $ (pretty a) <+> (pretty b)) "Map_unpack_keys#"
 pretty (PrimMap (PrimMapUnpackValues a b))
  = annotate (AnnType $ (pretty a) <+> (pretty b)) "Map_unpack_values#"


 pretty (PrimBuf    (PrimBufPush i t))
  = annotate (AnnType (BufT i t)) "Buf_push#"

 pretty (PrimBuf    (PrimBufRead _ t))
  = annotate (AnnType (ArrayT t)) "Buf_read#"
