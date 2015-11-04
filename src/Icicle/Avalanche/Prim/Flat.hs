-- | Flat primitives - after the folds are removed
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Avalanche.Prim.Flat (
      Prim        (..)
    , PrimProject (..)
    , PrimUnsafe  (..)
    , PrimUpdate  (..)
    , PrimArray   (..)
    , PrimPack    (..)
    , PrimBuf     (..)
    , typeOfPrim
    , flatFragment
  ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import qualified    Icicle.Common.Fragment         as Frag

import              P

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

 -- | Packing prims
 | PrimPack            PrimPack

 -- | Abstract circular buffer prims
 | PrimBuf             PrimBuf
 deriving (Eq, Ord, Show)


data PrimProject
 = PrimProjectArrayLength  ValType
 | PrimProjectMapLength    ValType ValType
 | PrimProjectMapLookup    ValType ValType
 | PrimProjectOptionIsSome ValType
 | PrimProjectSumIsRight   ValType ValType
 deriving (Eq, Ord, Show)


data PrimUnsafe
 = PrimUnsafeArrayIndex ValType
 -- | Create a new, uninitialised array.
 -- This is unsafe because it's uninitialised:
 -- you need to promise me that you'll initialise it before reading from it.
 | PrimUnsafeArrayCreate ValType
 | PrimUnsafeMapIndex    ValType ValType
 | PrimUnsafeSumGetLeft  ValType ValType
 | PrimUnsafeSumGetRight ValType ValType
 | PrimUnsafeOptionGet   ValType
 deriving (Eq, Ord, Show)


data PrimUpdate
 = PrimUpdateMapPut    ValType ValType
 | PrimUpdateArrayPut  ValType
 | PrimUpdateArrayPut2 ValType ValType -- ^ update 2 arrays, should become C statement exps
 deriving (Eq, Ord, Show)

data PrimArray
 = PrimArrayZip     ValType ValType -- ^ zip two arrays into one
 | PrimArrayUnzip   ValType ValType -- ^ unzip an array of pairs into two arrays of pairs
 | PrimArraySum     ValType ValType -- ^ combine three arrays into an array of sums
 | PrimArrayUnsum   ValType ValType -- ^ split an array of sums into three arrays
 deriving (Eq, Ord, Show)

data PrimPack
 = PrimSumPack    ValType ValType
 | PrimOptionPack ValType
 deriving (Eq, Ord, Show)


-- | These correspond directly to the latest buffer primitives in Core.
data PrimBuf
 = PrimBufMake ValType
 | PrimBufPush ValType
 | PrimBufRead ValType
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

    PrimProject (PrimProjectMapLength a b)
     -> FunT [funOfVal (MapT a b)] IntT

    PrimProject (PrimProjectMapLookup a b)
     -> FunT [funOfVal (MapT a b), funOfVal a] (OptionT b)


    PrimProject (PrimProjectOptionIsSome a)
     -> FunT [funOfVal (OptionT a)] BoolT

    PrimProject (PrimProjectSumIsRight a b)
     -> FunT [funOfVal (SumT a b)] BoolT


    PrimUnsafe  (PrimUnsafeArrayIndex a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT] a

    PrimUnsafe  (PrimUnsafeArrayCreate a)
     -> FunT [funOfVal IntT] (ArrayT a)

    PrimUnsafe  (PrimUnsafeMapIndex a b)
     -> FunT [funOfVal (MapT a b), funOfVal IntT] (PairT a b)

    PrimUnsafe  (PrimUnsafeOptionGet a)
     -> FunT [funOfVal (OptionT a)] a

    PrimUnsafe  (PrimUnsafeSumGetLeft a b)
     -> FunT [funOfVal (SumT a b)] a
    PrimUnsafe  (PrimUnsafeSumGetRight a b)
     -> FunT [funOfVal (SumT a b)] b


    PrimUpdate  (PrimUpdateMapPut a b)
     -> FunT [funOfVal (MapT a b), funOfVal a, funOfVal b] (MapT a b)

    PrimUpdate  (PrimUpdateArrayPut a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT, funOfVal a] (ArrayT a)

    PrimUpdate  (PrimUpdateArrayPut2 a b)
     -> FunT [funOfVal (ArrayT a), funOfVal (ArrayT b), funOfVal IntT, funOfVal a, funOfVal b] (PairT (ArrayT a) (ArrayT b))


    PrimArray   (PrimArrayZip a b)
     -> FunT [funOfVal (ArrayT a), funOfVal (ArrayT b)] (ArrayT (PairT a b))

    PrimArray   (PrimArrayUnzip a b)
     -> FunT [funOfVal (ArrayT (PairT a b))] (PairT (ArrayT a) (ArrayT b))

    PrimArray   (PrimArraySum a b)
     -> FunT [funOfVal (ArrayT BoolT), funOfVal (ArrayT a), funOfVal (ArrayT b)] (ArrayT (SumT a b))

    PrimArray   (PrimArrayUnsum a b)
     -> FunT [funOfVal (ArrayT (SumT a b))] (PairT (ArrayT BoolT) (PairT (ArrayT a) (ArrayT b)))


    PrimPack    (PrimSumPack a b)
     -> FunT [funOfVal BoolT, funOfVal a, funOfVal b] (SumT a b)

    PrimPack    (PrimOptionPack t)
     -> FunT [funOfVal BoolT, funOfVal t] (OptionT t)


    PrimBuf     (PrimBufMake t)
     -> FunT [funOfVal IntT] (BufT t)

    PrimBuf     (PrimBufPush t)
     -> FunT [funOfVal (BufT t), funOfVal t] (BufT t)

    PrimBuf     (PrimBufRead t)
     -> FunT [funOfVal (BufT t)] (ArrayT t)


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimMinimal m) = pretty m

 pretty (PrimProject (PrimProjectArrayLength a))
  = annotate (AnnType a) "Array_length#"
 pretty (PrimProject (PrimProjectMapLength a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Map_length#"
 pretty (PrimProject (PrimProjectMapLookup a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Map_lookup#"
 pretty (PrimProject (PrimProjectOptionIsSome a))
  = text "Option_isSome#" <+> brackets (pretty a)
 pretty (PrimProject (PrimProjectSumIsRight a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Sum_isRight#"


 pretty (PrimUnsafe (PrimUnsafeArrayIndex a))
  = annotate (AnnType a) "unsafe_Array_index#"

 pretty (PrimUnsafe (PrimUnsafeArrayCreate a))
  = annotate (AnnType a) "unsafe_Array_create#"

 pretty (PrimUnsafe (PrimUnsafeMapIndex a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "unsafe_Map_index#"

 pretty (PrimUnsafe (PrimUnsafeOptionGet a))
  = annotate (AnnType a) "unsafe_Option_get#"

 pretty (PrimUnsafe (PrimUnsafeSumGetLeft a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "unsafe_Sum_left#"

 pretty (PrimUnsafe (PrimUnsafeSumGetRight a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "unsafe_Sum_right#"



 pretty (PrimUpdate (PrimUpdateMapPut a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Map_put#"

 pretty (PrimUpdate (PrimUpdateArrayPut a))
  = annotate (AnnType a) "Array_put#"

 pretty (PrimUpdate (PrimUpdateArrayPut2 a b))
  = annotate (AnnType $ pretty a <.> pretty b) "Array_put2#"


 pretty (PrimArray (PrimArrayZip a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Array_zip#"

 pretty (PrimArray (PrimArrayUnzip a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Array_unzip#"

 pretty (PrimArray (PrimArraySum a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Array_sum#"

 pretty (PrimArray (PrimArrayUnsum a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Array_unsum#"


 pretty (PrimPack (PrimOptionPack t))
  = annotate (AnnType t) "Option_pack#"

 pretty (PrimPack (PrimSumPack a b))
  = annotate (AnnType $ (pretty a) <.> (pretty b)) "Sum_pack#"


 pretty (PrimBuf    (PrimBufMake t))
  = annotate (AnnType t) "Buf_make#"

 pretty (PrimBuf    (PrimBufPush t))
  = annotate (AnnType t) "Buf_push#"

 pretty (PrimBuf    (PrimBufRead t))
  = annotate (AnnType t) "Buf_read#"
