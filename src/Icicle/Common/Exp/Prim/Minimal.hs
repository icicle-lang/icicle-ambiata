{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Prim.Minimal (
      Prim   (..)
    , PrimArith(..)
    , PrimRelation(..)
    , PrimLogical(..)
    , PrimConst(..)
    , PrimDateTime (..)
    , PrimPair(..)
    , PrimStruct(..)
    , typeOfPrim
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type

import              P

import qualified    Data.Map as Map


-- | Top-level primitive
-- Pretty empty for now.
data Prim
 = PrimArith    PrimArith
 -- | Relation prims like less than, equal etc work for a bunch of different types
 | PrimRelation PrimRelation ValType
 | PrimLogical  PrimLogical
 | PrimConst    PrimConst
 -- | Date primitives
 | PrimDateTime PrimDateTime
 | PrimPair     PrimPair
 | PrimStruct   PrimStruct
 deriving (Eq, Ord, Show)

-- | Arithmetic primitives
data PrimArith
 = PrimArithPlus
 | PrimArithMinus
 | PrimArithDiv
 | PrimArithMul
 | PrimArithNegate
 deriving (Eq, Ord, Show)

-- | Predicates like >=
data PrimRelation
 = PrimRelationGt
 | PrimRelationGe
 | PrimRelationLt
 | PrimRelationLe
 | PrimRelationEq
 | PrimRelationNe
 deriving (Eq, Ord, Show)

-- | Logical relations like &&, not
data PrimLogical
 = PrimLogicalNot
 | PrimLogicalAnd
 | PrimLogicalOr
 deriving (Eq, Ord, Show)

-- | Constructors
data PrimConst
 = PrimConstPair ValType ValType
 | PrimConstSome ValType
 deriving (Eq, Ord, Show)

-- | DateTime primitives
data PrimDateTime
 = PrimDateTimeDaysDifference
 deriving (Eq, Ord, Show)

-- | Pair primitives
data PrimPair
 = PrimPairFst ValType ValType
 | PrimPairSnd ValType ValType
 deriving (Eq, Ord, Show)

data PrimStruct
 = PrimStructGet StructField ValType StructType
 deriving (Eq, Ord, Show)



-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    -- All arithmetics are working on ints for now
    PrimArith PrimArithNegate
     -> FunT [intT] IntT
    PrimArith _
     -> FunT [intT, intT] IntT

    -- All relations are binary to bool
    PrimRelation _ val
     -> FunT [funOfVal val, funOfVal val] BoolT

    -- Logical relations
    PrimLogical PrimLogicalNot
     -> FunT [funOfVal BoolT] BoolT
    PrimLogical PrimLogicalAnd
     -> FunT [funOfVal BoolT, funOfVal BoolT] BoolT
    PrimLogical PrimLogicalOr
     -> FunT [funOfVal BoolT, funOfVal BoolT] BoolT

    -- Constants
    PrimConst (PrimConstPair a b)
     -> FunT [funOfVal a, funOfVal b] (PairT a b)
    PrimConst (PrimConstSome a)
     -> FunT [funOfVal a] (OptionT a)

    PrimDateTime PrimDateTimeDaysDifference
     -> FunT [funOfVal DateTimeT, funOfVal DateTimeT] IntT

    PrimPair (PrimPairFst a b)
     -> FunT [funOfVal (PairT a b)] a
    PrimPair (PrimPairSnd a b)
     -> FunT [funOfVal (PairT a b)] b

    PrimStruct (PrimStructGet f t (StructType fs))
     -> FunT [funOfVal (StructT $ StructType $ Map.insert f t fs)] t
 where
  intT = FunT [] IntT


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimArith PrimArithPlus)       = text  "add#"
 pretty (PrimArith PrimArithMinus)      = text  "sub#"
 pretty (PrimArith PrimArithDiv)        = text  "div#"
 pretty (PrimArith PrimArithMul)        = text  "mul#"
 pretty (PrimArith PrimArithNegate)     = text  "negate#"

 pretty (PrimRelation rel t)
  = text prel <+> brackets (pretty t)
  where
   prel
    = case rel of
       PrimRelationGt -> "gt#"
       PrimRelationGe -> "ge#"
       PrimRelationLt -> "lt#"
       PrimRelationLe -> "le#"
       PrimRelationEq -> "eq#"
       PrimRelationNe -> "ne#"

 pretty (PrimLogical  PrimLogicalNot)   = text  "not#"
 pretty (PrimLogical  PrimLogicalAnd)   = text  "and#"
 pretty (PrimLogical  PrimLogicalOr)    = text  "or#"

 pretty (PrimConst (PrimConstPair a b)) = text "pair#" <+> brackets (pretty a) <+> brackets (pretty b)
 pretty (PrimConst (PrimConstSome t))   = text "some#" <+> brackets (pretty t)

 pretty (PrimDateTime PrimDateTimeDaysDifference)
  = text "DateTime_daysDifference#"

 pretty (PrimPair (PrimPairFst a b)) = text "fst#" <+> brackets (pretty a) <+> brackets (pretty b)
 pretty (PrimPair (PrimPairSnd a b)) = text "snd#" <+> brackets (pretty a) <+> brackets (pretty b)

 pretty (PrimStruct (PrimStructGet f t fs)) = text "get#" <+> brackets (pretty f) <+> brackets (pretty t) <+> brackets (pretty fs)

