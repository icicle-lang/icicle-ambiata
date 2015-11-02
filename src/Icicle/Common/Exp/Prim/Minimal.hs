{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Common.Exp.Prim.Minimal (
      Prim   (..)
    , PrimArithUnary(..)
    , PrimArithBinary(..)
    , PrimDouble(..)
    , PrimCast(..)
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
 = PrimArithUnary       PrimArithUnary ArithType
 | PrimArithBinary      PrimArithBinary ArithType
 | PrimDouble   PrimDouble
 | PrimCast     PrimCast
 -- | Relation prims like less than, equal etc work for a bunch of different types
 | PrimRelation PrimRelation ValType
 | PrimLogical  PrimLogical
 | PrimConst    PrimConst
 -- | Date primitives
 | PrimDateTime PrimDateTime
 | PrimPair     PrimPair
 | PrimStruct   PrimStruct
 deriving (Eq, Ord, Show)

-- | Arithmetic primitives, common to all number-like things
data PrimArithUnary
 = PrimArithNegate
 deriving (Eq, Ord, Show)

data PrimArithBinary
 = PrimArithPlus
 | PrimArithMinus
 | PrimArithMul
 | PrimArithPow
 deriving (Eq, Ord, Show)

-- | Specific Double things.
-- Division doesn't really apply to Ints.
data PrimDouble
 = PrimDoubleDiv
 | PrimDoubleLog
 | PrimDoubleExp
 deriving (Eq, Ord, Show)

-- | Casts between types
data PrimCast
 = PrimCastDoubleOfInt
 | PrimCastIntOfDouble
 | PrimCastStringOfInt
 | PrimCastStringOfDouble
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
 = PrimConstPair  ValType ValType
 | PrimConstSome  ValType
 | PrimConstLeft  ValType ValType
 | PrimConstRight ValType ValType
 deriving (Eq, Ord, Show)

-- | DateTime primitives
data PrimDateTime
 = PrimDateTimeDaysDifference
 | PrimDateTimeDaysEpoch
 | PrimDateTimeMinusDays
 | PrimDateTimeMinusMonths
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
    PrimArithUnary _ t
     -> FunT [funOfVal (valTypeOfArithType t)] (valTypeOfArithType t)
    PrimArithBinary _ t
     -> FunT [funOfVal (valTypeOfArithType t), funOfVal (valTypeOfArithType t)] (valTypeOfArithType t)

    PrimDouble PrimDoubleDiv
     -> FunT [funOfVal DoubleT, funOfVal DoubleT] DoubleT
    PrimDouble PrimDoubleLog
     -> FunT [funOfVal DoubleT] DoubleT
    PrimDouble PrimDoubleExp
     -> FunT [funOfVal DoubleT] DoubleT

    PrimCast PrimCastDoubleOfInt
     -> FunT [funOfVal IntT] DoubleT
    PrimCast PrimCastIntOfDouble
     -> FunT [funOfVal DoubleT] IntT
    PrimCast PrimCastStringOfInt
     -> FunT [funOfVal IntT] StringT
    PrimCast PrimCastStringOfDouble
     -> FunT [funOfVal DoubleT] StringT

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
    PrimConst (PrimConstLeft a b)
     -> FunT [funOfVal a] (SumT a b)
    PrimConst (PrimConstRight a b)
     -> FunT [funOfVal b] (SumT a b)

    PrimDateTime PrimDateTimeDaysDifference
     -> FunT [funOfVal DateTimeT, funOfVal DateTimeT] IntT
    PrimDateTime PrimDateTimeDaysEpoch
     -> FunT [funOfVal DateTimeT] IntT
    PrimDateTime PrimDateTimeMinusDays
     -> FunT [funOfVal DateTimeT, funOfVal IntT] DateTimeT
    PrimDateTime PrimDateTimeMinusMonths
     -> FunT [funOfVal DateTimeT, funOfVal IntT] DateTimeT

    PrimPair (PrimPairFst a b)
     -> FunT [funOfVal (PairT a b)] a
    PrimPair (PrimPairSnd a b)
     -> FunT [funOfVal (PairT a b)] b

    PrimStruct (PrimStructGet f t (StructType fs))
     -> FunT [funOfVal (StructT $ StructType $ Map.insert f t fs)] t

-- Pretty -------------

instance Pretty Prim where
 pretty (PrimArithUnary p t)
  = annotate (AnnType $ valTypeOfArithType t) p'
  where
   p'
    = case p of
       PrimArithNegate -> "negate#"

 pretty (PrimArithBinary p t)
  = annotate (AnnType $ valTypeOfArithType t) p'
  where
   p'
    = case p of
       PrimArithPlus    -> "add#"
       PrimArithMinus   -> "sub#"
       PrimArithMul     -> "mul#"
       PrimArithPow     -> "pow#"

 pretty (PrimDouble p)
  = case p of
     PrimDoubleDiv -> "div#"
     PrimDoubleLog -> "log#"
     PrimDoubleExp -> "exp#"

 pretty (PrimCast p)
  = case p of
     PrimCastDoubleOfInt -> "doubleOfInt#"
     PrimCastIntOfDouble -> "intOfDouble#"
     PrimCastStringOfInt -> "stringOfInt#"
     PrimCastStringOfDouble -> "stringOfDouble#"


 pretty (PrimRelation rel t)
  = annotate (AnnType t) prel
  where
   prel
    = case rel of
       PrimRelationGt -> "gt#"
       PrimRelationGe -> "ge#"
       PrimRelationLt -> "lt#"
       PrimRelationLe -> "le#"
       PrimRelationEq -> "eq#"
       PrimRelationNe -> "ne#"

 pretty (PrimLogical  PrimLogicalNot)   = "not#"
 pretty (PrimLogical  PrimLogicalAnd)   = "and#"
 pretty (PrimLogical  PrimLogicalOr)    = "or#"

 pretty (PrimConst (PrimConstPair a b)) = annotate (AnnType $ (pretty a) <+> (pretty b)) "pair#"
 pretty (PrimConst (PrimConstSome t))   = annotate (AnnType t)  "some#"
 pretty (PrimConst (PrimConstLeft  a b))
  = annotate (AnnType $ (pretty a) <+> (pretty b)) "left#"
 pretty (PrimConst (PrimConstRight a b))
  = annotate (AnnType $ (pretty a) <+> (pretty b)) "right#"

 pretty (PrimDateTime PrimDateTimeDaysDifference) = "DateTime_daysDifference#"
 pretty (PrimDateTime PrimDateTimeDaysEpoch)      = "DateTime_daysEpoch#"
 pretty (PrimDateTime PrimDateTimeMinusDays)      = "DateTime_minusDays#"
 pretty (PrimDateTime PrimDateTimeMinusMonths)    = "DateTime_minusMonths#"

 pretty (PrimPair (PrimPairFst a b)) = annotate (AnnType $ (pretty a) <.> (pretty b)) "fst#"
 pretty (PrimPair (PrimPairSnd a b)) = annotate (AnnType $ (pretty a) <.> (pretty b)) "snd#"

 pretty (PrimStruct (PrimStructGet f t fs)) = annotate (AnnType $ (pretty f) <+> (pretty t) <+> (pretty fs)) "get#"

