{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Common.Exp.Prim.Minimal (
      Prim   (..)
    , PrimArithUnary(..)
    , PrimArithBinary(..)
    , PrimDouble(..)
    , PrimToInt(..)
    , PrimToDouble(..)
    , PrimToString(..)
    , PrimRelation(..)
    , PrimLogical(..)
    , PrimConst(..)
    , PrimTime (..)
    , PrimPair(..)
    , PrimStruct(..)
    , typeOfPrim
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type

import              P

import qualified    Data.Map as Map


-- | Common primitives in all language fragements.
--
data Prim
 = PrimArithUnary  PrimArithUnary  ArithType  -- ^ "Polymorphic" (double or int) unary operators
 | PrimArithBinary PrimArithBinary ArithType  -- ^ "Polymorphic" (double or int) binary operators
 | PrimDouble      PrimDouble                 -- ^ Arithmetic operators only defined for doubles
 | PrimToInt       PrimToInt                  -- ^ Conversion to int
 | PrimToDouble    PrimToDouble               -- ^ Conversion to double
 | PrimToString    PrimToString               -- ^ Conversion to string
 | PrimRelation    PrimRelation    ValType    -- ^ "Polymorphic" relation operators
 | PrimLogical     PrimLogical                -- ^ Logical operators
 | PrimConst       PrimConst                  -- ^ Literal value constructors
 | PrimPair        PrimPair                   -- ^ Pair projections
 | PrimStruct      PrimStruct                 -- ^ Struct projections
 | PrimTime        PrimTime                   -- ^ Time/date primitives
 deriving (Eq, Ord, Show)

-- | Unary arithmetic primitives common to all numeric types.
--   Must be closed under the set of the input.
data PrimArithUnary
 = PrimArithNegate
 | PrimArithAbsolute
 deriving (Eq, Ord, Show)

-- | Binary arithmetic primitives common to all numeric types.
--   Must be closed under the set of the input.
data PrimArithBinary
 = PrimArithPlus
 | PrimArithMinus
 | PrimArithMul
 | PrimArithPow
 deriving (Eq, Ord, Show)

-- | Primitives that converts numeric types to ints.
data PrimToInt
 = PrimToIntCeiling
 | PrimToIntFloor
 | PrimToIntTruncate
 | PrimToIntRound
 deriving (Eq, Ord, Show)

data PrimToDouble
 = PrimToDoubleFromInt
 deriving (Eq, Ord, Show)

data PrimToString
 = PrimToStringFromInt
 | PrimToStringFromDouble
 deriving (Eq, Ord, Show)

-- | Specific Double things.
-- Division doesn't really apply to Ints.
data PrimDouble
 = PrimDoubleDiv
 | PrimDoubleLog
 | PrimDoubleExp
 | PrimDoubleSqrt
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

-- | Time primitives
data PrimTime
 = PrimTimeDaysDifference
 | PrimTimeDaysEpoch
 | PrimTimeMinusDays
 | PrimTimeMinusMonths
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
    PrimDouble PrimDoubleSqrt
     -> FunT [funOfVal DoubleT] DoubleT

    PrimToInt    PrimToIntFloor
     -> FunT [funOfVal DoubleT] IntT
    PrimToInt    PrimToIntCeiling
     -> FunT [funOfVal DoubleT] IntT
    PrimToInt    PrimToIntTruncate
     -> FunT [funOfVal DoubleT] IntT
    PrimToInt    PrimToIntRound
     -> FunT [funOfVal DoubleT] IntT

    PrimToDouble PrimToDoubleFromInt
     -> FunT [funOfVal IntT] DoubleT

    PrimToString PrimToStringFromInt
     -> FunT [funOfVal IntT] StringT
    PrimToString PrimToStringFromDouble
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

    PrimTime PrimTimeDaysDifference
     -> FunT [funOfVal TimeT, funOfVal TimeT] IntT
    PrimTime PrimTimeDaysEpoch
     -> FunT [funOfVal TimeT] IntT
    PrimTime PrimTimeMinusDays
     -> FunT [funOfVal TimeT, funOfVal IntT] TimeT
    PrimTime PrimTimeMinusMonths
     -> FunT [funOfVal TimeT, funOfVal IntT] TimeT

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
       PrimArithNegate   -> "negate#"
       PrimArithAbsolute -> "abs#"

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
     PrimDoubleDiv  -> "div#"
     PrimDoubleLog  -> "log#"
     PrimDoubleExp  -> "exp#"
     PrimDoubleSqrt -> "sqrt#"

 pretty (PrimToInt p)
  = case p of
     PrimToIntFloor    -> "floor#"
     PrimToIntCeiling  -> "ceil#"
     PrimToIntRound    -> "round#"
     PrimToIntTruncate -> "trunc#"

 pretty (PrimToDouble p)
  = case p of
     PrimToDoubleFromInt -> "doubleOfInt#"

 pretty (PrimToString p)
  = case p of
     PrimToStringFromInt    -> "stringOfInt#"
     PrimToStringFromDouble -> "stringOfDouble#"


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

 pretty (PrimTime PrimTimeDaysDifference) = "Time_daysDifference#"
 pretty (PrimTime PrimTimeDaysEpoch)      = "Time_daysEpoch#"
 pretty (PrimTime PrimTimeMinusDays)      = "Time_minusDays#"
 pretty (PrimTime PrimTimeMinusMonths)    = "Time_minusMonths#"

 pretty (PrimPair (PrimPairFst a b)) = annotate (AnnType $ (pretty a) <.> (pretty b)) "fst#"
 pretty (PrimPair (PrimPairSnd a b)) = annotate (AnnType $ (pretty a) <.> (pretty b)) "snd#"

 pretty (PrimStruct (PrimStructGet f t fs)) = annotate (AnnType $ (pretty f) <+> (pretty t) <+> (pretty fs)) "get#"

