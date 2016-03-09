{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Common.Exp.Prim.Minimal (
      Prim   (..)
    , PrimArithUnary(..)
    , PrimArithBinary(..)
    , PrimToString(..)
    , PrimRelation(..)
    , PrimLogical(..)
    , PrimConst(..)
    , PrimTime (..)
    , PrimPair(..)
    , PrimStruct(..)
    , PrimBuiltinFun(..)
    , PrimBuiltinMath(..)
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
 | PrimToString    PrimToString               -- ^ Conversion to string
 | PrimRelation    PrimRelation    ValType    -- ^ "Polymorphic" relation operators
 | PrimLogical     PrimLogical                -- ^ Logical operators
 | PrimConst       PrimConst                  -- ^ Literal value constructors
 | PrimPair        PrimPair                   -- ^ Pair projections
 | PrimStruct      PrimStruct                 -- ^ Struct projections
 | PrimTime        PrimTime                   -- ^ Time/date primitives
 | PrimBuiltinFun  PrimBuiltinFun
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


data PrimToString
 = PrimToStringFromInt
 | PrimToStringFromDouble
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

--------------------------------------------------------------------------------

-- | Built-in functions, baked-in with direct C implementation.
data PrimBuiltinFun
  = PrimBuiltinMath   PrimBuiltinMath
 deriving (Eq, Ord, Show)

-- | Built-in math functions
data PrimBuiltinMath
 = PrimBuiltinCeiling
 | PrimBuiltinFloor
 | PrimBuiltinTruncate
 | PrimBuiltinRound
 | PrimBuiltinToDoubleFromInt
 | PrimBuiltinDiv
 | PrimBuiltinLog
 | PrimBuiltinExp
 | PrimBuiltinSqrt
 deriving (Eq, Ord, Show, Enum, Bounded)

--------------------------------------------------------------------------------

-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    -- All arithmetics are working on ints for now
    PrimArithUnary _ t
     -> FunT [funOfVal (valTypeOfArithType t)] (valTypeOfArithType t)
    PrimArithBinary _ t
     -> FunT [funOfVal (valTypeOfArithType t), funOfVal (valTypeOfArithType t)] (valTypeOfArithType t)

    -- Built-in functions
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinFloor)
     -> FunT [funOfVal DoubleT] IntT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinCeiling)
     -> FunT [funOfVal DoubleT] IntT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinTruncate)
     -> FunT [funOfVal DoubleT] IntT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinRound)
     -> FunT [funOfVal DoubleT] IntT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinDiv)
     -> FunT [funOfVal DoubleT, funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinLog)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinExp)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinSqrt)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinToDoubleFromInt)
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

instance Pretty PrimBuiltinFun where
 pretty (PrimBuiltinMath   p) = pretty p

instance Pretty PrimBuiltinMath where
 pretty p = case p of
   PrimBuiltinDiv  -> "div#"
   PrimBuiltinLog  -> "log#"
   PrimBuiltinExp  -> "exp#"
   PrimBuiltinSqrt -> "sqrt#"
   PrimBuiltinFloor    -> "floor#"
   PrimBuiltinCeiling  -> "ceil#"
   PrimBuiltinRound    -> "round#"
   PrimBuiltinTruncate -> "trunc#"
   PrimBuiltinToDoubleFromInt -> "doubleOfInt#"

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


 pretty (PrimToString p)
  = case p of
     PrimToStringFromInt    -> "stringOfInt#"
     PrimToStringFromDouble -> "stringOfDouble#"

 pretty (PrimBuiltinFun p)
  = pretty p

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

