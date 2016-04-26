{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Common.Exp.Prim.Minimal (
      Prim            (..)
    , PrimArithUnary  (..)
    , PrimArithBinary (..)
    , PrimToString    (..)
    , PrimRelation    (..)
    , PrimLogical     (..)
    , PrimConst       (..)
    , PrimTime        (..)
    , PrimPair        (..)
    , PrimStruct      (..)
    , PrimBuiltinFun  (..)
    , PrimBuiltinMath (..)
    , PrimBuiltinMap  (..)
    , typeOfPrim
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type
import              Icicle.Common.Exp.Prim.Builtin

import              P

import qualified    Data.Map as Map


-- | Common primitives in all language fragements.
--
data Prim
 = PrimArithUnary  !PrimArithUnary  !ArithType  -- ^ "Polymorphic" (double or int) unary operators
 | PrimArithBinary !PrimArithBinary !ArithType  -- ^ "Polymorphic" (double or int) binary operators
 | PrimToString    !PrimToString                -- ^ Conversion to string
 | PrimRelation    !PrimRelation    !ValType    -- ^ "Polymorphic" relation operators
 | PrimLogical     !PrimLogical                 -- ^ Logical operators
 | PrimConst       !PrimConst                   -- ^ Literal value constructors
 | PrimPair        !PrimPair                    -- ^ Pair projections
 | PrimStruct      !PrimStruct                  -- ^ Struct projections
 | PrimTime        !PrimTime                    -- ^ Time/date primitives
 | PrimBuiltinFun  !PrimBuiltinFun
 deriving (Eq, Ord, Show)

-- | Unary arithmetic primitives common to all numeric types.
--   Must be closed under the set of the input.
data PrimArithUnary
 = PrimArithNegate
 | PrimArithAbsolute
 deriving (Eq, Ord, Show, Enum, Bounded)

-- | Binary arithmetic primitives common to all numeric types.
--   Must be closed under the set of the input.
data PrimArithBinary
 = PrimArithPlus
 | PrimArithMinus
 | PrimArithMul
 | PrimArithPow
 deriving (Eq, Ord, Show, Enum, Bounded)

data PrimToString
 = PrimToStringFromInt
 | PrimToStringFromDouble
 deriving (Eq, Ord, Show, Enum, Bounded)

-- | Predicates like >=
data PrimRelation
 = PrimRelationGt
 | PrimRelationGe
 | PrimRelationLt
 | PrimRelationLe
 | PrimRelationEq
 | PrimRelationNe
 deriving (Eq, Ord, Show, Enum, Bounded)

-- | Logical relations like &&, not
data PrimLogical
 = PrimLogicalNot
 | PrimLogicalAnd
 | PrimLogicalOr
 deriving (Eq, Ord, Show, Enum, Bounded)

-- | Constructors
data PrimConst
 = PrimConstPair  !ValType !ValType
 | PrimConstSome  !ValType
 | PrimConstLeft  !ValType !ValType
 | PrimConstRight !ValType !ValType
 deriving (Eq, Ord, Show)

-- | Time primitives
data PrimTime
 = PrimTimeDaysDifference
 | PrimTimeDaysEpoch
 | PrimTimeMinusDays
 | PrimTimeMinusMonths
 deriving (Eq, Ord, Show, Enum, Bounded)

-- | Pair primitives
data PrimPair
 = PrimPairFst !ValType !ValType
 | PrimPairSnd !ValType !ValType
 deriving (Eq, Ord, Show)

data PrimStruct
 = PrimStructGet !StructField !ValType !StructType
 deriving (Eq, Ord, Show)

instance NFData PrimPair        where rnf x = seq x ()
instance NFData PrimArithUnary  where rnf x = seq x ()
instance NFData PrimArithBinary where rnf x = seq x ()
instance NFData PrimToString    where rnf x = seq x ()
instance NFData PrimRelation    where rnf x = seq x ()
instance NFData PrimLogical     where rnf x = seq x ()
instance NFData PrimConst       where rnf x = seq x ()
instance NFData PrimTime        where rnf x = seq x ()
instance NFData PrimStruct      where rnf x = seq x ()
instance NFData Prim            where rnf x = seq x ()

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

    PrimBuiltinFun    (PrimBuiltinMap (PrimBuiltinKeys k v))
     -> FunT [funOfVal (MapT k v)] (ArrayT k)
    PrimBuiltinFun    (PrimBuiltinMap (PrimBuiltinVals k v))
     -> FunT [funOfVal (MapT k v)] (ArrayT v)

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
 pretty (PrimArithUnary PrimArithNegate   t) = annotateArithType t "negate#"
 pretty (PrimArithUnary PrimArithAbsolute t) = annotateArithType t "abs#"

 pretty (PrimArithBinary PrimArithPlus  t) = annotateArithType t "add#"
 pretty (PrimArithBinary PrimArithMinus t) = annotateArithType t "sub#"
 pretty (PrimArithBinary PrimArithMul   t) = annotateArithType t "mul#"
 pretty (PrimArithBinary PrimArithPow   t) = annotateArithType t "pow#"

 pretty (PrimRelation PrimRelationGt t) = annotateTypeArgs [t] "gt#"
 pretty (PrimRelation PrimRelationGe t) = annotateTypeArgs [t] "ge#"
 pretty (PrimRelation PrimRelationLt t) = annotateTypeArgs [t] "lt#"
 pretty (PrimRelation PrimRelationLe t) = annotateTypeArgs [t] "le#"
 pretty (PrimRelation PrimRelationEq t) = annotateTypeArgs [t] "eq#"
 pretty (PrimRelation PrimRelationNe t) = annotateTypeArgs [t] "ne#"

 pretty (PrimToString PrimToStringFromInt)    = "stringOfInt#"
 pretty (PrimToString PrimToStringFromDouble) = "stringOfDouble#"

 pretty (PrimLogical  PrimLogicalNot)   = "not#"
 pretty (PrimLogical  PrimLogicalAnd)   = "and#"
 pretty (PrimLogical  PrimLogicalOr)    = "or#"

 pretty (PrimConst (PrimConstPair a b))  = annotateTypeArgs [a,b] "pair#"
 pretty (PrimConst (PrimConstSome t))    = annotate (AnnType t)   "some#"
 pretty (PrimConst (PrimConstLeft  a b)) = annotateTypeArgs [a,b] "left#"
 pretty (PrimConst (PrimConstRight a b)) = annotateTypeArgs [a,b] "right#"

 pretty (PrimTime PrimTimeDaysDifference) = "Time_daysDifference#"
 pretty (PrimTime PrimTimeDaysEpoch)      = "Time_daysEpoch#"
 pretty (PrimTime PrimTimeMinusDays)      = "Time_minusDays#"
 pretty (PrimTime PrimTimeMinusMonths)    = "Time_minusMonths#"

 pretty (PrimPair (PrimPairFst a b)) = annotateTypeArgs [a,b] "fst#"
 pretty (PrimPair (PrimPairSnd a b)) = annotateTypeArgs [a,b] "snd#"

 pretty (PrimStruct (PrimStructGet f t fs)) = annotateTypeArgs [pretty f, pretty t, pretty fs] "get#"

 pretty (PrimBuiltinFun p) = pretty p

annotateArithType :: ArithType -> Doc -> Doc
annotateArithType t = annotate (AnnType (valTypeOfArithType t))
