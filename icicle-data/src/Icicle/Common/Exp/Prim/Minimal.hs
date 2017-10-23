{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Common.Exp.Prim.Minimal (
      Prim            (..)
    , PrimArithUnary  (..)
    , PrimArithBinary (..)
    , PrimRelation    (..)
    , PrimLogical     (..)
    , PrimConst       (..)
    , PrimTime        (..)
    , PrimPair        (..)
    , PrimStruct      (..)
    , PrimBuiltinFun  (..)
    , PrimBuiltinMath (..)
    , PrimBuiltinMap  (..)
    , PrimBuiltinArray(..)
    , ArithType       (..)
    , typeOfPrim
    ) where

import qualified Data.Map as Map

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty
import           Icicle.Common.NanEq
import           Icicle.Common.Type
import           Icicle.Common.Exp.Prim.Builtin

import           P


-- | Common primitives in all language fragements.
--
data Prim
 = PrimArithUnary  !PrimArithUnary  !ArithType  -- ^ "Polymorphic" (double or int) unary operators
 | PrimArithBinary !PrimArithBinary !ArithType  -- ^ "Polymorphic" (double or int) binary operators
 | PrimRelation    !PrimRelation    !ValType    -- ^ "Polymorphic" relation operators
 | PrimLogical     !PrimLogical                 -- ^ Logical operators
 | PrimConst       !PrimConst                   -- ^ Literal value constructors
 | PrimPair        !PrimPair                    -- ^ Pair projections
 | PrimStruct      !PrimStruct                  -- ^ Struct projections
 | PrimTime        !PrimTime                    -- ^ Time/date primitives
 | PrimBuiltinFun  !PrimBuiltinFun
 deriving (Eq, Ord, Show, Generic, NanEq)

-- | Unary arithmetic primitives common to all numeric types.
--   Must be closed under the set of the input.
data PrimArithUnary
 = PrimArithNegate
 | PrimArithAbsolute
 deriving (Eq, Ord, Show, Enum, Bounded, Generic, NanEq)

-- | Binary arithmetic primitives common to all numeric types.
--   Must be closed under the set of the input.
data PrimArithBinary
 = PrimArithPlus
 | PrimArithMinus
 | PrimArithMul
 deriving (Eq, Ord, Show, Enum, Bounded, Generic, NanEq)

-- | Predicates like >=
data PrimRelation
 = PrimRelationGt
 | PrimRelationGe
 | PrimRelationLt
 | PrimRelationLe
 | PrimRelationEq
 | PrimRelationNe
 deriving (Eq, Ord, Show, Enum, Bounded, Generic, NanEq)

-- | Logical relations like &&, not
data PrimLogical
 = PrimLogicalNot
 | PrimLogicalAnd
 | PrimLogicalOr
 deriving (Eq, Ord, Show, Enum, Bounded, Generic, NanEq)

-- | Constructors
data PrimConst
 = PrimConstPair  !ValType !ValType
 | PrimConstSome  !ValType
 | PrimConstLeft  !ValType !ValType
 | PrimConstRight !ValType !ValType
 deriving (Eq, Ord, Show, Generic, NanEq)

-- | Time primitives
data PrimTime
 = PrimTimeDaysDifference
 | PrimTimeSecondsDifference
 | PrimTimeDaysJulianEpoch
 | PrimTimeSecondsJulianEpoch
 | PrimTimeMinusDays
 | PrimTimeMinusSeconds
 | PrimTimeMinusMonths
 | PrimTimeProjectDay
 | PrimTimeProjectMonth
 | PrimTimeProjectYear
 deriving (Eq, Ord, Show, Enum, Bounded, Generic, NanEq)

-- | Pair primitives
data PrimPair
 = PrimPairFst !ValType !ValType
 | PrimPairSnd !ValType !ValType
 deriving (Eq, Ord, Show, Generic, NanEq)

data PrimStruct
 = PrimStructGet !StructField !ValType !StructType
 deriving (Eq, Ord, Show, Generic, NanEq)

instance NFData PrimPair
instance NFData PrimArithUnary
instance NFData PrimArithBinary
instance NFData PrimRelation
instance NFData PrimLogical
instance NFData PrimConst
instance NFData PrimTime
instance NFData PrimStruct
instance NFData Prim

--------------------------------------------------------------------------------

-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
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
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinDoubleIsValid)
     -> FunT [funOfVal DoubleT] BoolT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinRound)
     -> FunT [funOfVal DoubleT] IntT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinDiv)
     -> FunT [funOfVal DoubleT, funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinPow)
     -> FunT [funOfVal DoubleT, funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinLog)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinExp)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinSqrt)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinToDoubleFromInt)
     -> FunT [funOfVal IntT] DoubleT

    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinAtan2)
     -> FunT [funOfVal DoubleT, funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinAcos)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinAsin)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinAtan)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinCos)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinCosh)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinSin)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinSinh)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinTan)
     -> FunT [funOfVal DoubleT] DoubleT
    PrimBuiltinFun    (PrimBuiltinMath PrimBuiltinTanh)
     -> FunT [funOfVal DoubleT] DoubleT

    PrimBuiltinFun    (PrimBuiltinMap (PrimBuiltinKeys k v))
     -> FunT [funOfVal (MapT k v)] (ArrayT k)
    PrimBuiltinFun    (PrimBuiltinMap (PrimBuiltinVals k v))
     -> FunT [funOfVal (MapT k v)] (ArrayT v)

    PrimBuiltinFun    (PrimBuiltinArray (PrimBuiltinSort t))
     -> FunT [funOfVal (ArrayT t)] (ArrayT t)
    PrimBuiltinFun    (PrimBuiltinArray (PrimBuiltinLength t))
     -> FunT [funOfVal (ArrayT t)] IntT
    PrimBuiltinFun    (PrimBuiltinArray (PrimBuiltinIndex t))
     -> FunT [funOfVal (ArrayT t), funOfVal IntT] (SumT ErrorT t)

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
    PrimTime PrimTimeDaysJulianEpoch
     -> FunT [funOfVal TimeT] IntT
    PrimTime PrimTimeSecondsDifference
     -> FunT [funOfVal TimeT, funOfVal TimeT] IntT
    PrimTime PrimTimeSecondsJulianEpoch
     -> FunT [funOfVal TimeT] IntT
    PrimTime PrimTimeMinusSeconds
     -> FunT [funOfVal TimeT, funOfVal IntT] TimeT
    PrimTime PrimTimeMinusDays
     -> FunT [funOfVal TimeT, funOfVal IntT] TimeT
    PrimTime PrimTimeMinusMonths
     -> FunT [funOfVal TimeT, funOfVal IntT] TimeT
    PrimTime PrimTimeProjectDay
     -> FunT [funOfVal TimeT] IntT
    PrimTime PrimTimeProjectMonth
     -> FunT [funOfVal TimeT] IntT
    PrimTime PrimTimeProjectYear
     -> FunT [funOfVal TimeT] IntT

    PrimPair (PrimPairFst a b)
     -> FunT [funOfVal (PairT a b)] a
    PrimPair (PrimPairSnd a b)
     -> FunT [funOfVal (PairT a b)] b

    PrimStruct (PrimStructGet f t (StructType fs))
     -> FunT [funOfVal (StructT $ StructType $ Map.insert f t fs)] t

-- Pretty -------------

instance Pretty PrimArithUnary where
 pretty PrimArithNegate   = "negate#"
 pretty PrimArithAbsolute = "abs#"

instance Pretty PrimArithBinary where
 pretty PrimArithPlus  = "add#"
 pretty PrimArithMinus = "sub#"
 pretty PrimArithMul   = "mul#"

instance Pretty PrimRelation where
 pretty PrimRelationGt = "gt#"
 pretty PrimRelationGe = "ge#"
 pretty PrimRelationLt = "lt#"
 pretty PrimRelationLe = "le#"
 pretty PrimRelationEq = "eq#"
 pretty PrimRelationNe = "ne#"

instance Pretty PrimLogical where
 pretty PrimLogicalNot   = "not#"
 pretty PrimLogicalAnd   = "and#"
 pretty PrimLogicalOr    = "or#"

instance Pretty PrimConst where
 pretty (PrimConstPair _a _b)  = "pair#"
 pretty (PrimConstSome _t)     = "some#"
 pretty (PrimConstLeft  _a _b) = "left#"
 pretty (PrimConstRight _a _b) = "right#"

instance Pretty PrimTime where
 pretty PrimTimeDaysDifference     = "Time_daysDifference#"
 pretty PrimTimeDaysJulianEpoch    = "Time_daysJulianEpoch#"
 pretty PrimTimeSecondsDifference  = "Time_secondsDifference#"
 pretty PrimTimeSecondsJulianEpoch = "Time_secondsJulianEpoch#"
 pretty PrimTimeMinusSeconds       = "Time_minusSeconds#"
 pretty PrimTimeMinusDays          = "Time_minusDays#"
 pretty PrimTimeMinusMonths        = "Time_minusMonths#"
 pretty PrimTimeProjectDay         = "Time_projectDay"
 pretty PrimTimeProjectMonth       = "Time_projectMonth"
 pretty PrimTimeProjectYear        = "Time_projectYear"

instance Pretty PrimPair where
 pretty (PrimPairFst _a _b) = "fst#"
 pretty (PrimPairSnd _a _b) = "snd#"

instance Pretty PrimStruct where
 pretty (PrimStructGet f _t _fs) = "get_" <> pretty f <> "#"

instance Pretty Prim where
 pretty (PrimArithUnary  p _t) = pretty p
 pretty (PrimArithBinary p _t) = pretty p
 pretty (PrimRelation    p _t) = pretty p
 pretty (PrimLogical     p)   = pretty p
 pretty (PrimConst       p)   = pretty p
 pretty (PrimTime        p)   = pretty p
 pretty (PrimPair        p)   = pretty p
 pretty (PrimStruct      p)   = pretty p
 pretty (PrimBuiltinFun  p)   = pretty p
