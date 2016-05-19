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
    , PrimBuiltinArray(..)
    , ArithType       (..)
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
 | PrimTimeSecondsDifference
 | PrimTimeDaysJulianEpoch
 | PrimTimeSecondsJulianEpoch
 | PrimTimeMinusDays
 | PrimTimeMinusSeconds
 | PrimTimeMinusMonths
 | PrimTimeProjectDay
 | PrimTimeProjectMonth
 | PrimTimeProjectYear
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

    PrimBuiltinFun    (PrimBuiltinArray (PrimBuiltinSort t))
     -> FunT [funOfVal (ArrayT t)] (ArrayT t)
    PrimBuiltinFun    (PrimBuiltinArray (PrimBuiltinLength t))
     -> FunT [funOfVal (ArrayT t)] IntT
    PrimBuiltinFun    (PrimBuiltinArray (PrimBuiltinIndex t))
     -> FunT [funOfVal (ArrayT t), funOfVal IntT] t

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
 pretty PrimArithPow   = "pow#"

instance Pretty PrimRelation where
 pretty PrimRelationGt = "gt#"
 pretty PrimRelationGe = "ge#"
 pretty PrimRelationLt = "lt#"
 pretty PrimRelationLe = "le#"
 pretty PrimRelationEq = "eq#"
 pretty PrimRelationNe = "ne#"

instance Pretty PrimToString where
 pretty PrimToStringFromInt    = "stringOfInt#"
 pretty PrimToStringFromDouble = "stringOfDouble#"

instance Pretty PrimLogical where
 pretty PrimLogicalNot   = "not#"
 pretty PrimLogicalAnd   = "and#"
 pretty PrimLogicalOr    = "or#"

instance Pretty PrimConst where
 pretty (PrimConstPair a b)  = annotateTypeArgs [a,b] "pair#"
 pretty (PrimConstSome t)    = annotate (AnnType t)   "some#"
 pretty (PrimConstLeft  a b) = annotateTypeArgs [a,b] "left#"
 pretty (PrimConstRight a b) = annotateTypeArgs [a,b] "right#"

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
 pretty (PrimPairFst a b) = annotateTypeArgs [a,b] "fst#"
 pretty (PrimPairSnd a b) = annotateTypeArgs [a,b] "snd#"

instance Pretty PrimStruct where
 pretty (PrimStructGet f t fs) = annotateTypeArgs [pretty f, pretty t, pretty fs] "get#"

instance Pretty Prim where
 pretty (PrimArithUnary  p t) = annotateArithType t  $ pretty p
 pretty (PrimArithBinary p t) = annotateArithType t  $ pretty p
 pretty (PrimRelation    p t) = annotateTypeArgs [t] $ pretty p
 pretty (PrimToString    p)   = pretty p
 pretty (PrimLogical     p)   = pretty p
 pretty (PrimConst       p)   = pretty p
 pretty (PrimTime        p)   = pretty p
 pretty (PrimPair        p)   = pretty p
 pretty (PrimStruct      p)   = pretty p
 pretty (PrimBuiltinFun  p)   = pretty p

annotateArithType :: ArithType -> Doc -> Doc
annotateArithType t = annotate (AnnType (valTypeOfArithType t))
