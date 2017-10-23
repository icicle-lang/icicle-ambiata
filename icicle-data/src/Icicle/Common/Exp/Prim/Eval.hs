{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Prim.Eval (
      evalPrim
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Value
import              Icicle.Common.Type
import              Icicle.Common.Exp.Eval
import              Icicle.Common.Exp.Prim.Minimal
import qualified    Icicle.Data.Time                as Time

import              P

import qualified    Data.Map                        as Map
import qualified    Data.List                       as List


-- | Evaluate a primitive, given list of argument values
-- Since this is supposed to be used by another evaluator,
-- the values will not be of the minimal primitive type,
-- and instead be the higher primitive.
--
-- Since values can be closures (though they shouldn't be for
-- these primitives), the values need to be parameterised by
-- the primitive type.
evalPrim    :: Prim
            -> EvalPrim a n p
evalPrim p originalP vs
 = let primError = Left $ RuntimeErrorPrimBadArgs originalP vs
   in case p of

     -- Unary
     PrimArithUnary PrimArithNegate _
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ negate i
      | [VBase (VInt i)] <- vs
      -> return $ VBase $ VInt $ negate i
      | otherwise
      -> primError

     PrimArithUnary PrimArithAbsolute _
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ abs i
      | [VBase (VInt i)] <- vs
      -> return $ VBase $ VInt $ abs i
      | otherwise
      -> primError


     -- Binary
     PrimArithBinary PrimArithPlus _
      | [VBase (VDouble i), VBase (VDouble j)] <- vs
      -> return $ VBase $ VDouble $ i + j
      | [VBase (VInt i), VBase (VInt j)] <- vs
      -> return $ VBase $ VInt $ i + j
      | otherwise
      -> primError

     PrimArithBinary PrimArithMinus _
      | [VBase (VDouble i), VBase (VDouble j)] <- vs
      -> return $ VBase $ VDouble $ i - j
      | [VBase (VInt i), VBase (VInt j)] <- vs
      -> return $ VBase $ VInt $ i - j
      | otherwise
      -> primError

     PrimArithBinary PrimArithMul _
      | [VBase (VDouble i), VBase (VDouble j)] <- vs
      -> return $ VBase $ VDouble $ i * j
      | [VBase (VInt i), VBase (VInt j)] <- vs
      -> return $ VBase $ VInt $ i * j
      | otherwise
      -> primError

     -- Builtin
     PrimBuiltinFun (PrimBuiltinMath PrimBuiltinDiv)
      | [VBase (VDouble i), VBase (VDouble j)] <- vs
      -> return $ VBase $ VDouble $ i / j
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath PrimBuiltinPow)
      | [VBase (VDouble i), VBase (VDouble j)] <- vs
      -> return $ VBase $ VDouble $ i ** j
      | otherwise
      -> primError


     PrimBuiltinFun (PrimBuiltinMath PrimBuiltinLog)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ log i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath PrimBuiltinExp)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ exp i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath PrimBuiltinSqrt)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ sqrt i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinFloor)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VInt $ floor i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinCeiling)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VInt $ ceiling i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinRound)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VInt $ round i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinTruncate)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VInt $ truncate i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath PrimBuiltinToDoubleFromInt)
      | [VBase (VInt i)] <- vs
      -> return $ VBase $ VDouble $ fromIntegral i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinDoubleIsValid)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VBool $ not (isNaN i || isInfinite i)
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinAcos)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ acos i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinAsin)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ asin i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinAtan)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ atan i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinAtan2)
      | [VBase (VDouble i), VBase (VDouble j)] <- vs
      -> return $ VBase $ VDouble $ atan2 i j
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinCos)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ cos i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinCosh)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ cosh i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinSin)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ sin i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinSinh)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ sinh i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinTan)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ tan i
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMath  PrimBuiltinTanh)
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ tanh i
      | otherwise
      -> primError


     PrimBuiltinFun (PrimBuiltinMap (PrimBuiltinKeys _ _))
      | [VBase (VMap m)] <- vs
      -> return $ VBase $ VArray $ Map.keys m
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinMap (PrimBuiltinVals _ _))
      | [VBase (VMap m)] <- vs
      -> return $ VBase $ VArray $ Map.elems m
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinArray (PrimBuiltinSort _))
      | [VBase (VArray a)] <- vs
      -> return $ VBase $ VArray $ List.sort a
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinArray (PrimBuiltinLength _))
      | [VBase (VArray a)] <- vs
      -> return $ VBase $ VInt $ List.length a
      | otherwise
      -> primError

     PrimBuiltinFun (PrimBuiltinArray (PrimBuiltinIndex _))
      | [VBase (VArray a), VBase (VInt i)] <- vs
      -> if i >= 0 && i < length a
         then return $ VBase $ VRight $ a List.!! i
         else return $ VBase $ VLeft  $ VError ExceptIndexOutOfBounds
      | otherwise
      -> primError

     -- Relation
     PrimRelation rel _
      -- It is safe to assume they are of the same value type
      -- and "ordable", if we assume that it typechecks.
      -- So we should be able to rely on the BaseValue Ord instance
      -- without unwrapping the different types
      | [VBase i, VBase j] <- vs
      -> return $ VBase $ VBool
       $ case rel of
          PrimRelationGt -> i >  j
          PrimRelationGe -> i >= j
          PrimRelationLt -> i <  j
          PrimRelationLe -> i <= j
          PrimRelationEq -> i == j
          PrimRelationNe -> i /= j
      | otherwise
      -> primError


     -- Logical
     PrimLogical  PrimLogicalNot
      | [VBase (VBool u)] <- vs
      -> return $ VBase $ VBool $ not u
      | otherwise
      -> primError

     PrimLogical  PrimLogicalAnd
      | [VBase (VBool u), VBase (VBool v)] <- vs
      -> return $ VBase $ VBool $ u && v
      | otherwise
      -> primError

     PrimLogical  PrimLogicalOr
      | [VBase (VBool u), VBase (VBool v)] <- vs
      -> return $ VBase $ VBool $ u || v
      | otherwise
      -> primError


     -- Constructors
     PrimConst (PrimConstPair _ _)
      | [VBase x,VBase y] <- vs
      -> return $ VBase $ VPair x y
      | otherwise
      -> primError

     PrimConst (PrimConstSome _)
      | [VBase v] <- vs
      -> return $ VBase $ VSome v
      | otherwise
      -> primError

     PrimConst (PrimConstLeft _ _)
      | [VBase v] <- vs
      -> return $ VBase $ VLeft v
      | otherwise
      -> primError

     PrimConst (PrimConstRight _ _)
      | [VBase v] <- vs
      -> return $ VBase $ VRight v
      | otherwise
      -> primError


     -- Time stuff
     PrimTime PrimTimeDaysDifference
      | [VBase (VTime a), VBase (VTime b)] <- vs
      -> return $ VBase $ VInt $ Time.daysDifference a b
      | otherwise
      -> primError

     PrimTime PrimTimeDaysJulianEpoch
      | [VBase (VTime a)] <- vs
      -> return $ VBase $ VInt $ Time.daysCountIvory a
      | otherwise
      -> primError

     PrimTime PrimTimeSecondsDifference
      | [VBase (VTime a), VBase (VTime b)] <- vs
      -> return $ VBase $ VInt $ Time.secondsDifference a b
      | otherwise
      -> primError

     PrimTime PrimTimeSecondsJulianEpoch
      | [VBase (VTime a)] <- vs
      -> return $ VBase $ VInt $ Time.secondsCountIvory a
      | otherwise
      -> primError

     PrimTime PrimTimeMinusSeconds
      | [VBase (VTime a), VBase (VInt b)] <- vs
      -> return $ VBase $ VTime $ Time.minusSeconds a b
      | otherwise
      -> primError

     PrimTime PrimTimeMinusDays
      | [VBase (VTime a), VBase (VInt b)] <- vs
      -> return $ VBase $ VTime $ Time.minusDays a b
      | otherwise
      -> primError

     PrimTime PrimTimeMinusMonths
      | [VBase (VTime a), VBase (VInt b)] <- vs
      -> return $ VBase $ VTime $ Time.minusMonths a b
      | otherwise
      -> primError

     PrimTime PrimTimeProjectDay
      | [VBase (VTime a)] <- vs
      -> return $ VBase $ VInt $ Time.dayOf a
      | otherwise
      -> primError

     PrimTime PrimTimeProjectMonth
      | [VBase (VTime a)] <- vs
      -> return $ VBase $ VInt $ Time.monthOf a
      | otherwise
      -> primError

     PrimTime PrimTimeProjectYear
      | [VBase (VTime a)] <- vs
      -> return $ VBase $ VInt $ Time.yearOf a
      | otherwise
      -> primError


     -- Projections
     PrimPair (PrimPairFst _ _)
      | [VBase (VPair x _)] <- vs
      -> return $ VBase $ x
      | otherwise
      -> primError

     PrimPair (PrimPairSnd _ _)
      | [VBase (VPair _ y)] <- vs
      -> return $ VBase $ y
      | otherwise
      -> primError


     -- Struct
     PrimStruct (PrimStructGet f (OptionT _) _)
      | [VBase (VStruct fs)] <- vs
      , Just v' <- Map.lookup f fs
      -> return $ VBase v'
      | otherwise
      -> return $ VBase $ VNone

     PrimStruct (PrimStructGet f _ _)
      | [VBase (VStruct fs)] <- vs
      , Just v' <- Map.lookup f fs
      -> return $ VBase v'
      | otherwise
      -> primError
