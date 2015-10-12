{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Prim.Eval (
      evalPrim
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Value
import              Icicle.Common.Exp.Eval
import              Icicle.Common.Exp.Prim.Minimal
import qualified    Icicle.Data.DateTime            as DT

import              P

import qualified    Data.Map                        as Map

import qualified    Data.Text                       as T


-- | Evaluate a primitive, given list of argument values
-- Since this is supposed to be used by another evaluator,
-- the values will not be of the minimal primitive type,
-- and instead be the higher primitive.
--
-- Since values can be closures (though they shouldn't be for
-- these primitives), the values need to be parameterised by
-- the primitive type.
evalPrim    :: Ord n
            => Prim
            -> EvalPrim a n p
evalPrim p originalP vs
 = let primError = Left $ RuntimeErrorPrimBadArgs originalP vs
   in case p of

     PrimArithUnary PrimArithNegate _
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ negate i
      | [VBase (VInt i)] <- vs
      -> return $ VBase $ VInt $ negate i
      | otherwise
      -> primError


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

     PrimArithBinary PrimArithPow _
      | [VBase (VDouble i), VBase (VDouble j)] <- vs
      -> return $ VBase $ VDouble $ i ** j
      | [VBase (VInt i), VBase (VInt j)] <- vs
      -> return $ VBase $ VInt $ i ^ j
      | otherwise
      -> primError


     PrimDouble PrimDoubleDiv
      | [VBase (VDouble i), VBase (VDouble j)] <- vs
      -> return $ VBase $ VDouble $ i / j
      | otherwise
      -> primError
     PrimDouble PrimDoubleLog
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ log i
      | otherwise
      -> primError
     PrimDouble PrimDoubleExp
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VDouble $ exp i
      | otherwise
      -> primError

     PrimCast PrimCastDoubleOfInt
      | [VBase (VInt i)] <- vs
      -> return $ VBase $ VDouble $ fromIntegral i
      | otherwise
      -> primError
     PrimCast PrimCastIntOfDouble
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VInt $ truncate i
      | otherwise
      -> primError
     PrimCast PrimCastStringOfInt
      | [VBase (VInt i)] <- vs
      -> return $ VBase $ VString $ T.pack $ show i
      | otherwise
      -> primError
     PrimCast PrimCastStringOfDouble
      | [VBase (VDouble i)] <- vs
      -> return $ VBase $ VString $ T.pack $ show i
      | otherwise
      -> primError


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


     -- Date stuff
     PrimDateTime PrimDateTimeDaysDifference
      | [VBase (VDateTime a), VBase (VDateTime b)] <- vs
      -> return $ VBase $ VInt $ DT.daysDifference a b
      | otherwise
      -> primError

     PrimDateTime PrimDateTimeDaysEpoch
      | [VBase (VDateTime a)] <- vs
      -> return $ VBase $ VInt $ DT.daysOfDate a
      | otherwise
      -> primError

     PrimDateTime PrimDateTimeMinusDays
      | [VBase (VDateTime a), VBase (VInt b)] <- vs
      -> return $ VBase $ VDateTime $ DT.minusDays a b
      | otherwise
      -> primError

     PrimDateTime PrimDateTimeMinusMonths
      | [VBase (VDateTime a), VBase (VInt b)] <- vs
      -> return $ VBase $ VDateTime $ DT.minusMonths a b
      | otherwise
      -> primError

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

     PrimStruct (PrimStructGet f _ _)
      | [VBase (VStruct fs)] <- vs
      , Just v' <- Map.lookup f fs
      -> return $ VBase v'
      | otherwise
      -> primError

