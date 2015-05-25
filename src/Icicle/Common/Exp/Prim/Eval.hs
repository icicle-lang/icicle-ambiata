{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Prim.Eval (
      evalPrim
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Value
import              Icicle.Common.Exp.Eval
import              Icicle.Common.Exp.Prim.Minimal
import qualified    Icicle.Data.DateTime as DT

import              P


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
            -> EvalPrim n p
evalPrim p originalP vs
 = let primError = Left $ RuntimeErrorPrimBadArgs originalP vs
   in case p of

     PrimArith PrimArithPlus
      | [VBase (VInt i), VBase (VInt j)] <- vs
      -> return $ VBase $ VInt $ i + j
      | otherwise
      -> primError

     PrimArith PrimArithMinus
      | [VBase (VInt i), VBase (VInt j)] <- vs
      -> return $ VBase $ VInt $ i - j
      | otherwise
      -> primError

     PrimArith PrimArithDiv
      | [VBase (VInt i), VBase (VInt j)] <- vs
      -> return $ VBase $ VInt $ i `div` j
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


     -- Date stuff
     PrimDateTime PrimDateTimeDaysDifference
      | [VBase (VDateTime a), VBase (VDateTime b)] <- vs
      -> return $ VBase $ VInt $ DT.daysDifference a b
      | otherwise
      -> primError

