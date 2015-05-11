-- | This is a very simple expression evaluator, the idea being to serve as a spec
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Exp (
      evalPrim
    ) where

import Icicle.Common.Base
import Icicle.Common.Value
import Icicle.Common.Exp.Eval
import Icicle.Core.Exp.Prim

import              P

import qualified    Data.Map as Map


-- | Evaluate a primitive, given list of argument values
evalPrim :: Ord n => EvalPrim n Prim
evalPrim p vs
 = let primError = Left $ RuntimeErrorPrimBadArgs p vs
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


     PrimRelation rel
      | [VBase (VInt i), VBase (VInt j)] <- vs
      -> return $ VBase $ VBool
       $ case rel of
          PrimRelationGt -> i >  j
          PrimRelationGe -> i >= j
          PrimRelationLt -> i <  j
          PrimRelationLe -> i <  j
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


     PrimConst (PrimConstBool b)
      | [] <- vs
      -> return $ VBase $ VBool b
      | otherwise
      -> primError

     PrimConst (PrimConstInt i)
      | [] <- vs
      -> return $ VBase $ VInt i
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

     PrimConst (PrimConstNone _)
      | [] <- vs
      -> return $ VBase $ VNone
      | otherwise
      -> primError

     PrimConst (PrimConstArrayEmpty _)
      | [] <- vs
      -> return $ VBase $ VArray []
      | otherwise
      -> primError

     PrimConst (PrimConstMapEmpty _ _)
      | [] <- vs
      -> return $ VBase $ VMap Map.empty
      | otherwise
      -> primError


     -- Folds and destructions
     PrimFold PrimFoldBool _
      | [t, _, VBase (VBool True)] <- vs
      -> return t
      | [_, f, VBase (VBool False)] <- vs
      -> return f
      | otherwise
      -> primError

     PrimFold (PrimFoldPair _ _) _
      | [f,VBase v] <- vs
      , VPair a b   <- v
      -> applies f [VBase a, VBase b]
      | otherwise
      -> primError

     PrimFold (PrimFoldArray _) _
      | [k, z, VBase (VArray as)] <- vs
      -> foldM (\a c -> applies k [a,VBase c]) z as
      | otherwise
      -> primError

     PrimFold (PrimFoldOption _) _
      | [s, _, VBase (VSome v)] <- vs
      -> applies s [VBase v]
      | [_, n, VBase (VNone)] <- vs
      -> return n
      | otherwise
      -> primError

     PrimFold (PrimFoldMap _ _) _
      | [k, z, VBase (VMap mm)] <- vs
      -> foldM (\a (b,c) -> applies k [a,VBase b,VBase c]) z
       $ Map.toList mm
      | otherwise
      -> primError

     PrimMap (PrimMapInsertOrUpdate _ _)
      | [upd, VBase ins, VBase key, VBase (VMap mm)] <- vs
      -> case Map.lookup key mm of
          Nothing
           -> return $ VBase $ VMap $ Map.insert key ins mm
          Just v
           -> do    v' <- applyValues evalPrim upd (VBase v)
                    case v' of
                     VBase v'' -> return $ VBase $ VMap $ Map.insert key v'' mm
                     VFun{}    -> primError

      | otherwise
      -> primError

 where
  applies :: Ord n => Value n Prim -> [Value n Prim] -> Either (RuntimeError n Prim) (Value n Prim)
  applies = foldM (applyValues evalPrim)

