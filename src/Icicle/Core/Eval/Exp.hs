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

import qualified    Data.Map  as Map
import qualified    Data.List as List
import qualified    Icicle.Common.Exp.Prim.Eval as Min


-- | Evaluate a primitive, given list of argument values
evalPrim :: Ord n => EvalPrim a n Prim
evalPrim p vs
 = case p of
     PrimMinimal m
      -> Min.evalPrim m p vs

     -- Folds and destructions
     PrimFold PrimFoldBool _
      | [t, _, VBase (VBool True)] <- vs
      -> return t
      | [_, f, VBase (VBool False)] <- vs
      -> return f
      | otherwise
      -> primError

     PrimFold (PrimFoldArray _) _
      | [k, z, VBase (VArray as)] <- vs
      -> foldM (\a c -> applies' k [a,VBase c]) z as
      | otherwise
      -> primError

     PrimFold (PrimFoldOption _) _
      | [s, _, VBase (VSome v)] <- vs
      -> applies' s [VBase v]
      | [_, n, VBase (VNone)] <- vs
      -> return n
      | otherwise
      -> primError

     PrimFold (PrimFoldSum _ _) _
      | [s, _, VBase (VLeft v)] <- vs
      -> applies' s [VBase v]
      | [_, s, VBase (VRight v)] <- vs
      -> applies' s [VBase v]
      | otherwise
      -> primError

     PrimFold (PrimFoldMap _ _) _
      | [k, z, VBase (VMap mm)] <- vs
      -> foldM (\a (b,c) -> applies' k [a,VBase b,VBase c]) z
       $ Map.toList mm
      | otherwise
      -> primError

     PrimArray (PrimArrayMap _ _)
      | [upd, VBase (VArray as)] <- vs
      -> (VBase . VArray) <$> mapM (applyBase upd) as
      | otherwise
      -> primError

     PrimMap (PrimMapInsertOrUpdate _ _)
      | [upd, VBase ins, VBase key, VBase (VMap mm)] <- vs
      -> case Map.lookup key mm of
          Nothing
           -> return $ VBase $ VMap $ Map.insert key ins mm
          Just v
           -> do    v' <- applyBase upd v
                    return $ VBase $ VMap $ Map.insert key v' mm

      | otherwise
      -> primError

     PrimMap (PrimMapMapValues _ _ _)
      | [upd, VBase (VMap mm)] <- vs
      -> (VBase . VMap) <$> mapM (applyBase upd) mm
      | otherwise
      -> primError

     PrimLatest (PrimLatestMake _)
      | [VBase (VInt i)] <- vs
      -> return . VBase . VBuf
      $  List.replicate i (VError ExceptScalarVariableNotAvailable)
      | otherwise
      -> primError

     PrimLatest (PrimLatestPush _)
      | [VBase (VBuf as), VBase e] <- vs
      -> return . VBase . VBuf
      $  circ e as
      | otherwise
      -> primError

     PrimLatest (PrimLatestRead _)
      | [VBase (VBuf as)] <- vs
      -> return . VBase . VArray
      $ filter justElem as
      | otherwise
      -> primError

 where
  applies' = applies evalPrim

  applyBase f v
   = do v' <- applyValues evalPrim f (VBase v)
        case v' of
          VBase v'' -> return v''
          VFun{}    -> primError

  primError
   = Left $ RuntimeErrorPrimBadArgs p vs

  -- TODO make an efficient circular buffer
  circ x xs
   = List.drop 1 (xs <> [x])

  -- If the circular buffer is not completely filled,
  -- just grab the filled part.
  justElem (VError ExceptScalarVariableNotAvailable)
   = False
  justElem _
   = True
