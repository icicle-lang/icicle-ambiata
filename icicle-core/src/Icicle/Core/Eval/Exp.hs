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
import Icicle.Data.Time
import qualified    Icicle.Common.Exp.Prim.Eval as Min

import              P

import qualified    Data.Map  as Map
import qualified    Data.List as List
import              Data.Hashable (Hashable)


-- | Evaluate a primitive, given list of argument values
evalPrim :: (Hashable n, Eq n) => EvalPrim a n Prim
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
      -> applies' n [VBase VUnit]
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

     PrimMap (PrimMapDelete _ _)
      | [VBase key, VBase (VMap mm)] <- vs
      -> return $ VBase $ VMap $ Map.delete key mm
      | otherwise
      -> primError

     PrimMap (PrimMapMapValues _ _ _)
      | [upd, VBase (VMap mm)] <- vs
      -> (VBase . VMap) <$> mapM (applyBase upd) mm
      | otherwise
      -> primError

     PrimMap (PrimMapLookup _ _)
      | [VBase (VMap mm), VBase key] <- vs
      -> case Map.lookup key mm of
          Nothing
           -> return $ VBase $ VNone
          Just v
           -> return $ VBase $ VSome v

      | otherwise
      -> primError


     PrimLatest (PrimLatestPush i _)
      | [VBase (VBuf as), VBase factid, VBase e] <- vs
      -> return . VBase . VBuf
      $  circ i (VPair factid e) as
      | otherwise
      -> primError

     PrimLatest (PrimLatestRead _ _)
      | [VBase (VBuf as)] <- vs
      -> return . VBase . VArray $ fmap getSnd as
      | otherwise
      -> primError

     PrimWindow newerThan olderThan
      | [VBase (VTime now), VBase (VTime fact), _] <- vs
      -> let newer = windowEdge now     newerThan 
             older = windowEdge now <$> olderThan
             range = fact >= newer && maybe True (fact <=) older
         in  return . VBase . VBool $ range
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

  circ n x xs
   | length xs < n
   = xs <> [x]
   | otherwise
   = List.drop 1 (xs <> [x])

  getSnd (VPair _ b) = b
  getSnd v           = v

  windowEdge now (Days   d) = minusDays   now d
  windowEdge now (Weeks  w) = minusDays   now $ 7 * w
  windowEdge now (Months m) = minusMonths now m

