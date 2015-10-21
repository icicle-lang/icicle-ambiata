-- | Evaluate Flat primitives
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Prim.Eval (
      evalPrim
    ) where

import Icicle.Common.Base
import Icicle.Common.Exp.Eval
import Icicle.Common.Type
import Icicle.Common.Value
import Icicle.Avalanche.Prim.Flat

import              P
import              Data.List (lookup, zip, zipWith)

import qualified    Data.List as List
import qualified    Data.Map  as Map
import qualified    Icicle.Common.Exp.Prim.Eval as Min

evalPrim :: Ord n => EvalPrim a n Prim
evalPrim p vs
 = let primError = Left $ RuntimeErrorPrimBadArgs p vs
   in case p of
     PrimMinimal m
      -> Min.evalPrim m p vs

     PrimProject (PrimProjectArrayLength _)
      | [VBase (VArray var)]    <- vs
      -> return $ VBase $ VInt $ length var
      | otherwise
      -> primError

     PrimProject (PrimProjectMapLength _ _)
      | [VBase (VMap var)]    <- vs
      -> return $ VBase $ VInt $ Map.size var
      | otherwise
      -> primError

     PrimProject (PrimProjectMapLookup _ _)
      | [VBase (VMap var), VBase vk]  <- vs
      -> return $ VBase
       $ case Map.lookup vk var of
          Nothing -> VNone
          Just v' -> VSome v'
      | otherwise
      -> primError

     PrimProject (PrimProjectOptionIsSome _)
      | [VBase VNone]  <- vs
      -> return $ VBase $ VBool False
      | [VBase (VSome _)]  <- vs
      -> return $ VBase $ VBool True
      | otherwise
      -> primError

     PrimProject (PrimProjectSumIsRight _ _)
      | [VBase (VLeft _)]  <- vs
      -> return $ VBase $ VBool False
      | [VBase (VRight _)]  <- vs
      -> return $ VBase $ VBool True
      | otherwise
      -> primError

     PrimBuf (PrimBufMake _)
      | [VBase (VInt i)] <- vs
      -> return . VBase . VBuf i $ []
      | otherwise
      -> primError

     PrimBuf (PrimBufPush _)
      | [VBase (VBuf i as), VBase e] <- vs
      -> return . VBase . VBuf i
      $  circ i e as
      | otherwise
      -> primError

     PrimBuf (PrimBufRead _)
      | [VBase (VBuf _ as)] <- vs
      -> return . VBase . VArray $ as
      | otherwise
      -> primError

     -- TODO: give better errors here - at least that an unsafe went wrong
     PrimUnsafe (PrimUnsafeArrayIndex _)
      | [VBase (VArray var), VBase (VInt ix)]  <- vs
      , Just v <- lookup ix (zip [0..] var)
      -> return $ VBase $ v
      | otherwise
      -> primError

     -- Create an uninitialised array.
     -- We have nothing to put in there, so we might as well just
     -- fill it up with units.
     PrimUnsafe (PrimUnsafeArrayCreate _)
      | [VBase (VInt sz)]  <- vs
      -> return $ VBase $ VArray $ [VUnit | _ <- [1..sz]]
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeMapIndex _ _)
      | [VBase (VMap var), VBase (VInt ix)]  <- vs
      , Just (k,v) <- lookup ix (zip [0..] $ Map.toList var)
      -> return $ VBase $ VPair k v
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeOptionGet t)
      | [VBase (VSome v)]  <- vs
      -> return $ VBase v
      | [VBase VNone]      <- vs
      -> return $ VBase (defaultOfType t)
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeSumGetLeft t _)
      | [VBase (VLeft v)]  <- vs
      -> return $ VBase v
      | [VBase (VRight _)] <- vs
      -> return $ VBase (defaultOfType t)
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeSumGetRight _ t)
      | [VBase (VRight v)] <- vs
      -> return $ VBase v
      | [VBase (VLeft _)]  <- vs
      -> return $ VBase (defaultOfType t)
      | otherwise
      -> primError


     PrimUpdate (PrimUpdateMapPut _ _)
      | [VBase (VMap vmap), VBase k, VBase v]  <- vs
      -> return $ VBase $ VMap $ Map.insert k v vmap
      | otherwise
      -> primError

     PrimUpdate (PrimUpdateArrayPut _)
      | [VBase (VArray varr), VBase (VInt ix), VBase v]  <- vs
      -> return $ VBase
       $ VArray [ if i == ix then v else u
                | (i,u) <- zip [0..] varr ]
      | otherwise
      -> primError


     PrimArray (PrimArrayZip _ _)
      | [VBase (VArray arr1), VBase (VArray arr2)]  <- vs
      -> return $ VBase
       $ VArray (zipWith VPair arr1 arr2)
      | otherwise
      -> primError

     PrimPack (PrimOptionPack _)
      | [VBase (VBool False), _]       <- vs
      -> return $ VBase $ VNone
      | [VBase (VBool True), VBase v]  <- vs
      -> return $ VBase $ VSome v
      | otherwise
      -> primError

     PrimPack (PrimSumPack _ _)
      | [VBase (VBool False), VBase a, _]       <- vs
      -> return $ VBase $ VLeft a
      | [VBase (VBool True), _, VBase b]  <- vs
      -> return $ VBase $ VRight b
      | otherwise
      -> primError
 where
  circ n x xs
   | length xs < n
   = xs <> [x]
   | otherwise
   = List.drop 1 (xs <> [x])
