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
 = case p of
     PrimMinimal m
      -> Min.evalPrim m p vs

     PrimProject (PrimProjectArrayLength _)
      | [VBase (VArray var)]    <- vs
      -> return $ VBase $ VInt $ length var
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


     PrimUpdate (PrimUpdateArrayPut _)
      | [VBase (VArray varr), VBase (VInt ix), VBase v]  <- vs
      -> return $ VBase
       $ VArray [ if i == ix then v else u
                | (i,u) <- zip [0..] varr ]
      | otherwise
      -> primError

     PrimUpdate (PrimUpdateArrayPut2 _ _)
      | [VBase (VArray varr1), VBase (VArray varr2), VBase (VInt ix), VBase v1, VBase v2]  <- vs
      -> return . VBase $ VPair
         (VArray [ if i == ix then v1 else u | (i,u) <- zip [0..] varr1 ])
         (VArray [ if i == ix then v2 else u | (i,u) <- zip [0..] varr2 ])
      | otherwise
      -> primError


     PrimArray (PrimArrayZip _ _)
      | [VBase (VArray arr1), VBase (VArray arr2)]  <- vs
      -> return $ VBase
       $ VArray (zipWith VPair arr1 arr2)
      | otherwise
      -> primError

     PrimArray (PrimArrayUnzip _ _)
      | [VBase (VArray arr)] <- vs
      -> do (arr1, arr2)     <- foldM uz mempty arr
            return . VBase $ VPair (VArray $ reverse arr1)
                                   (VArray $ reverse arr2)
      | otherwise
      -> primError

     PrimArray (PrimArraySum _ _)
      | [VBase (VArray arr), VBase (VArray arr1), VBase (VArray arr2)]  <- vs
      -> do arr' <- zipWithM pick arr (zip arr1 arr2)
            return . VBase . VArray $ arr'
      | otherwise
      -> primError

     PrimArray (PrimArrayUnsum _ _)
      | [VBase (VArray arr)]   <- vs
      -> do (arr0, arr1, arr2) <- foldM us mempty arr
            return . VBase $ VPair (VArray $ reverse arr0)
                                   (VPair (VArray $ reverse arr1)
                                           (VArray $ reverse arr2))
      | otherwise
      -> primError


     PrimPack (PrimOptionPack _)
      | [VBase (VBool False), _]      <- vs
      -> return $ VBase $ VNone
      | [VBase (VBool True), VBase v] <- vs
      -> return $ VBase $ VSome v
      | otherwise
      -> primError

     PrimPack (PrimSumPack _ _)
      | [VBase (VBool False), VBase a, _] <- vs
      -> return $ VBase $ VLeft a
      | [VBase (VBool True), _, VBase b]  <- vs
      -> return $ VBase $ VRight b
      | otherwise
      -> primError

     PrimPack (PrimStructPack (StructType fts))
      | Just vs' <- traverse unpack vs
      , fs       <- Map.keys fts
      -> return $ VBase $ VStruct $ Map.fromList $ List.zip fs vs'
      | otherwise
      -> primError

     PrimMap (PrimMapPack _ _)
      | [VBase (VArray ks), VBase (VArray vals)] <- vs
      -> return $ VBase $ VMap $ Map.fromList $ List.zip ks vals
      | otherwise
      -> primError
     PrimMap (PrimMapUnpackKeys _ _)
      | [VBase (VMap m)] <- vs
      -> return $ VBase $ VArray $ Map.keys m
      | otherwise
      -> primError
     PrimMap (PrimMapUnpackValues _ _)
      | [VBase (VMap m)] <- vs
      -> return $ VBase $ VArray $ Map.elems m
      | otherwise
      -> primError
 where
  circ n x xs
   | length xs < n
   = xs <> [x]
   | otherwise
   = List.drop 1 (xs <> [x])

  uz (fs, ss) (VPair f s)
   = return (f     : fs, s     : ss)
  uz (fs, ss) VUnit -- unitialised array
   = return (VUnit : fs, VUnit : ss)
  uz _ _
   = primError

  us (bs, ls, rs) (VLeft l)
   = return ( VBool  False  : bs
            , l             : ls
            , VUnit         : rs )
  us (bs, ls, rs) (VRight r)
   = return ( VBool  True  : bs
            , VUnit        : ls
            , r            : rs )
  us (bs, ls, rs) VUnit -- uninitialised array
   = return (VUnit : bs, VUnit : ls, VUnit : rs)
  us _ _
   = primError

  primError
   = Left $ RuntimeErrorPrimBadArgs p vs

  pick (VBool b) (l,r)
   = return $ if b then VRight r else VLeft l
  pick  VUnit _ -- uninitialised array
   = return VUnit
  pick _ _
   = primError

  unpack (VBase x)    = Just x
  unpack (VFun _ _ _) = Nothing
