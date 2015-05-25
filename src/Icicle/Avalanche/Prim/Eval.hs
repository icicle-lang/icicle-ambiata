-- | Evaluate Flat primitives
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Prim.Eval (
      evalPrim
    ) where

import Icicle.Common.Base
import Icicle.Common.Value
import Icicle.Common.Exp.Eval
import Icicle.Avalanche.Prim.Flat

import              P
import              Data.List (lookup, zip)

import qualified    Data.Map as Map
import qualified    Icicle.Common.Exp.Prim.Eval as Min

evalPrim :: Ord n => EvalPrim n Prim
evalPrim p vs
 = let primError = Left $ RuntimeErrorPrimBadArgs p vs
   in case p of
     PrimMinimal m
      -> Min.evalPrim m p vs

     PrimProject (PrimProjectPair which _ _)
      | [VBase (VPair va vb)] <- vs
      -> if   which
         then return $ VBase vb
         else return $ VBase va
      | otherwise
      -> primError

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


     -- TODO: give better errors here - at least that an unsafe went wrong
     PrimUnsafe (PrimUnsafeArrayIndex _)
      | [VBase (VArray var), VBase (VInt ix)]  <- vs
      , Just v <- lookup ix (zip [0..] var)
      -> return $ VBase $ v
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeMapIndex _ _)
      | [VBase (VMap var), VBase (VInt ix)]  <- vs
      , Just (k,v) <- lookup ix (zip [0..] $ Map.toList var)
      -> return $ VBase $ VPair k v
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeOptionGet _)
      | [VBase (VSome v)]  <- vs
      -> return $ VBase v
      | otherwise
      -> primError

     PrimUpdate (PrimUpdateMapPut _ _)
      | [VBase (VMap vmap), VBase k, VBase v]  <- vs
      -> return $ VBase $ VMap $ Map.insert k v vmap
      | otherwise
      -> primError







