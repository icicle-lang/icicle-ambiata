{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp.Constructor (
    constructor
  ) where

import              Icicle.Avalanche.Prim.Flat
import qualified    Icicle.Common.Exp.Prim.Minimal as Min
import              Icicle.Avalanche.Statement.Statement
import              Icicle.Avalanche.Statement.Simp.ExpEnv

import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              P


constructor :: Ord n => a -> Statement a n Prim -> Fresh n (Statement a n Prim)
constructor a_fresh statements
 = transformUDStmt goS emptyExpEnv statements
 where
  xPrim = XPrim a_fresh
  xApp  = XApp  a_fresh

  goS env s
   = let env' = updateExpEnv s env
         go   = goX env
         ret s' = return (env', s')
     in  case s of
          If x t e
           -> ret $ If (go x) t e
          Let n x ss
           -> ret $ Let n (go x) ss
          ForeachInts n from to ss
           -> ret $ ForeachInts n (go from) (go to) ss
          InitAccumulator (Accumulator n at vt x) ss
           -> ret $ InitAccumulator (Accumulator n at vt (go x)) ss
          Write n x
           -> ret $ Write n (go x)
          Push n x
           -> ret $ Push n (go x)
          Output n x
           -> ret $ Output n (go x)
          _
           -> ret s

  goX env x
   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairFst _ _)), [XVar _ n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (_,_,a,_) <- pair x'
   = a

   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairSnd _ _)), [XVar _ n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (_,_,_,b) <- pair x'
   = b

   | Just (PrimUnsafe (PrimUnsafeArrayIndex _), [XVar _ n, ix]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, tb, a, b) <- zippedArray x'
   = xPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta tb)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex ta)) `xApp` a `xApp` ix)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex tb)) `xApp` b `xApp` ix)

   | Just (PrimProject (PrimProjectArrayLength _), [XVar _ n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, _, a, _) <- zippedArray x'
   = xPrim (PrimProject $ PrimProjectArrayLength ta)
    `xApp` a

   | otherwise
   = x

  zippedArray x
   | Just (PrimArray (PrimArrayZip ta tb), [a, b]) <- takePrimApps x
   = Just (ta, tb, a, b)
   | otherwise
   = Nothing

  pair x
   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstPair ta tb)), [a,b]) <- takePrimApps x
   = Just (ta,tb,a,b)
   | otherwise
   = Nothing

  get env n
   | (_,x'):_ <- filter ((==n).fst) env
   = Just x'
   | otherwise
   = Nothing
