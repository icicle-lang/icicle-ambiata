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


constructor :: Ord n => Statement n Prim -> Fresh n (Statement n Prim)
constructor statements
 = transformUDStmt goS emptyExpEnv statements
 where
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
   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairFst _ _)), [XVar n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (_,_,a,_) <- pair x'
   = a

   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairSnd _ _)), [XVar n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (_,_,_,b) <- pair x'
   = b

   | Just (PrimUnsafe (PrimUnsafeArrayIndex _), [XVar n, ix]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, tb, a, b) <- zippedArray x'
   = XPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta tb)
    `XApp` (XPrim (PrimUnsafe (PrimUnsafeArrayIndex ta)) `XApp` a `XApp` ix)
    `XApp` (XPrim (PrimUnsafe (PrimUnsafeArrayIndex tb)) `XApp` b `XApp` ix)

   | Just (PrimProject (PrimProjectArrayLength _), [XVar n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, _, a, _) <- zippedArray x'
   = XPrim (PrimProject $ PrimProjectArrayLength ta)
    `XApp` a

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
