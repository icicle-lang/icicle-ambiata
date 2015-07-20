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
          Return x
           -> ret $ Return (go x)
          _
           -> ret s

  goX env x
   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairFst _ _)), [XVar n]) <- takePrimApps x
   , (_,x'):_ <- filter ((==n).fst) env
   , Just (PrimMinimal (Min.PrimConst (Min.PrimConstPair _ _)), [a,_]) <- takePrimApps x'
   = a

   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairSnd _ _)), [XVar n]) <- takePrimApps x
   , (_,x'):_ <- filter ((==n).fst) env
   , Just (PrimMinimal (Min.PrimConst (Min.PrimConstPair _ _)), [_,b]) <- takePrimApps x'
   = b

   | otherwise
   = x

