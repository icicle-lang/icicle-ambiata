{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp.Melt (
    melt
  ) where

import              Icicle.Avalanche.Prim.Flat
import qualified    Icicle.Common.Exp.Prim.Minimal as Min
import              Icicle.Avalanche.Statement.Statement
import              Icicle.Avalanche.Statement.Simp

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Fresh
import              Icicle.Common.Type

import              P

import qualified    Data.Map            as Map


melt :: (Show n, Ord n)
     => a
     -> Statement a n Prim
     -> Fresh n (Statement a n Prim)
melt a_fresh statements
 = transformUDStmt goS Map.empty statements
 where
  xVar   = XVar   a_fresh
  xPrim  = XPrim  a_fresh
  xValue = XValue a_fresh
  xApp   = XApp   a_fresh

  goS env s
   = do env' <- updateEnv env s
        let go ss = goS env' ss
        case s of
             InitAccumulator (Accumulator n at _ x) ss
              | Just (Latest,PairT a b,[na,nb]) <- Map.lookup n env'
              -> go
               $ InitAccumulator (Accumulator na at a x)
               $ InitAccumulator (Accumulator nb at b x)
                 ss

              | Just (Mutable,PairT a b,[na,nb]) <- Map.lookup n env'
              -> go
               $ InitAccumulator (Accumulator na at a (xPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairFst a b) `xApp` x))
               $ InitAccumulator (Accumulator nb at b (xPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd a b) `xApp` x)) ss

              | Just (Mutable,UnitT,[]) <- Map.lookup n env'
              -> go ss


             Read n acc at _ ss
              | Just (Latest, PairT ta tb, [na,nb]) <- Map.lookup acc env'
              -> do n1 <- freshPrefix' n
                    n2 <- freshPrefix' n
                    let zips = xPrim (PrimArray $ PrimArrayZip ta tb)
                              `xApp` xVar n1
                              `xApp` xVar n2
                    ss'<- substXinS a_fresh n zips ss
                    go $ Read n1 na at ta
                       $ Read n2 nb at tb
                       $ ss'

              | Just (Mutable, PairT ta tb, [na,nb]) <- Map.lookup acc env'
              -> do n1 <- freshPrefix' n
                    n2 <- freshPrefix' n
                    let pair    = xPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta tb)
                                `xApp` (xVar n1)
                                `xApp` (xVar n2)
                    ss'<- substXinS a_fresh n pair ss
                    go $ Read n1 na at ta
                       $ Read n2 nb at tb
                       $ ss'

              | Just (Mutable, UnitT, []) <- Map.lookup acc env'
              -> do ss' <- substXinS a_fresh n (xValue UnitT VUnit) ss
                    go ss'


             Push n x
              | Just (Latest, PairT a b, [na,nb]) <- Map.lookup n env'
              -> go
               $ Block
               [ Push na (xPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairFst a b) `xApp` x)
               , Push nb (xPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd a b) `xApp` x) ]


             Write n x
              | Just (Mutable, PairT a b, [na,nb]) <- Map.lookup n env'
              -> go
               $ Block
               [ Write na (xPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairFst a b) `xApp` x)
               , Write nb (xPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd a b) `xApp` x) ]

              | Just (_, UnitT, _) <- Map.lookup n env'
              -> return (env', mempty)


             LoadResumable n t
              | Just (_, _, ns) <- Map.lookup n env'
              -> go $ Block (fmap (flip LoadResumable t) ns)


             SaveResumable n t
              | Just (_, _, ns) <- Map.lookup n env'
              -> go $ Block (fmap (flip SaveResumable t) ns)


             _ -> return (env, s)

  updateEnv env s
   | InitAccumulator (Accumulator n at vt@(PairT _ _) _) _ <- s
   -- TODO: XXX: temporarily disable splitting out Latests.
   -- We need a "zip" here, really
   = do v1 <- freshPrefix' n
        v2 <- freshPrefix' n
        return $ Map.insert n (at,vt,[v1,v2]) env

   | InitAccumulator (Accumulator n Mutable UnitT _) _ <- s
   = do return $ Map.insert n (Mutable,UnitT,[]) env

   | otherwise
   = return env


