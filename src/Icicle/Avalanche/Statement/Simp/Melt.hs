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
     => Statement n Prim
     -> Fresh n (Statement n Prim)
melt statements
 = transformUDStmt goS Map.empty statements
 where
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
               $ InitAccumulator (Accumulator na at a (XPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairFst a b) `XApp` x))
               $ InitAccumulator (Accumulator nb at b (XPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd a b) `XApp` x)) ss

              | Just (Mutable,UnitT,[]) <- Map.lookup n env'
              -> go ss


             Read n acc ss
              | Just (Latest, PairT ta tb, [na,nb]) <- Map.lookup acc env'
              -> do n1 <- freshPrefix' n
                    n2 <- freshPrefix' n
                    let zips = XPrim (PrimArray $ PrimArrayZip ta tb)
                              `XApp` XVar n1
                              `XApp` XVar n2
                    ss'<- substXinS n zips ss
                    go $ Read n1 na $ Read n2 nb $ ss'

              | Just (Mutable, PairT ta tb, [na,nb]) <- Map.lookup acc env'
              -> do n1 <- freshPrefix' n
                    n2 <- freshPrefix' n
                    let pair    = XPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta tb)
                                `XApp` (XVar n1)
                                `XApp` (XVar n2)
                    ss'<- substXinS n pair ss
                    go $ Read n1 na $ Read n2 nb $ ss'

              | Just (Mutable, UnitT, []) <- Map.lookup acc env'
              -> do ss' <- substXinS n (XValue UnitT VUnit) ss
                    go ss'


             Push n x
              | Just (Latest, PairT a b, [na,nb]) <- Map.lookup n env'
              -> go
               $ Block
               [ Push na (XPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairFst a b) `XApp` x)
               , Push nb (XPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd a b) `XApp` x) ]


             Write n x
              | Just (Mutable, PairT a b, [na,nb]) <- Map.lookup n env'
              -> go
               $ Block
               [ Write na (XPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairFst a b) `XApp` x)
               , Write nb (XPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd a b) `XApp` x) ]

              | Just (_, UnitT, _) <- Map.lookup n env'
              -> return (env', mempty)


             LoadResumable n
              | Just (_, _, ns) <- Map.lookup n env'
              -> go $ Block (fmap LoadResumable ns)


             SaveResumable n
              | Just (_, _, ns) <- Map.lookup n env'
              -> go $ Block (fmap SaveResumable ns)


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


