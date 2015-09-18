{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
module Icicle.Avalanche.Statement.Simp.Melt (
    melt
  ) where

import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Statement.Simp
import              Icicle.Avalanche.Statement.Statement

import qualified    Icicle.Common.Exp.Prim.Minimal as Min
import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Fresh
import              Icicle.Common.Type

import              P

import qualified    Data.Map            as Map


pattern PrimZip  ta tb = PrimArray   (PrimArrayZip ta tb)
pattern PrimPair ta tb = PrimMinimal (Min.PrimConst (Min.PrimConstPair ta tb))
pattern PrimFst  ta tb = PrimMinimal (Min.PrimPair  (Min.PrimPairFst   ta tb))
pattern PrimSnd  ta tb = PrimMinimal (Min.PrimPair  (Min.PrimPairSnd   ta tb))
pattern PrimMkOpt   tv = PrimOption  (PrimOptionPack          tv)
pattern PrimIsSome  tv = PrimProject (PrimProjectOptionIsSome tv)
pattern PrimGet     tv = PrimUnsafe  (PrimUnsafeOptionGet     tv)


melt :: (Show n, Ord n)
     => a
     -> Statement a n Prim
     -> Fresh n (Statement a n Prim)
melt a_fresh statements
 = transformUDStmt goStmt Map.empty statements
 where
  xVar   = XVar   a_fresh
  xPrim  = XPrim  a_fresh
  xValue = XValue a_fresh
  xApp   = XApp   a_fresh

  primZip  ta tb x y = xPrim (PrimZip  ta tb) `xApp` xVar x `xApp` xVar y
  primPair ta tb x y = xPrim (PrimPair ta tb) `xApp` xVar x `xApp` xVar y
  primFst  ta tb x   = xPrim (PrimFst  ta tb) `xApp` x
  primSnd  ta tb x   = xPrim (PrimSnd  ta tb) `xApp` x

  primMkOpt   tv b v = xPrim (PrimMkOpt   tv) `xApp` xVar b `xApp` xVar v
  primIsSome  tv v   = xPrim (PrimIsSome  tv) `xApp` v
  primGet     tv v   = xPrim (PrimGet     tv) `xApp` v

  goStmt env stmt
   = do env' <- updateEnv stmt env
        let go = goStmt env'
        case stmt of

          InitAccumulator (Accumulator n avt _ x) ss
           | Just (Latest, PairT ta tb, [na, nb])   <- Map.lookup n env'
           -> go
            . InitAccumulator (Accumulator na avt ta x)
            . InitAccumulator (Accumulator nb avt tb x)
            $ ss

           | Just (Mutable, PairT ta tb, [na, nb])  <- Map.lookup n env'
           -> go
            . InitAccumulator (Accumulator na avt ta (primFst ta tb x))
            . InitAccumulator (Accumulator nb avt tb (primSnd ta tb x))
            $ ss

           | Just (Mutable, OptionT tv, [nb, nv])   <- Map.lookup n env'
           , tb                                     <- BoolT
           -> go
            . InitAccumulator (Accumulator nb avt tb (primIsSome tv x))
            . InitAccumulator (Accumulator nv avt tv (primGet    tv x))
            $ ss

           | Just (Mutable, UnitT, [])              <- Map.lookup n env'
           -> go ss


          Read n acc avt _ ss
           | Just (Latest, PairT ta tb, [na, nb])   <- Map.lookup acc env'
           -> do na' <- freshPrefix' n
                 nb' <- freshPrefix' n
                 ss' <- substXinS a_fresh n (primZip ta tb na' nb') ss
                 go . Read na' na avt ta
                    . Read nb' nb avt tb
                    $ ss'

           | Just (Mutable, PairT ta tb, [na, nb])  <- Map.lookup acc env'
           -> do na' <- freshPrefix' n
                 nb' <- freshPrefix' n
                 ss' <- substXinS a_fresh n (primPair ta tb na' nb') ss
                 go . Read na' na avt ta
                    . Read nb' nb avt tb
                    $ ss'

           | Just (Mutable, OptionT tv, [nb, nv])   <- Map.lookup acc env'
           , tb                                     <- BoolT
           -> do nb' <- freshPrefix' n
                 nv' <- freshPrefix' n
                 ss' <- substXinS a_fresh n (primMkOpt tv nb' nv') ss
                 go . Read nb' nb avt tb
                    . Read nv' nv avt tv
                    $ ss'

           | Just (Mutable, UnitT, [])              <- Map.lookup acc env'
           -> do ss' <- substXinS a_fresh n (xValue UnitT VUnit) ss
                 go ss'


          Push n x
           | Just (Latest, PairT ta tb, [na, nb])   <- Map.lookup n env'
           -> go
            $ Block [ Push na (primFst ta tb x)
                    , Push nb (primSnd ta tb x) ]


          Write n x
           | Just (Mutable, PairT ta tb, [na, nb])  <- Map.lookup n env'
           -> go
            $ Block [ Write na (primFst ta tb x)
                    , Write nb (primSnd ta tb x) ]

           | Just (Mutable, OptionT tv, [nb, nv])   <- Map.lookup n env'
           -> go
            $ Block [ Write nb (primIsSome tv x)
                    , Write nv (primGet    tv x) ]

           | Just (_, UnitT, _)                     <- Map.lookup n env'
           -> return (env', mempty)


          LoadResumable n _
           | Just (_, PairT ta tb, [na, nb]) <- Map.lookup n env'
           -> go
            $ Block [ LoadResumable na ta
                    , LoadResumable nb tb ]

           | Just (_, OptionT tv, [nb, nv]) <- Map.lookup n env'
           , tb                             <- BoolT
           -> go
            $ Block [ LoadResumable nb tb
                    , LoadResumable nv tv ]

           | Just (_, UnitT, [])            <- Map.lookup n env'
           -> go
            $ Block []


          SaveResumable n _
           | Just (_, PairT ta tb, [na, nb]) <- Map.lookup n env'
           -> go
            $ Block [ SaveResumable na ta
                    , SaveResumable nb tb ]

           | Just (_, OptionT tv, [nb, nv]) <- Map.lookup n env'
           , tb                             <- BoolT
           -> go
            $ Block [ SaveResumable nb tb
                    , SaveResumable nv tv ]

           | Just (_, UnitT, [])            <- Map.lookup n env'
           -> go
            $ Block []

          _
           -> return (env', stmt)


  updateEnv s env
   | InitAccumulator (Accumulator n at avt@(PairT _ _) _) _ <- s
   = do na <- freshPrefix' n
        nb <- freshPrefix' n
        return (Map.insert n (at, avt, [na, nb]) env)

   | InitAccumulator (Accumulator n at avt@(OptionT _) _) _ <- s
   = do nb <- freshPrefix' n
        nv <- freshPrefix' n
        return (Map.insert n (at, avt, [nb, nv]) env)

   | InitAccumulator (Accumulator n Mutable UnitT _) _ <- s
   = do return (Map.insert n (Mutable, UnitT, []) env)

   | otherwise
   = return env
