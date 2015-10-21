{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Avalanche.Statement.Simp.Melt (
    melt
  , meltValue
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


------------------------------------------------------------------------

-- this could be shared between modules if it's useful?

pattern PrimZip     ta tb = PrimArray   (PrimArrayZip ta tb)
pattern PrimPair    ta tb = PrimMinimal (Min.PrimConst (Min.PrimConstPair ta tb))
pattern PrimFst     ta tb = PrimMinimal (Min.PrimPair  (Min.PrimPairFst   ta tb))
pattern PrimSnd     ta tb = PrimMinimal (Min.PrimPair  (Min.PrimPairSnd   ta tb))

pattern PrimMkOpt      tv = PrimPack    (PrimOptionPack          tv)
pattern PrimIsSome     tv = PrimProject (PrimProjectOptionIsSome tv)
pattern PrimGet        tv = PrimUnsafe  (PrimUnsafeOptionGet     tv)

pattern PrimMkSum   ta tb = PrimPack    (PrimSumPack           ta tb)
pattern PrimIsRight ta tb = PrimProject (PrimProjectSumIsRight ta tb)
pattern PrimLeft    ta tb = PrimUnsafe  (PrimUnsafeSumGetLeft  ta tb)
pattern PrimRight   ta tb = PrimUnsafe  (PrimUnsafeSumGetRight ta tb)

data MeltOps a n p = MeltOps {
    xPrim  :: p -> Exp a n p
  , xVar   :: Name n -> Exp a n p
  , xValue :: ValType   -> BaseValue -> Exp a n p
  , xApp   :: Exp a n p -> Exp a n p -> Exp a n p

  , primZip  :: ValType -> ValType -> Name n -> Name n -> Exp a n p
  , primPair :: ValType -> ValType -> Name n -> Name n -> Exp a n p
  , primFst  :: ValType -> ValType -> Exp a n p        -> Exp a n p
  , primSnd  :: ValType -> ValType -> Exp a n p        -> Exp a n p

  , primMkOpt  :: ValType -> Name n -> Name n -> Exp a n p
  , primIsSome :: ValType -> Exp a n p        -> Exp a n p
  , primGet    :: ValType -> Exp a n p        -> Exp a n p

  , primMkSum   :: ValType -> ValType -> Name n -> Name n -> Name n -> Exp a n p
  , primIsRight :: ValType -> ValType -> Exp a n p                  -> Exp a n p
  , primLeft    :: ValType -> ValType -> Exp a n p                  -> Exp a n p
  , primRight   :: ValType -> ValType -> Exp a n p                  -> Exp a n p
  }

meltOps :: a -> MeltOps a n Prim
meltOps a_fresh
 = MeltOps{..}
 where
  xVar   = XVar   a_fresh
  xPrim  = XPrim  a_fresh
  xValue = XValue a_fresh
  xApp   = XApp   a_fresh

  primZip     ta tb x y   = xPrim (PrimZip     ta tb) `xApp` xVar x `xApp` xVar y
  primPair    ta tb x y   = xPrim (PrimPair    ta tb) `xApp` xVar x `xApp` xVar y
  primFst     ta tb x     = xPrim (PrimFst     ta tb) `xApp` x
  primSnd     ta tb x     = xPrim (PrimSnd     ta tb) `xApp` x

  primMkOpt   tv b v      = xPrim (PrimMkOpt      tv) `xApp` xVar b `xApp` xVar v
  primIsSome  tv v        = xPrim (PrimIsSome     tv) `xApp` v
  primGet     tv v        = xPrim (PrimGet        tv) `xApp` v

  primMkSum   ta tb i x y = xPrim (PrimMkSum   ta tb) `xApp` xVar i `xApp` xVar x `xApp` xVar y
  primIsRight ta tb x     = xPrim (PrimIsRight ta tb) `xApp` x
  primLeft    ta tb x     = xPrim (PrimLeft    ta tb) `xApp` x
  primRight   ta tb x     = xPrim (PrimRight   ta tb) `xApp` x

------------------------------------------------------------------------

melt :: (Show n, Ord n)
     => a
     -> Statement a n Prim
     -> Fresh n (Statement a n Prim)
melt a_fresh statements
 = do ss <- meltAccumulators a_fresh statements
      meltForeachFacts a_fresh ss

------------------------------------------------------------------------

meltAccumulators :: (Show n, Ord n)
                 => a
                 -> Statement a n Prim
                 -> Fresh n (Statement a n Prim)
meltAccumulators a_fresh statements
 = transformUDStmt goStmt Map.empty statements
 where
  MeltOps{..} = meltOps a_fresh

  goStmt env stmt
   = do env' <- updateEnv stmt env
        let go = goStmt env'
        case stmt of

          InitAccumulator (Accumulator n avt _ x) ss
           | Just (Latest, PairT ta tb, [na, nb])       <- Map.lookup n env'
           -> go
            . InitAccumulator (Accumulator na avt ta x)
            . InitAccumulator (Accumulator nb avt tb x)
            $ ss

           | Just (Mutable, PairT ta tb, [na, nb])      <- Map.lookup n env'
           -> go
            . InitAccumulator (Accumulator na avt ta (primFst ta tb x))
            . InitAccumulator (Accumulator nb avt tb (primSnd ta tb x))
            $ ss

           | Just (Mutable, OptionT tv, [nb, nv])       <- Map.lookup n env'
           , tb                                         <- BoolT
           -> go
            . InitAccumulator (Accumulator nb avt tb (primIsSome tv x))
            . InitAccumulator (Accumulator nv avt tv (primGet    tv x))
            $ ss

           | Just (Mutable, SumT ta tb, [ni, na, nb])   <- Map.lookup n env'
           , ti                                         <- BoolT
           -> go
            . InitAccumulator (Accumulator ni avt ti (primIsRight ta tb x))
            . InitAccumulator (Accumulator na avt ta (primLeft    ta tb x))
            . InitAccumulator (Accumulator nb avt tb (primRight   ta tb x))
            $ ss

           | Just (Mutable, UnitT, [])                  <- Map.lookup n env'
           -> go ss


          Read n acc avt _ ss
           | Just (Latest, PairT ta tb, [na, nb])       <- Map.lookup acc env'
           -> do na' <- freshPrefix' n
                 nb' <- freshPrefix' n
                 ss' <- substXinS a_fresh n (primZip ta tb na' nb') ss
                 go . Read na' na avt ta
                    . Read nb' nb avt tb
                    $ ss'

           | Just (Mutable, PairT ta tb, [na, nb])      <- Map.lookup acc env'
           -> do na' <- freshPrefix' n
                 nb' <- freshPrefix' n
                 ss' <- substXinS a_fresh n (primPair ta tb na' nb') ss
                 go . Read na' na avt ta
                    . Read nb' nb avt tb
                    $ ss'

           | Just (Mutable, OptionT tv, [nb, nv])       <- Map.lookup acc env'
           , tb                                         <- BoolT
           -> do nb' <- freshPrefix' n
                 nv' <- freshPrefix' n
                 ss' <- substXinS a_fresh n (primMkOpt tv nb' nv') ss
                 go . Read nb' nb avt tb
                    . Read nv' nv avt tv
                    $ ss'

           | Just (Mutable, SumT ta tb, [ni, na, nb])   <- Map.lookup acc env'
           , ti                                         <- BoolT
           -> do ni' <- freshPrefix' n
                 na' <- freshPrefix' n
                 nb' <- freshPrefix' n
                 ss' <- substXinS a_fresh n (primMkSum ta tb ni' na' nb') ss
                 go . Read ni' ni avt ti
                    . Read na' na avt ta
                    . Read nb' nb avt tb
                    $ ss'

           | Just (Mutable, UnitT, [])                  <- Map.lookup acc env'
           -> do ss' <- substXinS a_fresh n (xValue UnitT VUnit) ss
                 go ss'


          Push n x
           | Just (Latest, PairT ta tb, [na, nb])       <- Map.lookup n env'
           -> go
            $ Block [ Push na (primFst ta tb x)
                    , Push nb (primSnd ta tb x) ]


          Write n x
           | Just (Mutable, PairT ta tb, [na, nb])      <- Map.lookup n env'
           -> go
            $ Block [ Write na (primFst ta tb x)
                    , Write nb (primSnd ta tb x) ]

           | Just (Mutable, OptionT tv, [nb, nv])       <- Map.lookup n env'
           -> go
            $ Block [ Write nb (primIsSome tv x)
                    , Write nv (primGet    tv x) ]

           | Just (Mutable, SumT ta tb, [ni, na, nb])   <- Map.lookup n env'
           -> go
            $ Block [ Write ni (primIsRight ta tb x)
                    , Write na (primLeft    ta tb x)
                    , Write nb (primRight   ta tb x) ]

           | Just (_, UnitT, _)                         <- Map.lookup n env'
           -> return (env', mempty)


          LoadResumable n _
           | Just (_, PairT ta tb, [na, nb])            <- Map.lookup n env'
           -> go
            $ Block [ LoadResumable na ta
                    , LoadResumable nb tb ]

           | Just (_, OptionT tv, [nb, nv])             <- Map.lookup n env'
           , tb                                         <- BoolT
           -> go
            $ Block [ LoadResumable nb tb
                    , LoadResumable nv tv ]

           | Just (_, SumT ta tb, [ni, na, nb])         <- Map.lookup n env'
           , ti                                         <- BoolT
           -> go
            $ Block [ LoadResumable ni ti
                    , LoadResumable na ta
                    , LoadResumable nb tb ]

           | Just (_, UnitT, [])                        <- Map.lookup n env'
           -> go
            $ Block []


          SaveResumable n _
           | Just (_, PairT ta tb, [na, nb])            <- Map.lookup n env'
           -> go
            $ Block [ SaveResumable na ta
                    , SaveResumable nb tb ]

           | Just (_, OptionT tv, [nb, nv])             <- Map.lookup n env'
           , tb                                         <- BoolT
           -> go
            $ Block [ SaveResumable nb tb
                    , SaveResumable nv tv ]

           | Just (_, SumT ta tb, [ni, na, nb])         <- Map.lookup n env'
           , ti                                         <- BoolT
           -> go
            $ Block [ SaveResumable ni ti
                    , SaveResumable na ta
                    , SaveResumable nb tb ]

           | Just (_, UnitT, [])                        <- Map.lookup n env'
           -> go
            $ Block []

          _
           -> return (env', stmt)


  updateEnv s env
   | InitAccumulator (Accumulator n at avt@(PairT _ _) _) _ <- s
   = do na <- freshPrefix' n
        nb <- freshPrefix' n
        return (Map.insert n (at, avt, [na, nb]) env)

   | InitAccumulator (Accumulator n at avt@(SumT _ _) _) _ <- s
   = do ni <- freshPrefix' n
        na <- freshPrefix' n
        nb <- freshPrefix' n
        return (Map.insert n (at, avt, [ni, na, nb]) env)

   | InitAccumulator (Accumulator n at avt@(OptionT _) _) _ <- s
   = do nb <- freshPrefix' n
        nv <- freshPrefix' n
        return (Map.insert n (at, avt, [nb, nv]) env)

   | InitAccumulator (Accumulator n Mutable UnitT _) _ <- s
   = do return (Map.insert n (Mutable, UnitT, []) env)

   | otherwise
   = return env

------------------------------------------------------------------------

meltForeachFacts :: forall a n. (Show n, Ord n)
                 => a
                 -> Statement a n Prim
                 -> Fresh n (Statement a n Prim)
meltForeachFacts a_fresh statements
 = transformUDStmt goStmt () statements
 where
  MeltOps{..} = meltOps a_fresh

  goStmt () stmt
   = case stmt of
       ForeachFacts ns vt lt ss
        -> do (ns', ss') <- meltFix ns ss
              return ((), ForeachFacts ns' vt lt ss')
       _
        -> return ((), stmt)

  meltFix ns0 ss0 = do
    (ns1, ss1) <- meltFacts ns0 ss0
    if length ns0 /= length ns1
    then meltFix ns1 ss1
    else return (ns1, ss1)


  meltFacts :: [(Name n, ValType)]
            -> Statement a n Prim
            -> Fresh n ([(Name n, ValType)], Statement a n Prim)

  meltFacts []     ss0 = return ([], ss0)
  meltFacts (n:ns) ss0 = do
    (xs, ss1) <- meltFact  n  ss0
    (ys, ss2) <- meltFacts ns ss1
    return (xs <> ys, ss2)


  meltFact :: (Name n, ValType)
           -> Statement a n Prim
           -> Fresh n ([(Name n, ValType)], Statement a n Prim)

  meltFact (n, t) ss
   | PairT ta tb <- t
   = do na <- freshPrefix' n
        nb <- freshPrefix' n
        ss' <- substXinS a_fresh n (primPair ta tb na nb) ss
        let ns = [(na, ta), (nb, tb)]
        return (ns, ss')

   | OptionT tv <- t
   = do nb <- freshPrefix' n
        nv <- freshPrefix' n
        ss' <- substXinS a_fresh n (primMkOpt tv nb nv) ss
        let ns = [(nb, BoolT), (nv, tv)]
        return (ns, ss')

   | SumT ta tb <- t
   = do ni <- freshPrefix' n
        na <- freshPrefix' n
        nb <- freshPrefix' n
        ss' <- substXinS a_fresh n (primMkSum ta tb ni na nb) ss
        let ns = [(ni, BoolT), (na, ta), (nb, tb)]
        return (ns, ss')

   | otherwise
   = return ([(n, t)], ss)

------------------------------------------------------------------------

-- implementation should match `meltFact` above
meltValue :: BaseValue -> ValType -> Maybe [BaseValue]
meltValue v t
 = let apcat x y = (<>) <$> x <*> y
   in case v of
     VInt{}      -> Just [v]
     VDouble{}   -> Just [v]
     VUnit{}     -> Just [v]
     VBool{}     -> Just [v]
     VDateTime{} -> Just [v]
     VString{}   -> Just [v]
     VArray{}    -> Just [v]
     VMap{}      -> Just [v]
     VStruct{}   -> Just [v]
     VBuf{}      -> Just [v]
     VError{}    -> Just [v]

     VPair a b
      | PairT ta tb <- t
      -> meltValue a ta `apcat` meltValue b tb

      | otherwise
      -> Nothing

     VLeft a
      | SumT ta tb <- t
      -> pure [VBool False] `apcat` meltValue a ta `apcat` meltValue (defaultOfType tb) tb

      | otherwise
      -> Nothing

     VRight b
      | SumT ta tb <- t
      -> pure [VBool True] `apcat` meltValue (defaultOfType ta) ta `apcat` meltValue b tb

      | otherwise
      -> Nothing

     VNone
      | OptionT tv <- t
      -> pure [VBool False] `apcat` meltValue (defaultOfType tv) tv

      | otherwise
      -> Nothing

     VSome x
      | OptionT tx <- t
      -> pure [VBool True] `apcat` meltValue x tx

      | otherwise
      -> Nothing
