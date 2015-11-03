{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Avalanche.Statement.Simp.Melt (
    melt
  , meltValue
  , unmeltValue
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

import qualified    Data.List as List
import qualified    Data.Map as Map


------------------------------------------------------------------------

-- this could be shared between modules if it's useful?

pattern PrimZip  ta tb = PrimArray   (PrimArrayZip ta tb)
pattern PrimPair ta tb = PrimMinimal (Min.PrimConst (Min.PrimConstPair ta tb))
pattern PrimFst  ta tb = PrimMinimal (Min.PrimPair  (Min.PrimPairFst   ta tb))
pattern PrimSnd  ta tb = PrimMinimal (Min.PrimPair  (Min.PrimPairSnd   ta tb))

pattern PrimMkOpt   tv = PrimPack    (PrimOptionPack          tv)
pattern PrimIsSome  tv = PrimProject (PrimProjectOptionIsSome tv)
pattern PrimGetSome tv = PrimUnsafe  (PrimUnsafeOptionGet     tv)

pattern PrimMkSum    ta tb = PrimPack    (PrimSumPack           ta tb)
pattern PrimIsRight  ta tb = PrimProject (PrimProjectSumIsRight ta tb)
pattern PrimGetLeft  ta tb = PrimUnsafe  (PrimUnsafeSumGetLeft  ta tb)
pattern PrimGetRight ta tb = PrimUnsafe  (PrimUnsafeSumGetRight ta tb)

pattern PrimMkStruct ts       = PrimPack    (PrimStructPack ts)
pattern PrimGetField ts nf tf = PrimMinimal (Min.PrimStruct (Min.PrimStructGet nf tf ts))

data MeltOps a n p = MeltOps {
    xPrim  :: p                      -> Exp a n p
  , xVar   :: Name n                 -> Exp a n p
  , xValue :: ValType   -> BaseValue -> Exp a n p
  , xApp   :: Exp a n p -> Exp a n p -> Exp a n p

  , primZip  :: ValType -> ValType -> Name n -> Name n -> Exp a n p
  , primPair :: ValType -> ValType -> Name n -> Name n -> Exp a n p
  , primFst  :: ValType -> ValType -> Exp a n p        -> Exp a n p
  , primSnd  :: ValType -> ValType -> Exp a n p        -> Exp a n p

  , primMkOpt   :: ValType -> Name n -> Name n -> Exp a n p
  , primIsSome  :: ValType -> Exp a n p        -> Exp a n p
  , primGetSome :: ValType -> Exp a n p        -> Exp a n p

  , primMkSum    :: ValType -> ValType -> Name n -> Name n -> Name n -> Exp a n p
  , primIsRight  :: ValType -> ValType -> Exp a n p                  -> Exp a n p
  , primGetLeft  :: ValType -> ValType -> Exp a n p                  -> Exp a n p
  , primGetRight :: ValType -> ValType -> Exp a n p                  -> Exp a n p

  , primMkStruct :: StructType -> [Name n]                            -> Exp a n p
  , primGetField :: StructType -> StructField -> ValType -> Exp a n p -> Exp a n p
  }

meltOps :: a -> MeltOps a n Prim
meltOps a_fresh
 = MeltOps{..}
 where
  xVar   = XVar   a_fresh
  xPrim  = XPrim  a_fresh
  xValue = XValue a_fresh
  xApp   = XApp   a_fresh

  primZip     ta tb x y = xPrim (PrimZip  ta tb) `xApp` xVar x `xApp` xVar y
  primPair    ta tb x y = xPrim (PrimPair ta tb) `xApp` xVar x `xApp` xVar y
  primFst     ta tb x   = xPrim (PrimFst  ta tb) `xApp` x
  primSnd     ta tb x   = xPrim (PrimSnd  ta tb) `xApp` x

  primMkOpt   tv b v = xPrim (PrimMkOpt   tv) `xApp` xVar b `xApp` xVar v
  primIsSome  tv v   = xPrim (PrimIsSome  tv) `xApp` v
  primGetSome tv v   = xPrim (PrimGetSome tv) `xApp` v

  primMkSum    ta tb i x y = xPrim (PrimMkSum    ta tb) `xApp` xVar i `xApp` xVar x `xApp` xVar y
  primIsRight  ta tb x     = xPrim (PrimIsRight  ta tb) `xApp` x
  primGetLeft  ta tb x     = xPrim (PrimGetLeft  ta tb) `xApp` x
  primGetRight ta tb x     = xPrim (PrimGetRight ta tb) `xApp` x

  primMkStruct ts ns      = foldl (\x n -> x `xApp` xVar n) (xPrim (PrimMkStruct ts)) ns
  primGetField ts nf tf x = xPrim (PrimGetField ts nf tf) `xApp` x

------------------------------------------------------------------------

melt :: (Show n, Ord n)
     => a
     -> Statement a n Prim
     -> Fresh n (Statement a n Prim)
melt a_fresh ss0
 = do ss1 <- meltAccumulators a_fresh ss0
      ss2 <- meltForeachFacts a_fresh ss1
      ss3 <- meltOutputs      a_fresh ss2
      return ss3

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
            . InitAccumulator (Accumulator nb avt tb (primIsSome  tv x))
            . InitAccumulator (Accumulator nv avt tv (primGetSome tv x))
            $ ss

           | Just (Mutable, SumT ta tb, [ni, na, nb])   <- Map.lookup n env'
           , ti                                         <- BoolT
           -> go
            . InitAccumulator (Accumulator ni avt ti (primIsRight  ta tb x))
            . InitAccumulator (Accumulator na avt ta (primGetLeft  ta tb x))
            . InitAccumulator (Accumulator nb avt tb (primGetRight ta tb x))
            $ ss

           | Just (Mutable, StructT ts, ns)             <- Map.lookup n env'
           , nfts                                       <- List.zip ns (Map.toList (getStructType ts))
           -> go
            $ foldr (\(na,(f,t)) -> InitAccumulator (Accumulator na avt t (primGetField ts f t x))) ss nfts

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

           | Just (Mutable, StructT ts, nas)            <- Map.lookup acc env'
           , fts                                        <- Map.elems (getStructType ts)
           -> do ns' <- replicateM (length fts) (freshPrefix' n)
                 ss' <- substXinS a_fresh n (primMkStruct ts ns') ss
                 go $ foldr (\(n',na,t) -> Read n' na avt t) ss' (List.zip3 ns' nas fts)

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
            $ Block [ Write nb (primIsSome  tv x)
                    , Write nv (primGetSome tv x) ]

           | Just (Mutable, SumT ta tb, [ni, na, nb])   <- Map.lookup n env'
           -> go
            $ Block [ Write ni (primIsRight  ta tb x)
                    , Write na (primGetLeft  ta tb x)
                    , Write nb (primGetRight ta tb x) ]

           | Just (Mutable, StructT ts, nas)            <- Map.lookup n env'
           , nfts                                       <- List.zip nas (Map.toList (getStructType ts))
           -> go . Block
            $ fmap (\(na,(f,t)) -> Write na (primGetField ts f t x)) nfts

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

           | Just (_, StructT ts, nas)                  <- Map.lookup n env'
           , nts                                        <- List.zip nas (Map.elems (getStructType ts))
           -> go . Block
            $ fmap (\(na,t) -> LoadResumable na t) nts

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

           | Just (_, StructT ts, nas)                  <- Map.lookup n env'
           , nts                                        <- List.zip nas (Map.elems (getStructType ts))
           -> go . Block
            $ fmap (\(na,t) -> SaveResumable na t) nts

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

   | InitAccumulator (Accumulator n at avt@(StructT (StructType fts)) _) _ <- s
   = do ns <- replicateM (Map.size fts) (freshPrefix' n)
        return (Map.insert n (at, avt, ns) env)

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
    if fmap snd ns0 /= fmap snd ns1
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
   | UnitT <- t
   = do ss' <- substXinS a_fresh n (xValue UnitT VUnit) ss
        return ([], ss')

   | PairT ta tb <- t
   = do na <- freshPrefix' n
        nb <- freshPrefix' n
        ss' <- substXinS a_fresh n (primPair ta tb na nb) ss
        let nts = [(na, ta), (nb, tb)]
        return (nts, ss')

   | OptionT tv <- t
   = do nb <- freshPrefix' n
        nv <- freshPrefix' n
        ss' <- substXinS a_fresh n (primMkOpt tv nb nv) ss
        let nts = [(nb, BoolT), (nv, tv)]
        return (nts, ss')

   | SumT ta tb <- t
   = do ni <- freshPrefix' n
        na <- freshPrefix' n
        nb <- freshPrefix' n
        ss' <- substXinS a_fresh n (primMkSum ta tb ni na nb) ss
        let nts = [(ni, BoolT), (na, ta), (nb, tb)]
        return (nts, ss')

   | StructT ts@(StructType fts) <- t
   = do ns  <- replicateM (Map.size fts) (freshPrefix' n)
        ss' <- substXinS a_fresh n (primMkStruct ts ns) ss
        let nts = List.zip ns (Map.elems fts)
        return (nts, ss')

   | otherwise
   = return ([(n, t)], ss)

------------------------------------------------------------------------

meltOutputs :: forall a n. (Show n, Ord n)
            => a
            -> Statement a n Prim
            -> Fresh n (Statement a n Prim)
meltOutputs a_fresh statements
 = transformUDStmt goStmt () statements
 where
  MeltOps{..} = meltOps a_fresh

  goStmt () stmt
   = case stmt of
       Output n t xts
        -> return ((), Output n t (meltExps a_fresh xts))
       _
        -> return ((), stmt)

meltExps :: a -> [(Exp a n Prim, ValType)] -> [(Exp a n Prim, ValType)]
meltExps a_fresh
 = concatMap (\(x,t) -> meltExp a_fresh x t)

meltExp :: a -> Exp a n Prim -> ValType -> [(Exp a n Prim, ValType)]
meltExp a_fresh x t
 = let MeltOps{..} = meltOps a_fresh
   in case t of
     UnitT{}     -> []
     IntT{}      -> [(x, t)]
     DoubleT{}   -> [(x, t)]
     BoolT{}     -> [(x, t)]
     DateTimeT{} -> [(x, t)]
     StringT{}   -> [(x, t)]
     ArrayT{}    -> [(x, t)]
     MapT{}      -> [(x, t)]
     BufT{}      -> [(x, t)]
     ErrorT{}    -> [(x, t)]

     PairT ta tb
      -> meltExp a_fresh (primFst ta tb x) ta
      <> meltExp a_fresh (primSnd ta tb x) tb

     SumT ta tb
      -> meltExp a_fresh (primIsRight  ta tb x) BoolT
      <> meltExp a_fresh (primGetLeft  ta tb x) ta
      <> meltExp a_fresh (primGetRight ta tb x) tb

     OptionT tx
      -> meltExp a_fresh (primIsSome  tx x) BoolT
      <> meltExp a_fresh (primGetSome tx x) tx

     StructT ts
      | fts <- Map.toList (getStructType ts)
      -> let go (nf,tf) = meltExp a_fresh (primGetField ts nf tf x) tf
         in concat (fmap go fts)

------------------------------------------------------------------------

-- implementation should match `meltFact` above
meltValue :: BaseValue -> ValType -> Maybe [BaseValue]
meltValue v t
 = let apcat x y = (<>) <$> x <*> y
   in case v of
     VUnit{}     -> Just []
     VInt{}      -> Just [v]
     VDouble{}   -> Just [v]
     VBool{}     -> Just [v]
     VDateTime{} -> Just [v]
     VString{}   -> Just [v]
     VArray{}    -> Just [v]
     VMap{}      -> Just [v]
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

     VStruct fvs
      | StructT ts <- t
      , fts        <- Map.toList (getStructType ts)
      -> let go (nf,tf) = do
               fv <- Map.lookup nf fvs
               meltValue fv tf
         in concat <$> traverse go fts

      | otherwise
      -> Nothing

------------------------------------------------------------------------

unmeltValue :: [BaseValue] -> ValType -> Maybe BaseValue
unmeltValue vs t
 = case unmeltValue' vs t of
     Just (v, [])   -> Just v
     Nothing        -> Nothing

     -- if we still have values left over
     -- after unmelting, it's a type error
     Just (_v, _xs) -> Nothing

unmeltValue' :: [BaseValue] -> ValType -> Maybe (BaseValue, [BaseValue])
unmeltValue' vs0 t
 = case (vs0, t) of
     -- value was not properly melted in the first place
     (VPair{}:_,   _) -> Nothing
     (VNone{}:_,   _) -> Nothing
     (VSome{}:_,   _) -> Nothing
     (VLeft{}:_,   _) -> Nothing
     (VRight{}:_,  _) -> Nothing
     (VStruct{}:_, _) -> Nothing

     (vs,   UnitT{})     -> Just (VUnit, vs)
     (v:vs, IntT{})      -> Just (v, vs)
     (v:vs, DoubleT{})   -> Just (v, vs)
     (v:vs, BoolT{})     -> Just (v, vs)
     (v:vs, DateTimeT{}) -> Just (v, vs)
     (v:vs, StringT{})   -> Just (v, vs)
     (v:vs, ArrayT{})    -> Just (v, vs)
     (v:vs, MapT{})      -> Just (v, vs)
     (v:vs, BufT{})      -> Just (v, vs)
     (v:vs, ErrorT{})    -> Just (v, vs)

     (_, PairT ta tb)
      -> do (a, vs1) <- unmeltValue' vs0 ta
            (b, vs2) <- unmeltValue' vs1 tb
            Just (VPair a b, vs2)

     (_, SumT ta tb)
      -> do (i, vs1) <- unmeltValue' vs0 BoolT
            (a, vs2) <- unmeltValue' vs1 ta
            (b, vs3) <- unmeltValue' vs2 tb
            case i of
              VBool False -> Just (VLeft  a, vs3)
              VBool True  -> Just (VRight b, vs3)
              _           -> Nothing

     (_, OptionT tx)
      -> do (b, vs1) <- unmeltValue' vs0 BoolT
            (x, vs2) <- unmeltValue' vs1 tx
            case b of
              VBool False -> Just (VNone,   vs2)
              VBool True  -> Just (VSome x, vs2)
              _           -> Nothing

     (_, StructT (StructType ts))
      -> do let go (acc, vs1) ft = do
                  (fv, vs2) <- unmeltValue' vs1 ft
                  return (acc <> [fv], vs2)

            (acc, vs3) <- foldM go ([], vs0) (Map.elems ts)
            let fvs = Map.fromList $ List.zip (Map.keys ts) acc
            Just (VStruct fvs, vs3)

     ([], _)
      -> Nothing
