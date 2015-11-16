{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}
module Icicle.Avalanche.Statement.Simp.Melt (
    melt
  , meltValue
  , unmeltValue
  ) where

import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Prim.Eval
import              Icicle.Avalanche.Statement.Simp
import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Fresh
import              Icicle.Common.Type
import              Icicle.Common.Annot

import              P

import qualified    Data.List as List
import qualified    Data.Map as Map


------------------------------------------------------------------------

data MeltOps a n p = MeltOps {
    xPrim  :: p                      -> Exp a n p
  , xVar   :: Name n                 -> Exp a n p
  , xValue :: ValType   -> BaseValue -> Exp a n p
  , xApp   :: Exp a n p -> Exp a n p -> Exp a n p

  , primPack ::          ValType -> [Name n]  -> Exp a n p
  , primUnpack :: Int -> ValType -> Exp a n p -> Exp a n p
  }

meltOps :: a -> MeltOps a n Prim
meltOps a_fresh
 = MeltOps{..}
 where
  xVar   = XVar   a_fresh
  xPrim  = XPrim  a_fresh
  xValue = XValue a_fresh
  xApp   = XApp   a_fresh

  primPack      t ns = foldl (\x n -> x `xApp` xVar n) (xPrim (PrimMelt (PrimMeltPack t))) ns
  primUnpack ix t x  = xPrim (PrimMelt (PrimMeltUnpack ix t)) `xApp` x

------------------------------------------------------------------------

melt :: (Show n, Ord n)
     => Annot a
     -> Statement (Annot a) n Prim
     -> Fresh n (Statement (Annot a) n Prim)
melt a_fresh ss
 =   meltAccumulators a_fresh ss
 >>= meltBindings     a_fresh
 >>= meltForeachFacts a_fresh
 >>= meltOutputs      a_fresh

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
          InitAccumulator (Accumulator n _ x) ss
           | Just (tp, nas) <- Map.lookup n env'
           , Just ts        <- tryMeltType tp
           , ntis           <- List.zip3 nas ts [0..]
           -> go
            $ foldr (\(na,t,ix)
            -> InitAccumulator (Accumulator na t (primUnpack ix tp x))) ss ntis

          Read na acc _ ss
           | Just (tp, ns) <- Map.lookup acc env'
           , Just ts       <- tryMeltType tp
           -> do ns' <- freshes (length ns) na
                 ss' <- substXinS a_fresh na (primPack tp ns') ss
                 go $ foldr (\(n',n,t) -> Read n' n t) ss' (List.zip3 ns' ns ts)

          Write n x
           | Just (tp, nas) <- Map.lookup n env'
           , Just _         <- tryMeltType tp
           , nis            <- List.zip nas [0..]
           -> go . Block
            $ fmap (\(na,ix) -> Write na (primUnpack ix tp x)) nis

          LoadResumable n _
           | Just (tp, nas) <- Map.lookup n env'
           , Just ts        <- tryMeltType tp
           , nts            <- List.zip nas ts
           -> go . Block
            $ fmap (\(na,t) -> LoadResumable na t) nts

          SaveResumable n _
           | Just (tp, nas) <- Map.lookup n env'
           , Just ts        <- tryMeltType tp
           , nts            <- List.zip nas ts
           -> go . Block
            $ fmap (\(na,t) -> SaveResumable na t) nts

          _
           -> return (env', stmt)


  updateEnv s env
   | InitAccumulator (Accumulator n tp _) _ <- s
   , Just ts                                <- tryMeltType tp
   = do ns <- replicateM (length ts) (freshPrefix' n)
        return (Map.insert n (tp, ns) env)

   | otherwise
   = return env

--------------------------------------------------------------------------------

-- | Melt the body of Let bindings into multiple bindings and substitute the
--   old binding with new ones.
--
meltBindings
  :: (Ord n)
  => Annot a
  -> Statement (Annot a) n Prim
  -> Fresh n (Statement (Annot a) n Prim)
meltBindings a_fresh statements
 = transformUDStmt goStmt () statements
 where
  MeltOps{..} = meltOps a_fresh

  goStmt () stmt
   = case stmt of
       Let n x ss
        | vt     <- functionReturns (annType (annotOfExp x))
        , Just _ <- tryMeltType vt
        -> do (xs, x') <- meltBody a_fresh (n, vt, x)
              ss'      <- substXinS a_fresh n x' ss
              let stmt' = foldr mkLet ss' xs
              return ((), stmt')

       _ -> return ((), stmt)

  mkLet (n,_,x) s
   = Let n x s


meltBody
 :: a
 -> (Name n, ValType, Exp a n Prim)
 -> Fresh n ([(Name n, ValType, Exp a n Prim)], Exp a n Prim)
meltBody a_fresh (n, vt, x)
 = do let ts = meltType vt
      bns <- freshes (length ts) n
      let binds  = fmap (\(bn, bt, ix) -> (bn, bt, primUnpack ix vt x))
                 $ List.zip3 bns ts [0..]
          unmelt = primPack vt bns
      return (binds, unmelt)
 where
  MeltOps{..} = meltOps a_fresh

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
   | Just ts <- tryMeltType t
   = do ns  <- freshes (length ts) n
        ss' <- substXinS a_fresh n (primPack t ns) ss
        let nts = List.zip ns ts
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
 | Just ts <- tryMeltType t
 = let MeltOps{..} = meltOps a_fresh
       tis         = List.zip ts [0..]
       go (tv,ix)  = meltExp a_fresh (primUnpack ix t x) tv
   in concat (fmap go tis)

 | otherwise
 = [(x, t)]

freshes :: Int -> Name n -> Fresh n [Name n]
freshes i n
 = replicateM i $ freshPrefix' n
