{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
-- Remove accumulators that don't contribute to an Output
module Icicle.Avalanche.Statement.Simp.Dead (
    dead
  , killAccumulator
  ) where

import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Base
import              Icicle.Common.Exp

import              P

import              Data.Functor.Identity
import qualified    Data.Set as Set


-- | Remove accumulators that don't contribute to output
--
dead :: (Ord n) => Statement a n p -> Statement a n p
dead
 = snd . deadS mempty

data Usage n
 = Usage
 { usageAcc :: Set.Set (Name n)
 , usageExp :: Set.Set (Name n)
 }
 deriving Eq

instance Ord n => Monoid (Usage n) where
 mempty = Usage Set.empty Set.empty
 mappend a b
  = Usage
  { usageAcc = Set.union (usageAcc a) (usageAcc b)
  , usageExp = Set.union (usageExp a) (usageExp b)
  }


deadS :: (Ord n) => Usage n -> Statement a n p -> (Usage n, Statement a n p)
deadS us statements
 = case statements of
    If x ts fs
     -> let (tU, tS) = deadS  us ts
            (fU, fS) = deadS  us fs
            xU       = usageX x
        in (mconcat [tU, fU, xU], If x tS fS)
    Let n x ss
     -> let (sU, sS) = deadS us ss
            xU       = usageX x
        in  if   usedX n sU
            then (mconcat [xU, sU], Let n x sS)
            else (sU, sS)
    ForeachInts n from to ss
     -> let (sU, sS) = deadLoop us ss
            fU       = usageX from
            tU       = usageX to
        in (mconcat [sU, fU, tU], ForeachInts n from to sS)
    ForeachFacts ns vt ty ss
     -> let (sU, sS) = deadLoop us ss
        in  (sU, ForeachFacts ns vt ty sS)

    Block []
     -> (us, statements)
    Block (t:rs)
     -> let (rU, rS) = deadS us (Block rs)
            (tU, tS) = deadS rU t
        in  case rS of
             Block rS' -> (tU, Block (tS : rS'))
             _         -> (tU, Block [tS, rS])

    InitAccumulator acc@(Accumulator n _ x) ss
     -> let (sU, sS) = deadS us ss
            xU       = usageX x
        in  if   usedA n sU
            then (mconcat [xU, sU], InitAccumulator acc sS)
            else (sU, killAccumulator n x sS)

    Read nx na t ss
     -> let (sU, sS) = deadS us ss
        in  if   usedX nx sU
            then (mconcat [sU, usageA na], Read nx na t sS)
            else (sU, sS)

    Write na x
     ->     if   usedA na us
            then (mconcat [us, usageX x], Write na x)
            else (us, mempty)

    Output n t xts
     -> let xsU = fmap (usageX.fst) xts
        in  (mconcat (us : xsU), Output n t xts)

    KeepFactInHistory
     -> (us, KeepFactInHistory)
    -- Load and save resumables are very special.
    -- They themselves do not count as "using" the accumulator,
    -- but on the other hand we cannot get rid of them?
    LoadResumable n t
     -> (us, LoadResumable n t)
    SaveResumable n t
     -> (us, SaveResumable n t)


-- | Find fixpoint of loop usage
-- Will this terminate?
-- Yes!
-- Given that we only ever insert into a usage map, we can say that
--
-- > forall ss usage.
-- > usage `subseteq` fst (deadS usage ss)
--
-- And assuming there are a finite number of variables in ss,
-- the worst case is that the usage is all variables in ss.
deadLoop :: (Ord n) => Usage n -> Statement a n p -> (Usage n, Statement a n p)
deadLoop us ss
 = let (sU, sS) = deadS us ss
   in  if   sU == us
       then (sU, sS)
       else deadLoop sU sS


usageX :: (Ord n) => Exp a n p -> Usage n
usageX x = Usage mempty (freevars x)

usedX :: (Ord n) => Name n -> Usage n -> Bool
usedX n us = Set.member n (usageExp us)

usageA :: (Ord n) => Name n -> Usage n
usageA n = Usage (Set.singleton n) mempty

usedA :: (Ord n) => Name n -> Usage n -> Bool
usedA n us = Set.member n (usageAcc us)


killAccumulator :: (Ord n) => Name n -> Exp a n p -> Statement a n p -> Statement a n p
killAccumulator acc xx statements
 = runIdentity
 $ transformUDStmt trans () statements
 where
  trans _ s
   | Read n acc' _ ss <- s
   , acc == acc'
   = return ((), Let n xx ss)
   | Write acc' _ <- s
   , acc == acc'
   = return ((), mempty)
   | LoadResumable acc' _ <- s
   , acc == acc'
   = return ((), mempty)
   | SaveResumable acc' _ <- s
   , acc == acc'
   = return ((), mempty)

   | otherwise
   = return ((), s)
