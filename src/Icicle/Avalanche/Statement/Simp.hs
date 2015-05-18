-- | Simplifying and transforming statements
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp (
    pullLets
  , forwardStmts
  , substXinS
  , thresher
  ) where

import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.Fresh

import              P

import qualified    Data.Set as Set



pullLets :: Statement n p -> Statement n p
pullLets stm
 = case stm of
    If x subs elses
     -> pres x (\x' -> If x' (pullLets subs) (pullLets elses))
    Let n x subs
     -> pres x (\x' -> Let n x' $ pullLets subs)

    ForeachInts n from to subs
     -> pres from
     $ \from'
     -> pres to
     $ \to'
     -> ForeachInts n from' to' $ pullLets subs

    ForeachFacts n ty subs
     -> ForeachFacts n ty $ pullLets subs
     
    Block subs
     -> Block (fmap pullLets subs)

    InitAccumulator (Accumulator n at vt x) subs
     -> pres x (\x' -> InitAccumulator (Accumulator n at vt x') $ pullLets subs)

    Read n acc subs
     -> Read n acc (pullLets subs)

    Write n x
     -> pres x (Write n)
    Push n x
     -> pres x (Push n)

    Return x
     -> pres x (\x' -> Return x')

 where
  pres x instmt
   = let (bs, x') = takeLets x
     in  foldr mkLet (instmt x') bs

  mkLet (n,x) s
   = Let n x s


-- | Let-forwarding on statements
forwardStmts :: Ord n => Statement n p -> Fresh n (Statement n p)
forwardStmts s
 = case s of
    If x ss es
     -> If x    <$> go ss <*> go es

    Let n x ss
     | isSimpleValue x
     -> substXinS n x ss

     | otherwise
     -> Let n x <$> go ss

    ForeachInts n from to ss
     -> ForeachInts n from to <$> go ss

    ForeachFacts n ty ss
     -> ForeachFacts n ty <$> go ss

    Block ss
     -> Block <$> mapM go ss

    InitAccumulator acc ss
     -> InitAccumulator acc <$> go ss

    Read n acc ss
     -> Read n acc <$> go ss 

    Write n x
     -> return $ Write n x
    Push  n x
     -> return $ Push  n x

    Return  x
     -> return $ Return  x

 where
  go = forwardStmts


substXinS :: Ord n => Name n -> Exp n p -> Statement n p -> Fresh n (Statement n p)
substXinS name payload s
 = case s of
    If x ss es
     -> If <$> sub x <*> go ss <*> go es
    Let n x ss
     -- TODO name avoiding grr
     -> Let n <$> sub x <*> go ss

    ForeachInts n from to ss
     -> ForeachInts n <$> sub from <*> sub to <*> go ss

    ForeachFacts n ty ss
     -> ForeachFacts n ty <$> go ss

    Block ss
     -> Block <$> mapM go ss

    InitAccumulator (Accumulator n at vt x) ss
     -> InitAccumulator <$> (Accumulator n at vt <$> sub x) <*> go ss

    Read n acc ss
     -> Read n acc <$> go ss

    Write n x
     -> Write n <$> sub x
    Push  n x
     -> Push  n <$> sub x

    Return  x
     -> Return  <$> sub x

 where
  sub = subst     name payload
  go  = substXinS name payload


-- | Three things?
-- * Find let bindings that have already been bound
-- * Remove let bindings that are not mentioned
-- * Remove some other useless code
thresher :: (Ord n, Eq p) => Statement n p -> Fresh n (Statement n p)
thresher statements
 = go [] statements
 where
  go env s
   | not $ hasEffect Set.empty s
   = return mempty
   | otherwise
   = case s of
      If x ss es
       -> If x <$> go env ss <*> go env es
      Let n x ss
       | not $ Set.member n $ stmtFreeX ss
       -> go env ss
       | ((n',_):_) <- filter (\(_,x') -> x `alphaEquality` x') env
       -> Let n (XVar n') <$> go env ss

       | otherwise
       -> Let n x <$> go ((n,x):env) ss
      
      ForeachInts n from to ss 
       -> ForeachInts n from to <$> go env ss
      ForeachFacts n ty ss 
       -> ForeachFacts n ty <$> go env ss

      Block ss
       -> Block <$> mapM (go env) ss

      InitAccumulator acc ss
       -> InitAccumulator acc <$> go env ss

      Read n acc ss 
       | not $ Set.member n $ stmtFreeX ss
       -> go env ss
       | otherwise
       -> Read n acc <$> go env ss

      Write n x
       -> return $ Write n x
      Push n x
       -> return $ Push n x
      Return x
       -> return $ Return x


hasEffect :: Ord n => Set.Set (Name n) -> Statement n p -> Bool
hasEffect ignore s
 = case s of
    If _ ss es
     -> go ss || go es
    Let _ _ ss
     -> go ss
    ForeachInts _ _ _ ss
     -> go ss
    ForeachFacts _ _ ss
     -> go ss
    Block ss
     -> any go ss
    -- So, we can ignore the newly created var
    -- because any changes will go out of scope!
    InitAccumulator acc ss
     -> hasEffect (Set.insert (accName acc) ignore) ss

    Read _ _ ss
     -> go ss

    Write n _
     -> not $ Set.member n ignore
    Push  n _
     -> not $ Set.member n ignore

    Return _
     -- This is weird.
     -> True
 where
  go = hasEffect ignore


stmtFreeX :: Ord n => Statement n p -> Set.Set (Name n)
stmtFreeX s
 = case s of
    If x ss es
     -> freevars x `Set.union` stmtFreeX ss `Set.union` stmtFreeX es
    Let n x ss
     -> freevars x `Set.union`
        Set.delete n (stmtFreeX ss)
    ForeachInts n x y ss
     -> freevars x `Set.union` freevars y `Set.union`
        Set.delete n (stmtFreeX ss)
    ForeachFacts _ _ ss
     -> stmtFreeX ss
    Block ss
     -> Set.unions $ fmap stmtFreeX ss

    -- Accumulators are in a different scope
    InitAccumulator _ ss
     -> stmtFreeX ss

    -- this is binding a new var..
    Read n _ ss
     -> Set.delete n (stmtFreeX ss)

    Write n x
     -> Set.insert n (freevars x)
    Push  n x
     -> Set.insert n (freevars x)

    Return x
     -> freevars x
