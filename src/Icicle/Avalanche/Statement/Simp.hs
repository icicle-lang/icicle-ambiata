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


-- | Thresher transform - throw out the chaff.
-- Three things:
--  * Find let bindings that have already been bound:
--      keep an environment of previously bound expressions,
--      at each let binding check if it's already been seen.
--  * Remove let bindings that are not mentioned:
--      check freevariables of free statement.
--  * Remove some other useless code:
--      statements that do not update accumulators or return a value are silly.
--
thresher :: (Ord n, Eq p) => Statement n p -> Fresh n (Statement n p)
thresher statements
 = go [] statements
 where
  go env s
   -- Check if it actually does anything:
   -- updates accumulators, returns a value, etc
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
       -> Let n x <$> go ((n,x): clear n env) ss
      
      ForeachInts n from to ss 
       -> ForeachInts n from to <$> go (clear n env) ss
      ForeachFacts n ty ss 
       -> ForeachFacts n ty <$> go (clear n env) ss

      Block ss
       -> Block <$> mapM (go env) ss

      InitAccumulator acc ss
       -> InitAccumulator acc <$> go env ss

      Read n acc ss 
       | not $ Set.member n $ stmtFreeX ss
       -> go env ss
       | otherwise
       -> Read n acc <$> go (clear n env) ss

      Write n x
       -> return $ Write n x
      Push n x
       -> return $ Push n x
      Return x
       -> return $ Return x

  -- The environment stores previously bound expressions.
  -- These expressions can refer to names that are bound upwards.
  -- This would be fine if there were no shadowing, but with shadowing we may end up
  -- rebinding a name that is mentioned in another expression:
  --
  -- let a = 10
  -- let b = a + 1
  -- let a = 8
  -- let s = a + 1
  -- in  s
  --
  -- Here, s and b are locally alpha equivalent, but not really equivalent because they
  -- refer to different "a"s.
  -- When we see the second "a" binding, then, we must remove "b" from the environment of
  -- previously bound expressions.
  --
  clear n env
   = filter (\(n',x') -> n' /= n && not (Set.member n $ freevars x')) env


-- | Check whether a statement writes to any accumulators or returns a value.
-- The first argument is a set of accumulators to ignore.
-- If it does not update or return, it is probably dead code.
hasEffect :: Ord n => Set.Set (Name n) -> Statement n p -> Bool
hasEffect ignore s
 = case s of
    -- If any substatements have an effect, the superstatement does.
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

    -- We can ignore the newly created var
    -- because any changes will go out of scope:
    -- externally any updates are pure.
    InitAccumulator acc ss
     -> hasEffect (Set.insert (accName acc) ignore) ss

    -- Reading is fine on its own
    Read _ _ ss
     -> go ss

    -- Writing or pushing is an effect,
    -- unless we're explicitly ignoring this accumulator
    Write n _
     -> not $ Set.member n ignore
    Push  n _
     -> not $ Set.member n ignore

    -- A return is kind of an effect.
    -- Well, it means the returned value is visible from outside
    -- so maybe "potentially useful" is better than "effect"
    Return _
     -> True
 where
  go = hasEffect ignore


-- | Find free *expression* variables in statements.
-- Note that this ignores accumulators, as they are a different scope.
stmtFreeX :: Ord n => Statement n p -> Set.Set (Name n)
stmtFreeX s
 = case s of
    -- Simple recursion for most cases
    If x ss es
     -> freevars x `Set.union` stmtFreeX ss `Set.union` stmtFreeX es
    -- We want the free variables of x,
    -- but need to hide "n" from the free variables of ss:
    -- n is not free in there any more.
    Let n x ss
     -> freevars x `Set.union`
        Set.delete n (stmtFreeX ss)

    -- More boilerplate. I should do something about this.
    ForeachInts n x y ss
     -> freevars x `Set.union` freevars y `Set.union`
        Set.delete n (stmtFreeX ss)
    ForeachFacts _ _ ss
     -> stmtFreeX ss
    Block ss
     -> Set.unions $ fmap stmtFreeX ss

    -- Accumulators are in a different scope,
    -- so the binding doesn't hide anything.
    -- Still check the initialise expression though.
    InitAccumulator acc ss
     -> freevars (accInit acc) `Set.union` stmtFreeX ss

    -- We're binding a new expression variable from an accumulator.
    -- The accumulator doesn't matter - but we need to hide n from
    -- the substatement's free variables.
    Read n _ ss
     -> Set.delete n (stmtFreeX ss)

    -- Leaves that use expressions.
    -- Here, the n is an accumulator variable, so doesn't affect expressions.
    Write _ x
     -> freevars x
    Push  _ x
     -> freevars x

    Return x
     -> freevars x
