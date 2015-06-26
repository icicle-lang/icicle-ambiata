-- | Simplifying and transforming statements
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp (
    pullLets
  , forwardStmts
  , substXinS
  , thresher
  , nestBlocks
  ) where

import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.Fresh

import              P

import              Data.Functor.Identity
import              Data.List (reverse)
import qualified    Data.Set as Set



pullLets :: Statement n p -> Statement n p
pullLets statements
 = runIdentity
 $ transformUDStmt trans () statements
 where
  trans _ s
   = case s of
      If x subs elses
       -> pres x $ \x' -> If x' subs elses
      Let n x subs
       -> pres x $ \x' -> Let n x' subs

      ForeachInts n from to subs
       -> pres2 from to $ \from' to' -> ForeachInts n from' to' subs

      InitAccumulator (Accumulator n at vt x) subs
       -> pres x $ \x' -> InitAccumulator (Accumulator n at vt x') subs

      Write n x
       -> pres x $ Write n
      Push n x
       -> pres x $ Push n

      Return x
       -> pres x $ Return

      _
       -> return ((), s)

  pres x instmt
   = let (bs, x') = takeLets x
     in  return ((), foldr mkLet (instmt x') bs)

  pres2 x y instmt
   = let (bs, x') = takeLets x
         (cs, y') = takeLets y
     in  return ((), foldr mkLet (instmt x' y') (bs<>cs))

  mkLet (n,x) s
   = Let n x s


-- | Let-forwarding on statements
forwardStmts :: Ord n => Statement n p -> Fresh n (Statement n p)
forwardStmts statements
 = transformUDStmt trans () statements
 where
  trans _ s
   = case s of
      Let n x ss
       | isSimpleValue x
       -> do    s' <- substXinS n x ss
                return ((), s')
      
      _ -> return ((), s)


substXinS :: Ord n => Name n -> Exp n p -> Statement n p -> Fresh n (Statement n p)
substXinS name payload statements
 -- TODO name avoiding grr
 = transformUDStmt trans () statements
 where
  trans _ s
   = case s of
      If x ss es
       -> sub1 x $ \x' -> If x' ss es
      Let n x ss
       -> sub1 x $ \x' -> Let n x' ss

      ForeachInts n from to ss
       -> do    from' <- sub from
                to'   <- sub to
                return ((), ForeachInts n from' to' ss)

      InitAccumulator (Accumulator n at vt x) ss
       -> sub1 x $ \x' -> InitAccumulator (Accumulator n at vt x') ss

      Write n x
       -> sub1 x $ Write n
      Push  n x
       -> sub1 x $ Push n

      Return  x
       -> sub1 x $ Return

      _
       -> return ((), s)

  sub = subst     name payload
  sub1 x f
   = do x' <- sub x
        return ((), f x')


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
 = transformUDStmt trans [] statements
 where
  trans env s
   -- Check if it actually does anything:
   -- updates accumulators, returns a value, etc.
   -- If it doesn't, we might as well return a nop
   | not $ hasEffect s
   = return (env, mempty)

   | otherwise
   = case s of
      -- Unmentioned let - just return the substatement
      Let n x ss
       | not $ Set.member n $ stmtFreeX ss
       -> return (env, ss)
      -- Duplicate let: change to refer to existing one
       | ((n',_):_) <- filter (\(_,x') -> x `alphaEquality` x') env
       -> return (env, Let n (XVar n') ss)

      -- Normal let: remember the name and expression for later
       | otherwise
       -> return ((n,x) : clear n env, s)
      
      -- New variables are bound, so clear the environment
      ForeachInts n _ _ _
       -> return (clear n env, s)
      ForeachFacts n _ _
       -> return (clear n env, s)

      -- Read that's never used
      Read n _ ss 
       | not $ Set.member n $ stmtFreeX ss
       -> return (env, ss)
      -- Read is used, but we still need to clear the environment
       | otherwise
       -> return (clear n env, s)

      -- Anything else, we just recurse
      _
       -> return (env, s)

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
-- If it does not update or return, it is probably dead code.
--
-- The first argument is a set of accumulators to ignore.
hasEffect :: Ord n => Statement n p -> Bool
hasEffect statements
 = runIdentity
 $ foldStmt down up (||) Set.empty False statements
 where
  down ignore   s
   -- We can ignore the newly created var
   -- because any changes will go out of scope:
   -- externally any updates are pure.
   | InitAccumulator acc _ <- s
   = return $ Set.insert (accName acc) ignore
   | otherwise
   = return   ignore

  up   ignore r s
   -- Writing or pushing is an effect,
   -- unless we're explicitly ignoring this accumulator
   | Write n _ <- s
   = return $ not $ Set.member n ignore
   | Push  n _ <- s
   = return $ not $ Set.member n ignore

    -- A return is kind of an effect.
    -- Well, it means the returned value is visible from outside
    -- so maybe "potentially useful" is better than "effect"
   | Return _  <- s
   = return True

    -- Marking a fact as used is an effect.
   | KeepFactInHistory  <- s
   = return True


   -- If any substatements have an effect, the superstatement does.
   | otherwise
   = return r


-- | Find free *expression* variables in statements.
-- Note that this ignores accumulators, as they are a different scope.
stmtFreeX :: Ord n => Statement n p -> Set.Set (Name n)
stmtFreeX statements
 = runIdentity
 $ foldStmt down up Set.union () Set.empty statements
 where
  down _ _
   = return ()

  up _ subvars s
   = let ret x = return (freevars x `Set.union` subvars)
     in  case s of
          -- Simply recursion for most cases
          If x _ _
           -> ret x
          -- We want the free variables of x,
          -- but need to hide "n" from the free variables of ss:
          -- n is not free in there any more.
          Let n x _
           -> return (freevars x `Set.union` Set.delete n subvars)
          ForeachInts n x y _
           -> return (freevars x `Set.union` freevars y `Set.union` Set.delete n subvars)

          -- Accumulators are in a different scope,
          -- so the binding doesn't hide anything.
          -- Still check the initialise expression though.
          InitAccumulator acc _
           -> ret (accInit acc)

          -- We're binding a new expression variable from an accumulator.
          -- The accumulator doesn't matter - but we need to hide n from
          -- the substatement's free variables.
          Read n _ _
           -> return (Set.delete n subvars)

          -- Leaves that use expressions.
          -- Here, the name is an accumulator variable, so doesn't affect expressions.
          Write _ x
           -> ret x
          Push  _ x
           -> ret x
          Return x
           -> ret x

          -- Leftovers: just the union of the under bits
          _
           -> return subvars


-- | Nest blocks in further.
-- for example,
--
-- > Block [ Let x (Let y ...)
-- >       , baloney ]
--
-- could be translated to
--
-- > Let x $
-- > Block [ Let y ...
-- >       , baloney ]
-- 
-- this does not affect semantics so long as x isn't free in baloney.
-- In fact, it doesn't do anything except allowing thresher to
-- remove more duplicates.
--   
-- Note that the above example is only one step: nesting would then be
-- recursively performed etc.
nestBlocks :: Ord n => Statement n p -> Fresh n (Statement n p)
nestBlocks statements
 = transformUDStmt trans () statements
 where
  trans _ s
   = case s of
      Block bs
        -> goBlock (concatMap catBlock bs) []
      _ -> return ((), s)


  goBlock [] [pre]
   = return ((), pre)
  goBlock [] pres
   = return ((), Block $ reverse pres)

  goBlock (Let n x inner : baloney : ls) pres
   = do (n',inner') <- maybeRename n baloney inner
        goBlock (Let n' x (inner' <> baloney) : ls) pres

  goBlock (InitAccumulator acc inner : baloney : ls) pres
   = do goBlock (InitAccumulator acc (inner <> baloney) : ls) pres

  goBlock (Read nx nacc inner : baloney : ls) pres
   = do (nx',inner') <- maybeRename nx baloney inner
        goBlock (Read nx' nacc (inner' <> baloney) : ls) pres

  goBlock (skip : ls) pres
   = goBlock ls (skip : pres)

  -- concatMap catBlock
  -- should flatten out the nested blocks
  catBlock (Block bs)
   = bs
  catBlock b
   = [b]

  -- Check if we need to rename the let binding - and do it
  maybeRename n check inner
   | n `Set.member` stmtFreeX check
   = do n'      <- fresh
        inner'  <- substXinS n (XVar n') inner
        return (n', inner')

   | otherwise
   =    return (n, inner)


