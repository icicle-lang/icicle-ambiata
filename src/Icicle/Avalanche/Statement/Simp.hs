-- | Simplifying and transforming statements
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}

module Icicle.Avalanche.Statement.Simp (
    pullLets
  , forwardStmts
  , substXinS
  , thresher
  , nestBlocks
  , simpStatementExps
  ) where

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Avalanche.Statement.Simp.ExpEnv
import qualified    Icicle.Avalanche.Prim.Flat as F
import qualified    Icicle.Avalanche.Prim.Eval as AE

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.Fresh
import              Icicle.Common.Type
import              Icicle.Common.Value

import              P

import              Data.Functor.Identity
import qualified    Data.Set as Set
import qualified    Data.List as List



pullLets :: Statement a n p -> Statement a n p
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

      InitAccumulator (Accumulator n vt x) subs
       -> pres x $ \x' -> InitAccumulator (Accumulator n vt x') subs

      Write n x
       -> pres x $ Write n

      Output n t xs
       -> presN xs $ Output n t

      _
       -> return ((), s)

  pres x instmt
   = let (bs, x') = takeLets x
     in  return ((), foldr mkLet (instmt x') bs)

  pres2 x y instmt
   = let (bs, x') = takeLets x
         (cs, y') = takeLets y
     in  return ((), foldr mkLet (instmt x' y') (bs<>cs))

  presN xts instmt
   = let ts  = fmap snd xts
         bxs = fmap (takeLets . fst) xts
         bs  = concat (fmap fst bxs)
         xs  = List.zip (fmap snd bxs) ts
     in  return ((), foldr mkLet (instmt xs) bs)

  mkLet (n,x) s
   = Let n x s


-- | Let-forwarding on statements
forwardStmts :: Ord n => a -> Statement a n p -> Fresh n (Statement a n p)
forwardStmts a_fresh statements
 = transformUDStmt trans () statements
 where
  trans _ s
   = case s of
      Let n x ss
       | isSimpleValue x
       -> do    s' <- substXinS a_fresh n x ss
                return ((), s')

      _ -> return ((), s)


-- Substitute an expression into a statement.
--
-- It is important not to substitute shadowed variables:
-- > subst monkey := banana
-- > in (monkey + let monkey = not_banana in monkey)
-- here the inner "let monkey" is a different variable and must not be substituted.
--
-- This is incomplete, does not perform renaming.
-- > subst monkey := banana
-- > in (let banana = 5 in monkey)
-- here, simply substituting would change the meaning.
--
-- Instead we need to rename the local let
-- 
-- > let banana' = 5
-- > in subst monkey := banana
-- > in (subst banana := banana' in monkey)
--
substXinS :: Ord n => a -> Name n -> Exp a n p -> Statement a n p -> Fresh n (Statement a n p)
substXinS a_fresh name payload statements
 = transformUDStmt trans True statements
 where
  -- Do nothing; the variable has been shadowed
  trans False s
   = return (False, s)
  trans True s
   = case s of
      If x ss es
       -> sub1 x $ \x' -> If x' ss es

      Let n x ss
       | n == name
       -> do x' <- sub x
             finished (Let n x' ss)
       | Set.member n frees
       -> freshen n ss $ \n' ss' -> Let n' x ss'
       | otherwise
       -> sub1 x $ \x' -> Let n x' ss

      ForeachInts n from to ss
       | n == name
       -> finished s
       | Set.member n frees
       -> freshen n ss $ \n' ss' -> ForeachInts n' from to ss'
       | otherwise
       -> do    from' <- sub from
                to'   <- sub to
                return (True, ForeachInts n from' to' ss)

      InitAccumulator (Accumulator n vt x) ss
       -> sub1 x $ \x' -> InitAccumulator (Accumulator n vt x') ss

      Write n x
       -> sub1 x $ Write n

      Output n t xs
       -> subN xs $ Output n t

      Read n x z ss
       | n == name
       -> finished s
       | Set.member n frees
       -> freshen n ss $ \n' ss' -> Read n' x z ss'

      ForeachFacts ns x y ss
       | name `elem` fmap fst ns
       -> finished s
       | any (flip Set.member frees . fst) ns
       -> freshenForeach [] ns x y ss

      _
       -> return (True, s)

  sub = subst a_fresh name payload
  sub1 x f
   = do x' <- sub x
        return (True, f x')

  subN xts f
   = do let ts = fmap snd xts
        xs <- traverse (sub . fst) xts
        let xts' = List.zip xs ts
        return (True, f xts')

  finished s
   = return (False, s)

  freshen n ss f
   = do n' <- fresh
        ss' <- substXinS a_fresh n (XVar a_fresh n') ss
        trans True (f n' ss')

  freshenForeach ns [] x y ss
   = return (True, ForeachFacts ns x y ss)
  freshenForeach ns ((n,t):ns') x y ss
   = do n'  <- fresh
        ss' <- substXinS a_fresh n (XVar a_fresh n') ss
        freshenForeach (ns <> [(n',t)]) ns' x y ss'

  frees = freevars payload


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
thresher :: (Ord n, Eq p) => a -> Statement a n p -> Fresh n (Statement a n p)
thresher a_fresh statements
 = transformUDStmt trans emptyExpEnv statements
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
       -> return (env, Let n (XVar a_fresh n') ss)

      -- Read that's never used
      Read n _ _ ss
       | not $ Set.member n $ stmtFreeX ss
       -> return (env, ss)

      InitAccumulator (Accumulator n _ x) ss
       |  not (accRead $ accumulatorUsed n ss) || not (accWritten $ accumulatorUsed n ss)
       -> do    n' <- fresh
                let ss' = Let n' x (killAccumulator n (XVar a_fresh n') ss)
                return (env, ss')

      -- Anything else, we just update environment and recurse
      _
       -> return (updateExpEnv s env, s)


-- | Check whether a statement writes to any accumulators or returns a value.
-- If it does not update or return, it is probably dead code.
--
-- The first argument is a set of accumulators to ignore.
hasEffect :: Ord n => Statement a n p -> Bool
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

    -- Outputting is an effect
   | Output _ _ _       <- s
   = return True

    -- Marking a fact as used is an effect.
   | KeepFactInHistory  <- s
   = return True

   | LoadResumable _ _  <- s
   = return True
   | SaveResumable _ _  <- s
   = return True


   -- If any substatements have an effect, the superstatement does.
   | otherwise
   = return r


-- | Find free *expression* variables in statements.
-- Note that this ignores accumulators, as they are a different scope.
stmtFreeX :: Ord n => Statement a n p -> Set.Set (Name n)
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
          Read n _ _ _
           -> return (Set.delete n subvars)

          -- Leaves that use expressions.
          -- Here, the name is an accumulator variable, so doesn't affect expressions.
          Write _ x
           -> ret x
          Output _ _ xs
           -> return (Set.unions (fmap (freevars . fst) xs) `Set.union` subvars)

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
nestBlocks :: Ord n => a -> Statement a n p -> Fresh n (Statement a n p)
nestBlocks a_fresh statements
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

  goBlock (Read nx nacc vt inner : baloney : ls) pres
   = do (nx',inner') <- maybeRename nx baloney inner
        goBlock (Read nx' nacc vt (inner' <> baloney) : ls) pres

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
        inner'  <- substXinS a_fresh n (XVar a_fresh n') inner
        return (n', inner')

   | otherwise
   =    return (n, inner)



data AccumulatorUsage
 = AccumulatorUsage
 { accRead    :: Bool
 , accWritten :: Bool }

-- | Check whether statement uses this accumulator
accumulatorUsed :: Ord n => Name n -> Statement a n p -> AccumulatorUsage
accumulatorUsed acc statements
 = runIdentity
 $ foldStmt down up ors () (AccumulatorUsage False False) statements
 where
  ors (AccumulatorUsage a b) (AccumulatorUsage c d) = AccumulatorUsage (a || c) (b || d)

  down _ _ = return ()

  up _ r s
   -- Writing or pushing is an effect,
   -- unless we're explicitly ignoring this accumulator
   | Write n _ <- s
   , n == acc
   = return (AccumulatorUsage True False)

   | Read _ n _ _ <- s
   , n == acc
   = return (ors r (AccumulatorUsage False True))

   | otherwise
   = return r

killAccumulator :: (Ord n, Eq p) => Name n -> Exp a n p -> Statement a n p -> Statement a n p
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

-- | Simplify expressions with flattened primitives. Performs:
--    * constant folding for common expressions and flat prims.
--    * beta reduction
--
simpStatementExps :: Ord n => a -> Statement a n F.Prim -> Statement a n F.Prim
simpStatementExps a_fresh statements
 = runIdentity
 $ transformUDStmt trans () statements
 where
  trans _ ss
   = return . ((),)
   $ case ss of
      If x s1 s2            -> If (goX x) s1 s2
      Let n x s             -> Let n (goX x) s
      ForeachInts n x1 x2 s -> ForeachInts n (goX x1) (goX x2) s
      Write n x             -> Write n (goX x)
      InitAccumulator a s   -> InitAccumulator (goA a) s
      Output n t xs         -> Output n t (fmap (first goX) xs)
      _                     -> ss

  goA aa
   = aa { accInit = goX (accInit aa) }

  goX xx
   = case xx of
      XApp a p q
       | p' <- goX p
       , q' <- goX q
       , Just (prim, as) <- takePrimApps (XApp a p' q')
       , Just args       <- mapM (takeValue . goX) as
       -> fromMaybe (XApp a p' q') (goP prim args)

      XApp a p q
        -> XApp a (goX p) (goX q)

      XLam a n t x1
        -> XLam a n t (goX x1)

      XLet a n x1 x2
        -> XLet a n (goX x1) (goX x2)

      b@(XVar{})   -> b
      b@(XPrim{})  -> b
      b@(XValue{}) -> b

  goP p vs
   = case AE.evalPrim p vs of
      Right (VBase b)
       -> Just
        $ XValue a_fresh (functionReturns $ F.typeOfPrim p) b
      _ -> Nothing
