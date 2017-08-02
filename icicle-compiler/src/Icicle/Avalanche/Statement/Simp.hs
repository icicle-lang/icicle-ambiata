-- | Simplifying and transforming statements
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Icicle.Avalanche.Statement.Simp (
    pullLets
  , convertValues
  , forwardStmts
  , renameReads
  , substXinS
  , substXinS'
  , thresherWithAlpha, thresherNoAlpha
  , nestBlocks
  , dead
  , killNoEffect
  , stmtFreeX, stmtFreeX'
  , freevarsStmt
  ) where

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Avalanche.Statement.Simp.Dead
import              Icicle.Avalanche.Statement.Simp.ExpEnv
import              Icicle.Avalanche.Statement.Simp.ThreshOrd
import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Annot

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.FixT
import              Icicle.Common.Fresh
import              Icicle.Common.Type

import              P

import              Data.Functor.Identity
import              Data.Set (Set)
import qualified    Data.Set as Set
import qualified    Data.Map as Map
import qualified    Data.List as List
import              Data.Hashable (Hashable)

import              Control.Monad.Trans.Class


convertValues :: (Eq n) => a -> Statement (Ann a n) n Prim -> Statement (Ann a n) n Prim
convertValues a_fresh statements
 = runIdentity
 $ transformUDStmt trans () statements
 where
  trans _ s
   = case s of
      If  x subs elses
       -> go $ If (goX x) subs elses
      Let n x subs
       -> go $ Let n (goX x) subs
      ForeachInts t n from to subs
       -> go $ ForeachInts t n (goX from) (goX to) subs
      InitAccumulator acc subs
       -> go $ InitAccumulator (acc { accInit = goX $ accInit acc }) subs
      Write n x
       -> go $ Write n (goX x)
      Output n t xts
       -> go $ Output n t (fmap (first goX) xts)
      _ -> return ((), s)

  go x
    = return ((), x)

  goX xx
   = case xx of
       XValue _ (BufT n t) v
        -> goB xx n t v
       XValue _ (ArrayT t) v
        -> goA xx t v
       XApp a x1 x2
        -> XApp a (goX x1) (goX x2)
       XLam a n t x
        -> XLam a n t (goX x)
       XLet a n x1 x2
        -> XLet a n (goX x1) (goX x2)
       _ -> xx

  goB xx n t v
   = case v of
       VBuf buf
         -> bufPrim n t (reverse buf)
       _ -> xx

  goB' t v
   | BufT n t' <- t
   = goB (xValue t v) n t' v
   | ArrayT t' <- t
   = goA (xValue t v) t' v
   | otherwise
   = xValue t v

  goA xx t v
   = case v of
       VArray arr
         -> arrPrim 0 (xValue IntT (VInt (length arr))) t arr
       _ -> xx

  bufPrim n t b
   = case b of
       []
         -> xPrim (PrimBuf (PrimBufMake n t))
              `xApp`  xValue UnitT VUnit
       (x : xs)
         -> xPrim (PrimBuf (PrimBufPush n t))
              `xApp` bufPrim n t xs
              -- Allow for nested buffers
              `xApp` goB' t x

  arrPrim i n t a
    = case a of
         []
           -> xPrim (PrimUnsafe (PrimUnsafeArrayCreate t))
               `xApp` n
         (x : xs)
           -- This can mutate since it's on a newly created array
           -> xPrim (PrimArray  (PrimArrayPutMutable t))
                `xApp` arrPrim (i + 1) n t xs
                `xApp` (xValue IntT (VInt i))
                `xApp` goB' t x

  xApp x1 x2
   = XApp (a_fresh, snd (annotOfExp x1) <> snd (annotOfExp x2)) x1 x2
  xPrim
   = XPrim (a_fresh, Set.empty)
  xValue
   = XValue (a_fresh, Set.empty)


pullLets :: Statement (Ann a n) n p -> Statement (Ann a n) n p
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

      ForeachInts t n from to subs
       -> pres2 from to $ \from' to' -> ForeachInts t n from' to' subs

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
forwardStmts
  :: forall a n p. (Hashable n, Eq n)
  => a
  -> Statement (Ann a n) n p
  -> FixT (Fresh n) (Statement (Ann a n) n p)
forwardStmts a_fresh statements
 = transformUDStmt trans Map.empty statements
 where
  sub e x
   = lift $ substAnn a_fresh e x

  subS e x f
   = do x' <- sub e x
        return (e, f x')

  trans e s
   = case s of
      Let n x ss
       | isSimpleValue x
       -> do  x' <- sub e x
              progress (Map.insert n x' e, Block [ss])
       | otherwise
       -> do  x' <- sub e x
              return (Map.delete n e, Let n x' ss)

      If x ss es
       -> subS e x $ \x' -> If x' ss es

      While t n nt end ss
       -> do end'  <- sub e end
             return (e, While t n nt end' ss)

      ForeachInts t n from to ss
       -> do from' <- sub e from
             to'   <- sub e to
             let e' = Map.delete n e
             return (e', ForeachInts t n from' to' ss)

      InitAccumulator (Accumulator n vt x) ss
       -> subS e x $ \x' -> InitAccumulator (Accumulator n vt x') ss

      Write n x
       -> subS e x $ Write n

      Output n t xs
       -> do let subF (x,t') = (,t') <$> sub e x
             xs' <- mapM subF xs
             return (e, Output n t xs')

      Read n _acc _t _ss
       -> return (Map.delete n e, s)
      ForeachFacts ns _t _ss
       -> return (foldl (flip Map.delete) e (fmap fst $ factBindsAll ns), s)
      Block{}
       -> return (e,s)
      LoadResumable{}
       -> return (e,s)
      SaveResumable{}
       -> return (e,s)

-- | Funky renaming for C
-- Rename reads from accumulators to refer to the accumulator name.
-- This is an awful hack, but means that instead of outputting in the C version
--
-- > read     = acc$read                /* Read */
-- > acc$read = buf_push(read, ...)     /* Write */
--
-- we should end up with
--
-- > acc$read = acc$read                /* Read */
-- > acc$read = buf_push(acc$read, ...) /* Write */
--
-- and the C compiler should be able to get rid of the first, etc.
--
renameReads
 :: (Hashable n, Eq n)
 => a
 -> Statement (Ann a n) n p
 -> FixT (Fresh n) (Statement (Ann a n) n p)
renameReads a_fresh statements
 = transformUDStmt trans () statements
 where
  xVar n
   = XVar (a_fresh, Set.singleton n) n
  trans _ s
   = case s of
      Read nm acc vt ss
       | Just (pre, post) <- splitWrite acc mempty ss
       , nm /= acc
       , not $ Set.member nm $ stmtFreeX post
       -> do    pre' <- lift $ substXinS a_fresh nm (xVar acc) pre
                progress ((), Read acc acc vt (pre' <> post))
      _ -> return ((), s)

  splitWrite acc seen (Write acc' xx)
   | acc == acc'
   = Just (seen <> Write acc xx, mempty)

  splitWrite acc seen (Block (ss:rest))
   | Just (pre,post) <- splitWrite acc seen ss
   = Just (pre, post <> Block rest)

  splitWrite acc seen (Read nm' acc' vt' ss')
   | acc /= acc'
   , Just (pre,post) <- splitWrite acc seen ss'
   , not $ Set.member nm' $ stmtFreeX pre
   = Just (pre, Read nm' acc' vt' post)

  splitWrite acc seen (Block (ss:rest))
   | Nothing <- splitWrite acc seen ss
   = splitWrite acc (seen <> ss) (Block rest)

  splitWrite _ _ _
   = Nothing


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
substXinS
 :: (Hashable n, Eq n)
 => a
 -> Name n
 -> Exp (Ann a n) n p
 -> Statement (Ann a n) n p
 -> Fresh n (Statement (Ann a n) n p)
substXinS a_fresh name payload statements
 = transformUDStmt trans True statements
 where
  xVar n
   = XVar (a_fresh, Set.singleton n) n
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

      ForeachInts t n from to ss
       | n == name
       -> finished s
       | Set.member n frees
       -> freshen n ss $ \n' ss' -> ForeachInts t n' from to ss'
       | otherwise
       -> do    from' <- sub from
                to'   <- sub to
                return (True, ForeachInts t n from' to' ss)

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

      ForeachFacts binds@(FactBinds ntime ns) vt ss
       | name `elem` fmap fst (factBindsAll binds)
       -> finished s
       | any (flip Set.member frees . fst) (factBindsAll binds)
       -> do ntime'  <- fresh
             let subF n n' = substXinS a_fresh n (xVar n')
             ss'     <- subF ntime  ntime' ss
             freshenForeach ntime' [] ns vt ss'

      _
       -> return (True, s)

  sub = substAnn a_fresh (Map.singleton name payload)
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
        ss' <- substXinS a_fresh n (xVar n') ss
        trans True (f n' ss')

  freshenForeach ntime ns [] vt ss
   = return (True, ForeachFacts (FactBinds ntime ns) vt ss)
  freshenForeach ntime ns ((n,t):ns') vt ss
   = do n'  <- fresh
        ss' <- substXinS a_fresh n (xVar n') ss
        freshenForeach ntime (ns <> [(n',t)]) ns' vt ss'

  frees = freevars payload

-- | Like above, but the set of variables in the annotation isn't important.
substXinS'
 :: (Hashable n, Eq n)
 => a
 -> Name n
 -> Exp a n p
 -> Statement a n p
 -> Fresh n (Statement a n p)
substXinS' a_fresh name payload statements
  =   transformX return (return . reannotX ann) statements
  >>= substXinS a_fresh name (reannotX ann payload)
  >>= transformX return (return . reannotX fst)
  where ann a = (a, Set.empty)

-- | Thresher transform - throw out the chaff.
--  * Find let bindings that have already been bound:
--      keep an environment of previously bound expressions,
--      at each let binding check if it's already been seen.
--  * Remove some other useless code:
--      statements that do not have any external effect are silly
--  * Constant folding for ifs
--
thresherNoAlpha
 :: (Hashable n, Eq n, Ord a)
 => a
 -> Statement (Ann a n) n Prim
 -> FixT (Fresh n) (Statement (Ann a n) n Prim)
thresherNoAlpha a_fresh statements
 = transformUDStmt trans Map.empty statements
 where
  xVar n
   = XVar (a_fresh, Set.singleton n) n
  trans env s
   = case s of
      Let n x ss
      -- Duplicate let: change to refer to existing one
       | Just n' <- Map.lookup (ThreshMapOrd x) env
       -> progress (env, Let n (xVar n') ss)
       | otherwise
       -> return (Map.insert (ThreshMapOrd x) n env, s)

      If (XValue _ _ (VBool b)) t f
       -> let s' = if b then t else f
          in  progress (env, s')

      -- Anything else, we just recurse
      _
       -> return (env, s)

thresherWithAlpha
 :: (Hashable n, Eq n, Ord p)
 => a
 -> Statement (Ann a n) n p
 -> FixT (Fresh n) (Statement (Ann a n) n p)
thresherWithAlpha a_fresh statements
 = transformUDStmt trans emptyExpEnv statements
 where
  xVar n
   = XVar (a_fresh, Set.singleton n) n
  trans env s
   = case s of
      Let n x ss
      -- Duplicate let: change to refer to existing one
      -- I tried to use simple equality for simpFlattened since expressions cannot contain lambdas, but it was slower. WEIRD.
       | ((n',_):_) <- filter (\(_,x') -> x `alphaEquality` x') $ Map.toList env
       -> progress (env, Let n (xVar n') ss)

      If (XValue _ _ (VBool b)) t f
       -> let s' = if b then t else f
          in  progress (updateExpEnv s' env, s')

      -- Anything else, we just update environment and recurse
      _
       -> return (updateExpEnv s env, s)


freevarsStmt
  :: (Hashable n, Eq n)
  => Statement a n p
  -> Statement (a, Set (Name n)) n p
freevarsStmt = go
 where
  rmS n s
   = reannotS (\(a, set) -> (a, Set.delete n set)) s
  go xx
   = case xx of
       If x s1 s2
         -> If (freevarsExp x) (go s1) (go s2)
       Let n x s
         -> Let n (freevarsExp x) (rmS n $ go s)
       While t n nt x s
         -> While t n nt (freevarsExp x) (rmS n $ go s)
       ForeachInts t n x1 x2 s
         -> ForeachInts t n (freevarsExp x1) (freevarsExp x2) (rmS n $ go s)
       ForeachFacts ns t s
         -> ForeachFacts ns t (go s)
       Block ss
         -> Block $ fmap go ss
       InitAccumulator acc s
         -> InitAccumulator (freevarsAcc acc) (freevarsStmt s)
       Read n1 n2 t s
         -> Read n1 n2 t (rmS n1 $ freevarsStmt s)
       Write n x
         -> Write n (freevarsExp x)

       -- Anything else, we don't care, the transforms don't touch them
       Output n t xs     -> Output n t (fmap (first freevarsExp) xs)
       LoadResumable n t -> LoadResumable n t
       SaveResumable n t -> SaveResumable n t

freevarsAcc
  :: (Hashable n, Eq n)
  => Accumulator a n p
  -> Accumulator (a, Set (Name n)) n p
freevarsAcc acc
  = acc { accInit = freevarsExp (accInit acc) }


-- | Check whether a statement writes to any accumulators or returns a value.
-- If it does not update or return, it is probably dead code.
--
-- The first argument is a set of accumulators to ignore.
killNoEffect :: Eq n => Statement a n p -> Statement a n p
killNoEffect = fst . go Set.empty
  where
   go ignore ss
     = let subStmt s
             | !(_, hasEffect) <- go ignore s
             , ss'              <- if hasEffect then ss else mempty
             = (ss', hasEffect)
       in case ss of
         -- Looping over facts is an effect
         ForeachFacts _ _ _
           -> (ss, True)
         -- Writing or pushing is an effect,
         -- unless we're explicitly ignoring this accumulator
         Write n _
           | Set.member n ignore
           -> (ss, False)
           | otherwise
           -> (ss, True)
         Output _ _ _
           -> (ss, True)
         LoadResumable _ _
           -> (ss, True)
         SaveResumable _ _
           -> (ss, True)

         -- We can ignore the newly created var
         -- because any changes will go out of scope:
         -- externally any updates are pure.
         InitAccumulator n s
           -> let !ignore'         = Set.insert (accName n) ignore
                  !(s', hasEffect) = go ignore' s
                  ss'              = if hasEffect then ss else InitAccumulator n s'
              in (ss', hasEffect)

         -- If any substatements have an effect, the superstatement does.
         If x s1 s2
           -> let !(s1', hasEffect1) = go ignore s1
                  !(s2', hasEffect2) = go ignore s2

                  hasEffect = hasEffect1 || hasEffect2
                  ss'       = case (hasEffect1, hasEffect2) of
                                (True, True)   -> ss
                                (False, False) -> mempty
                                _              -> If x s1' s2'
              in (ss', hasEffect)
         Let _ _ s
           -> subStmt s
         While _ _ _ _ s
           -> subStmt s
         ForeachInts _ _ _ _ s
           -> subStmt s
         Block s
           -> let s'        = fmap (go ignore) s
                  hasEffect = List.or (fmap snd s')
                  ss'       = if hasEffect then Block (fmap fst s') else mempty
              in (ss', hasEffect)
         Read _ _ _ s
          -> subStmt s


-- | Find free *expression* variables in statements.
-- Note that this ignores accumulators, as they are a different scope.
stmtFreeX :: (Hashable n, Eq n) => Statement a n p -> Set (Name n)
stmtFreeX = stmtFreeX_ freevars

stmtFreeX' :: Eq n => Statement (a, Set (Name n)) n p -> Set (Name n)
stmtFreeX' = stmtFreeX_ (snd . annotOfExp)

stmtFreeX_
 :: Eq n
 => (Exp a n p -> Set (Name n))
 -> Statement a n p
 -> Set (Name n)
stmtFreeX_ frees statements
 = runIdentity
 $ foldStmt down up Set.union () Set.empty statements
 where
  down _ _
   = return ()

  up _ subvars s
   = let ret x = return (frees x `Set.union` subvars)
     in  case s of
          -- Simply recursion for most cases
          If x _ _
           -> ret x
          -- We want the free variables of x,
          -- but need to hide "n" from the free variables of ss:
          -- n is not free in there any more.
          Let n x _
           -> return (frees x `Set.union` Set.delete n subvars)
          While _ n _ x _
           -> return (frees x `Set.union` Set.delete n subvars)
          ForeachInts _ n x y _
           -> return (frees x `Set.union` frees y `Set.union` Set.delete n subvars)

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
           -> return (Set.unions (fmap (frees . fst) xs) `Set.union` subvars)

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
nestBlocks
 :: a
 -> Statement (Ann a n) n p
 -> Fresh n (Statement (Ann a n) n p)
nestBlocks _ statements
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
   = goBlock (Let n x (inner <> baloney) : ls) pres

  goBlock (InitAccumulator acc inner : baloney : ls) pres
   = goBlock (InitAccumulator acc (inner <> baloney) : ls) pres

  goBlock (Read nx nacc vt inner : baloney : ls) pres
   = goBlock (Read nx nacc vt (inner <> baloney) : ls) pres

  goBlock (skip : ls) pres
   = goBlock ls (skip : pres)

  -- concatMap catBlock
  -- should flatten out the nested blocks
  catBlock (Block bs)
   = bs
  catBlock b
   = [b]

