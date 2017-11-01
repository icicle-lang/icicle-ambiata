-- | A-normalise expressions
-- A-normal form is where the function and arguments of an application
-- can only be simple expressions such as values, variables, and primitives,
-- and let bindings are serialised.
--
-- For example,
--
--   map (+(5-2)) [1,2,3]
--
-- would end up as something like
--
--   let a = 5 - 2
--   let b = (+a)
--   let c = [1,2,3]
--   in  map b c
--
-- Conversion to a-normal form is not a benefit in itself, but simplifies
-- later transformations.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE BangPatterns      #-}
module Icicle.Common.Exp.Simp.ANormal (
      anormal
    , anormalAllVars
    ) where

import Icicle.Common.Base
import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds
import Icicle.Common.Fresh

import P
import Data.List (unzip)
import Data.Hashable (Hashable)

import qualified    Data.Set    as Set

-- | A-normalise an expression.
-- We need a fresh name supply.
anormal :: (Hashable n, Eq n) => a -> Exp a n p -> Fresh n (Exp a n p)
anormal a_fresh xx
 -- Annotate each expression with all the variables underneath it,
 -- then perform a-normalisation, then throw away the annotations
 = do let !x  = allvarsExp xx
      !y     <- anormalAllVars a_fresh x
      let !z  = reannotX fst y
      return z


-- | A-normalise an expression, annotated with all the mentioned variables.
-- Computing the variable set once and reusing it is much faster than recomputing it each time.
-- However, it does complicate constructing new expressions.
-- We need to be careful when constructing expressions to return,
-- as their variable set may be used by a parent recursive call.
-- We can cheat a little with the variable set though:
-- * a superset is allowed, as it will just make renaming more pessimistic
-- * new fresh variables don't need to be mentioned, as they will not appear elsewhere as binders or mentioned
--
anormalAllVars :: (Hashable n, Eq n) => a -> Exp (Ann a n) n p -> Fresh n (Exp (Ann a n) n p)
anormalAllVars a_fresh xx
 = do   (bs, x)  <- pullSubExps a_fresh xx

        (ret,_) <- foldM insertBinding (x, snd $ annotOfExp x) (reverse bs)
        return ret

 where
  insertBinding (x,bs) (n,b)
   | n `Set.member` bs
   = do n' <- fresh
        let bs' = Set.insert n'
                $ Set.union (snd $ annotOfExp b) bs
        let a' = (a_fresh, bs')
        x' <- subst1 a' n (XVar a' n') x
        return (XLet a' n' b x', bs')

   | otherwise
   = do let bs' = Set.insert n
                $ Set.union (snd $ annotOfExp b) bs
        let a' = (a_fresh, bs')
        let x' = XLet a' n b x
        return (x', bs)

-- | Recursively pull out sub-expressions to be bound
pullSubExps :: (Hashable n, Eq n) => a -> Exp (Ann a n) n p -> Fresh n ([(Name n, Exp (Ann a n) n p)], Exp (Ann a n) n p)
pullSubExps a_fresh xx
 = case xx of
    -- Values and other simple expressions can be left alone.
    XVar{}
     -> ret
    XPrim{}
     -> ret
    XValue{}
     -> ret

    -- An application - might as well do it in one go
    XApp{}
     -> do  -- Grab the function and its arguments
            let (f,args) = takeApps xx
            -- Recurse over the function and grab its bindings, if any
            (bf, xf)    <- extractBinding a_fresh f
            -- And with the arguments
            (bs, xs)    <- unzip <$> mapM (extractBinding a_fresh) args

            -- Reconstruct the new application, using the non-binding bits.
            -- Its variable set is really the union of all the non-binding bits,
            -- but the original expressions' variable set is a good approximation.
            let vars' = snd $ annotOfExp xx
            let app'  = makeApps (a_fresh, vars') xf xs
            -- Push all the bindings together
            return (concat (bf:bs), app')

    -- Lambdas are barriers.
    -- We don't want to pull anything out.
    XLam a n v x
     -> do  x' <- anormalAllVars a_fresh x
            return ([], XLam a n v x')

    -- Just recurse over lets
    XLet _ n x y
     -> do  (bx, x')    <- pullSubExps a_fresh x
            (by, y')    <- pullSubExps a_fresh y

            return (bx <> [(n, x')] <> by, y')

 where
  -- Return unchanged
  ret = return ([], xx)


-- | Extract bindings for part of an application expression.
-- If it's simple, just give it back.
-- If it's interesting, anormalise it and bind it to a fresh name
extractBinding :: (Hashable n, Eq n) => a -> Exp (Ann a n) n p -> Fresh n ([(Name n, Exp (Ann a n) n p)], Exp (Ann a n) n p)
extractBinding a_fresh xx
 | isNormal xx
 = return ([], xx)
 | otherwise
 = do   (bs,x') <- pullSubExps a_fresh xx
        n       <- fresh
        -- The annotation here should strictly be singleton of n,
        -- however since it is fresh it won't conflict with other bindings.
        -- Empty might be faster.
        return (bs <> [(n,x')], XVar (a_fresh, Set.empty) n)


-- | Check whether expression is worth extracting
isNormal :: Exp a n p -> Bool
isNormal xx
 = case xx of
    XVar{}   -> True
    XPrim{}  -> True
    XValue{} -> True
    XLam{}   -> True
    XApp{}   -> False
    XLet{}   -> False
