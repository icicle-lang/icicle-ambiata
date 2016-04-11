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
 -- = reannotX fst <$> (anormalAllVars a_fresh $ allvarsExp xx)
 = do let !x = {-# SCC anormal_ann #-} allvarsExp xx
      !y <- {-# SCC anormal_actual #-} anormalAllVars a_fresh x
      let !z = {-# SCC anormal_reannot #-} reannotX fst y
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
 = do   (bs, x)  <- {-# SCC anormal_pullsubexps #-}pullSubExps a_fresh xx
        -- Get the union of all the variables
        let !allNames = {-# SCC anormal_varOfLets #-}varsOfLets bs x
        -- and rename the outside binds if they are used
        (bs',x') <- {-# SCC anormal_renames #-}renames allNames bs [] x

        -- Tag the result with the union of the original bindings (bs)
        -- as well as the new names of the bindings.
        let !allNames' = {-# SCC anormal_allnames #-} allNames <> Set.fromList (fmap fst bs')
        let !ret = {-# SCC anormal_mklets#-}makeLets (a_fresh, allNames') bs' x'
        return ret

 where
  -- All the variables inside the bindings
  varsOfLets bs x
   = Set.unions
   $ fmap (snd.annotOfExp)
   $ x : fmap snd bs

  -- The sub-expressions of x might already have let bindings, and we're
  -- going to pull those out.
  --
  -- However, this actually changes the scope of those let bindings
  -- as they will now encompass any bindings in other sub-expressions.
  --
  -- If the same name is used in multiple places, we'll introduce shadowing, which is
  -- outlawed by the type checker.
  --
  -- So, we look at each binding, and if it would shadow something, we rename it.
  renames _ [] seen  x
   = return (seen, x)

  renames allNames ((n,b):bs) seen x
   -- Check if n is used anywhere else, including as a later binding name
   |  n `Set.member` (allNames <> Set.fromList (fmap fst bs))
   = do n' <- fresh
        -- Lets are non-recursive, so "b" will not change.

        -- We need to convert the rest of the bindings into a
        -- series of lets that we can substitute on.
        --
        -- Note that mapping subst over (fmap snd bs) wouldn't work - because we'd lose
        -- valuable shadowing information from the names in (fmap fst bs).
        -- (that is, if one of bs is also named "n")
        let a_fresh' = (a_fresh, allNames)
        let lets = makeLets a_fresh' bs x
        -- Substitute with the new name
        lets' <- subst1 a_fresh' n (XVar a_fresh' n') lets

        -- It's silly, but we need to pull back out to a list of bindings again.
        -- We could save some work by going backwards, but this way seems simpler for now.
        let (bs',x') = takeLets lets'

        -- Proceed.
        renames allNames bs' (seen <> [(n', b)]) x'

   -- The name isn't used elsewhere, so we don't need to rename it. Add it unchanged
   | otherwise
   = renames allNames bs (seen <> [(n,b)]) x


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
