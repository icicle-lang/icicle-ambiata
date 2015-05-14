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
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Simp.ANormal (
      anormal
    ) where

import Icicle.Common.Base
import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds
import Icicle.Common.Fresh

import P
import Data.List (unzip)

import qualified    Data.Set    as Set

-- | A-normalise an expression.
-- We need a fresh name supply.
anormal :: Ord n => Exp n p -> Fresh n (Exp n p)
anormal xx
 = do   (bs, x)  <- anormal' xx
        (bs',x') <- renames bs [] x
        return $ makeLets bs' x'
 where

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
  renames [] seen  x
   = return (seen, x)

  renames ((n,b):bs) seen x
   -- Check if n is used anywhere else
   | n `Set.member` allvars (makeLets bs x)
   = do n' <- fresh
        -- Lets are non-recursive, so "b" will not change.

        -- We need to convert the rest of the bindings into a
        -- series of lets that we can substitute on.
        --
        -- Note that mapping subst over (fmap snd bs) wouldn't work - because we'd lose
        -- valuable shadowing information from the names in (fmap fst bs).
        let lets = makeLets bs x
        -- Substitute with the new name
        lets' <- subst n (XVar n') lets

        -- It's silly, but we need to pull back out to a list of bindings again.
        -- We could save some work by going backwards, but this way seems simpler for now.
        let (bs',x') = takeLets lets'

        -- Proceed.
        renames bs' (seen <> [(n', b)]) x'

   -- The name isn't used elsewhere, so we can skip it
   | otherwise
   = renames bs (seen <> [(n,b)]) x


-- | Recursively pull out sub-expressions to be bound
anormal' :: Ord n => Exp n p -> Fresh n ([(Name n, Exp n p)], Exp n p)
anormal' xx
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
            (bf, xf)    <-      extractBinding f
            -- And with the arguments
            (bs, xs)    <- unzip <$> mapM extractBinding args

            -- Push all the bindings together, then reconstruct the new application
            return (concat (bf:bs), makeApps xf xs)

    -- Lambdas are barriers.
    -- We don't want to pull anything out.
    XLam n v x
     -> do  x' <- anormal x
            return ([], XLam n v x')

    -- Just recurse over lets
    XLet n x y
     -> do  (bx, x')    <- anormal' x
            (by, y')    <- anormal' y

            return (bx <> [(n, x')] <> by, y')

 where
  -- Return unchanged
  ret = return ([], xx)


-- | Extract bindings for part of an application expression.
-- If it's simple, just give it back.
-- If it's interesting, anormalise it and bind it to a fresh name
extractBinding :: Ord n => Exp n p -> Fresh n ([(Name n, Exp n p)], Exp n p)
extractBinding xx
 | isNormal xx
 = return ([], xx)
 | otherwise
 = do   (bs,x') <- anormal' xx
        n       <- fresh
        return (bs <> [(n,x')], XVar n)


-- | Check whether expression is worth extracting
isNormal :: Exp n p -> Bool
isNormal xx
 = case xx of
    XVar{}   -> True
    XPrim{}  -> True
    XValue{} -> True
    XLam{}   -> True
    XApp{}   -> False
    XLet{}   -> False
