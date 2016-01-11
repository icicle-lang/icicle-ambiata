-- | Some useful things to do with expressions
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Compounds (
      makeApps
    , takeApps
    , takePrimApps
    , takeValue
    , makeLets
    , takeLets
    , freevars
    , allvars
    , substMaybe
    , subst
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp.Exp
import              Icicle.Common.Fresh
import              Icicle.Common.Value

import              P

import qualified    Data.Set    as Set
import qualified    Data.Map    as Map


-- | Apply an expression to any number of arguments
makeApps :: a -> Exp a n p -> [Exp a n p] -> Exp a n p
makeApps a f args
 = foldl (XApp a) f args


-- | Split an expression into its function part and any arguments applied to it.
-- If it's not an application, arguments will be empty.
takeApps :: Exp a n p -> (Exp a n p, [Exp a n p])
takeApps xx
 = case xx of
    XApp _ p q
     -> let (f,as) = takeApps p
        in  (f, as <> [q])
    _
     -> (xx, [])


-- | Check if an expression is a primitive application
takePrimApps :: Exp a n p -> Maybe (p, [Exp a n p])
takePrimApps xx
 = case takeApps xx of
    (XPrim _ p, args) -> Just (p, args)
    _                 -> Nothing


-- | Prefix an expression with some let bindings
makeLets :: a -> [(Name n, Exp a n p)] -> Exp a n p -> Exp a n p
makeLets a bs x
 = foldr (uncurry (XLet a)) x bs

-- | Pull out the top-level let bindings
takeLets :: Exp a n p -> ([(Name n, Exp a n p)], Exp a n p)
takeLets xx
 = case xx of
    XLet _ n x y
     -> let (bs, y') = takeLets y
        in  ((n,x) : bs, y')
    _
     -> ([], xx)


takeValue :: Exp a n p -> Maybe (Value a n p)
takeValue (XValue _ _ b) = Just (VBase b)
-- We're pulling out a lambda as a closure.
-- However, we're ignoring the closure's heap.
-- This is fine - if the lambda references anything outside,
-- it will not evaluate and so won't be simplified.
takeValue (XLam _ n _ x) = Just (VFun Map.empty n x)
-- I promise this is exhaustive.
takeValue  _             = Nothing


-- | Collect all free variables in an expression
-- i.e. those that are not bound by lets or lambdas.
freevars
        :: Ord n
        => Exp a n p
        -> Set.Set (Name n)
freevars xx
 = case xx of
    XVar _ n     -> Set.singleton n
    XPrim{}      -> Set.empty
    XValue{}     -> Set.empty
    XApp _ p q   -> freevars p <> freevars q
    XLam _ n _ x -> Set.delete n (freevars x)
    XLet _ n x y -> freevars x <> Set.delete n (freevars y)


-- | Collect all variable names in an expression:
-- free and bound
allvars
        :: Ord n
        => Exp a n p
        -> Set.Set (Name n)
allvars xx
 = case xx of
    XVar _ n     -> Set.singleton n
    XPrim{}      -> Set.empty
    XValue{}     -> Set.empty
    XApp _ p q   -> allvars p <> allvars q
    XLam _ n _ x -> Set.singleton n <> allvars x
    XLet _ n x y -> Set.singleton n <> allvars x <> allvars y


-- | Substitute an expression in, but if it would require renaming
-- just give up and return Nothing
substMaybe
        :: Ord n
        => Name n
        -> Exp a n p
        -> Exp a n p
        -> Maybe (Exp a n p)
substMaybe name payload into
 = go into
 where
  payload_free = freevars payload

  go xx
   = case xx of
      XVar _ n
       | n == name
       -> return payload
       | otherwise
       -> return xx
      XApp a p q
       -> XApp a <$> go p <*> go q

      XPrim{}
       -> return xx
      XValue{}
       -> return xx

      XLam a n t x
       | n == name
       -> return xx
       -- If the name clashes, we can't do anything
       | n `Set.member` payload_free
       -> Nothing

       -- Name is mentioned and no clashes, so proceed
       | otherwise
       -> XLam a n t <$> go x

      XLet a n x1 x2
       | n == name
       -> XLet a n <$> go x1 <*> return x2
       -- If the let's name clashes with the substitution we're trying to make
       -- and the *body* of the let needs to be substituted into,
       -- we cannot proceed.
       -- (It doesn't matter if the definition, x1, mentions name because "n" is not bound there)
       | (n `Set.member` payload_free)
       -> Nothing

       -- Proceed as usual
       | otherwise
       -> XLet a n <$> go x1 <*> go x2


-- | Substitute an expression in,
-- using fresh names to avoid capture
subst
        :: Ord n
        => a
        -> Name n
        -> Exp a n p
        -> Exp a n p
        -> Fresh n (Exp a n p)
subst a_fresh name payload into
 = go into
 where
  payload_free = freevars payload

  go xx
   = case xx of
      XVar _ n
       | n == name
       -> return payload
       | otherwise
       -> return xx
      XApp a p q
       -> XApp a <$> go p <*> go q

      XPrim{}
       -> return xx
      XValue{}
       -> return xx

      XLam a n t x
       -- If the name clashes, we need to rename n
       | (n `Set.member` payload_free) || n == name
       -> do    n' <- fresh
                x' <- subst a_fresh n (XVar a_fresh n') x
                XLam a n' t <$> go x'

       -- Name is mentioned and no clashes, so proceed
       | otherwise
       -> XLam a n t <$> go x

      XLet a n x1 x2
       -- If the let's name clashes with the substitution we're trying to make,
       -- we need to rename
       | (n `Set.member` payload_free) || n == name
       -> do    n'  <- fresh
                x2' <- subst a_fresh n (XVar a_fresh n') x2
                XLet a n' <$> go x1 <*> go x2'

       -- Proceed as usual
       | otherwise
       -> XLet a n <$> go x1 <*> go x2

