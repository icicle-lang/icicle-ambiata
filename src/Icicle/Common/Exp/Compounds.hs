-- | Some useful things to do with expressions
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns  #-}
module Icicle.Common.Exp.Compounds (
      makeApps
    , takeApps
    , takePrimApps
    , takeValue
    , makeLets
    , takeLets
    , freevars
    , freevarsExp
    , allvars
    , allvarsExp
    , substMaybe
    , subst1
    , subst
    , reannotX
    , eraseAnnotX
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp.Exp
import              Icicle.Common.Fresh
import              Icicle.Common.Value

import              P

import              Data.Set (Set)
import qualified    Data.Set     as Set
import qualified    Data.Map     as Map
import              Data.Hashable


-- | Apply an expression to any number of arguments
makeApps :: a -> Exp a n p -> [Exp a n p] -> Exp a n p
makeApps a f args
 = foldl' (XApp a) f args


-- | Split an expression into its function part and any arguments applied to it.
-- If it's not an application, arguments will be empty.
takeApps :: Exp a n p -> (Exp a n p, [Exp a n p])
takeApps xx
 = go xx []
 where
  go (XApp _ p q) args
   = go p (q : args)
  go f args
   = (f, args)


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

--------------------------------------------------------------------------------

freevarsExp
  :: (Hashable n, Eq n)
  => Exp a n p
  -> Exp (a, Set (Name n)) n p
freevarsExp xx
 = case xx of
    XVar   a n     -> XVar   (a, Set.singleton n) n
    XPrim  a p     -> XPrim  (a, Set.empty)       p
    XValue a t v   -> XValue (a, Set.empty)       t v
    XApp   a p q   -> let !p' = freevarsExp p
                          !q' = freevarsExp q
                          !a' = ann p' <> ann q'
                      in  XApp (a, a') p' q'
    XLam   a n t x -> let !x' = freevarsExp x
                          !a' = Set.delete n (ann x')
                      in  XLam (a, a') n t x'
    XLet   a n x y -> let !x' = freevarsExp x
                          !y' = freevarsExp y
                          !a' = ann x' <> Set.delete n (ann y')
                      in  XLet (a, a') n x' y'
  where ann = snd . annotOfExp

-- | Collect all free variables in an expression
-- i.e. those that are not bound by lets or lambdas.
freevars :: (Hashable n, Eq n)
         => Exp a n p
         -> Set (Name n)
freevars xx
 = case xx of
    XVar _ n     -> Set.singleton n
    XPrim{}      -> Set.empty
    XValue{}     -> Set.empty
    XApp _ p q   -> let !x = freevars p <> freevars q in x
    XLam _ n _ x -> Set.delete n (freevars x)
    XLet _ n x y -> let !a = freevars x <> Set.delete n (freevars y) in a


allvarsExp
  :: (Hashable n, Eq n)
  => Exp a n p
  -> Exp (a, Set (Name n)) n p
allvarsExp xx
 = case xx of
    XVar   a n     -> XVar   (a, Set.singleton n) n
    XPrim  a p     -> XPrim  (a, Set.empty)       p
    XValue a t v   -> XValue (a, Set.empty)       t v
    XApp   a p q   -> let !p' = allvarsExp p
                          !q' = allvarsExp q
                          !a' = ann p' <> ann q'
                      in  XApp (a, a') p' q'
    XLam   a n t x -> let !x' = allvarsExp x
                          !a' = Set.singleton n <> ann x'
                      in  XLam (a, a') n t x'
    XLet   a n x y -> let !x' = allvarsExp x
                          !y' = allvarsExp y
                          !a' = Set.singleton n <> ann x' <> ann y'
                      in  XLet (a, a') n x' y'
  where ann = snd . annotOfExp

-- | Collect all variable names in an expression:
-- free and bound
allvars :: (Hashable n, Eq n)
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

--------------------------------------------------------------------------------

-- | Substitute an expression in, but if it would require renaming
-- just give up and return Nothing
substMaybe :: (Hashable n, Eq n)
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
subst1 :: (Hashable n, Eq n)
      => a
      -> Name n
      -> Exp a n p
      -> Exp a n p
      -> Fresh n (Exp a n p)
subst1 a_fresh name payload into
 = subst a_fresh (Map.singleton name payload) into

-- | Substitute an expression in,
-- using fresh names to avoid capture
subst :: (Hashable n, Eq n)
      => a
      -> Map.Map (Name n) (Exp a n p)
      -> Exp a n p
      -> Fresh n (Exp a n p)
subst a_fresh env into
 = subst' a_fresh env (Set.unions $ Map.elems $ Map.map freevars env) into

subst'  :: (Hashable n, Eq n)
        => a
        -> Map.Map (Name n) (Exp a n p)
        -> Set.Set (Name n)
        -> Exp a n p
        -> Fresh n (Exp a n p)
subst' a_fresh env frees into
 = go into
 where
  go xx
   = case xx of
      XVar _ n
       | Just payload <- Map.lookup n env
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
       -- If the name is in the free set of all payloads or it *has* a payload,
       -- might as well rename it.
       | n `Set.member` frees || n `Map.member` env
       -> do  -- Generate fresh name and add to environment
              -- We do not actually need to insert n' into the free set
              -- because it is fresh, and cannot occur in the expression.
              n' <- fresh
              let env' = Map.insert n (XVar a_fresh n') env
              XLam a n' t <$> subst' a_fresh env' frees x

       -- Name is mentioned and no clashes, so proceed
       | otherwise
       -> XLam a n t <$> go x

      XLet a n x1 x2
       -- As with lambda
       | n `Set.member` frees || n `Map.member` env
       -> do  n'  <- fresh
              let env' = Map.insert n (XVar a_fresh n') env
              XLet a n' <$> go x1 <*> subst' a_fresh env' frees x2

       -- Proceed as usual
       | otherwise
       -> XLet a n <$> go x1 <*> go x2

reannotX :: (a -> a') -> Exp a n p -> Exp a' n  p
reannotX f xx
 = case xx of
    XVar    a n     -> XVar   (f a) n
    XPrim   a p     -> XPrim  (f a) p
    XValue  a t v   -> XValue (f a) t v
    XApp    a x1 x2 -> XApp   (f a) (reannotX f x1) (reannotX f x2)
    XLam    a n t x -> XLam   (f a) n t (reannotX f x)
    XLet    a n b x -> XLet   (f a) n (reannotX f b) (reannotX f x)

eraseAnnotX :: Exp a n p -> Exp () n p
eraseAnnotX = reannotX (const ())

