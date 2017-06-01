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
    , substAnn
    , reannotX
    , eraseAnnotX
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp.Exp
import              Icicle.Common.Fresh
import              Icicle.Common.Value

import              P

import              Data.Set (Set)
import              Data.Map (Map)
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

-- | Substitute multiple expressions.
subst :: (Hashable n, Eq n)
      => a
      -> Map (Name n) (Exp a n p)
      -> Exp a n p
      -> Fresh n (Exp a n p)
subst a_fresh env into
  = let ann   = const (a_fresh, Set.empty)
        env'  = fmap (reannotX ann) env
        into' = reannotX ann into
    in  reannotX fst <$> substAnn a_fresh env' into'

-- | Exactly as above, but keeping track of variables
--   under expressions as an annotation.
substAnn :: (Hashable n, Eq n)
         => a
         -> Map (Name n) (Exp (Ann a n) n p)
         -> Exp (Ann a  n) n p
         -> Fresh n (Exp (Ann a n) n p)
substAnn a_fresh envmap into
 | Map.null envmap
 = return into
 | otherwise
 -- Use the set of all variables as the "capture set" of what that needs to be renamed.
 -- Using the set of free variables here would be ok, except it leads to shadowing.
 = fst <$> subst' envmap (Set.unions $ Map.elems $ Map.map allvars envmap) into
 where
   xVar n
    = XVar (a_fresh, Set.singleton n) n
   xApp a x1 x2
    = XApp (a, snd (annotOfExp x1) <> snd (annotOfExp x2)) x1 x2
   xLam a n t x
    = XLam (a, snd (annotOfExp x)) n t x
   xLet a n x1 x2
    = XLet (a, snd (annotOfExp x1) <> snd (annotOfExp x2)) n x1 x2

   xApp' hasSubst ann@(a, _) x1 x2
    | hasSubst  = xApp a   x1 x2
    | otherwise = XApp ann x1 x2
   xLam' hasSubst ann@(a, _) n t x
    | hasSubst  = xLam a   n t x
    | otherwise = XLam ann n t x
   xLet' hasSubst ann@(a, _) n x1 x2
    | hasSubst  = xLet a   n x1 x2
    | otherwise = XLet ann n x1 x2

   subst' env captures xx
    = let go      = subst' env captures
          noSubst = return (xx, False)
      in case xx of

       XVar _ n
        -- Substitution happened.
        | Just payload <- Map.lookup n env
        -> return (payload, True)
        -- No subst.
        | otherwise
        -> noSubst

       XApp a p q
        -> do !(p', substP) <- go p
              !(q', substQ) <- go q
              let !substX    = substP || substQ
              return (xApp' substX a p' q', substX)

       XPrim{}
        -> noSubst
       XValue{}
        -> noSubst

       XLam a n t x
        -- If the name is in the name set of all payloads or it *has* a payload,
        -- might as well rename it.
        | n `Set.member` captures || n `Map.member` env
        -> do  -- Generate fresh name and add to environment
               -- We do not actually need to insert n' into the free set
               -- because it is fresh, and cannot occur in the expression.
               !n'           <- fresh
               let !env'      = Map.insert n (xVar n') env
               !(x', substX) <- subst' env' captures x
               return (xLam' substX a n' t x', substX)

        -- Name is mentioned and no clashes, so proceed
        | otherwise
        -> do !(x', substX) <- go x
              return (xLam' substX a n t x', substX)

       XLet a n x1 x2
        -- As with lambda
        | n `Set.member` captures || n `Map.member` env
        -> do  n'              <- fresh
               let !env'        = Map.insert n (xVar n') env
               !(x1', substX1) <- go x1
               !(x2', substX2) <- subst' env' captures x2
               let !substX      = substX1 || substX2
               return (xLet' substX a n' x1' x2', substX)

        -- Proceed as usual
        | otherwise
        -> do !(x1', substX1) <- go x1
              !(x2', substX2) <- go x2
              let !substX      = substX1 || substX2
              return (xLet' substX a n x1' x2', substX)


reannotX :: (a -> a') -> Exp a n p -> Exp a' n  p
reannotX f xx
 = case xx of
    XVar    a n     -> XVar   (f a) n
    XPrim   a p     -> XPrim  (f a) p
    XValue  a t v   -> XValue (f a) t v
    XApp    a x1 x2 -> XApp   (f a) (reannotX f x1) (reannotX f x2)
    XLam    a n t x -> XLam   (f a) n t (reannotX f x)
    XLet    a n b x -> XLet   (f a) n (reannotX f b) (reannotX f x)
{-# INLINE reannotX #-}

eraseAnnotX :: Exp a n p -> Exp () n p
eraseAnnotX = reannotX (const ())

