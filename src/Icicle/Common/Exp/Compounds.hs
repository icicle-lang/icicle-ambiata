-- | Some useful things to do with expressions
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Compounds (
      makeApps
    , takeApps
    , takePrimApps
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
import              P

import qualified    Data.Set    as Set


-- | Apply an expression to any number of arguments
makeApps :: Exp n p -> [Exp n p] -> Exp n p
makeApps f args
 = foldl XApp f args


-- | Split an expression into its function part and any arguments applied to it.
-- If it's not an application, arguments will be empty.
takeApps :: Exp n p -> (Exp n p, [Exp n p]) 
takeApps xx
 = case xx of
    XApp p q
     -> let (f,as) = takeApps p
        in  (f, as <> [q])
    _
     -> (xx, [])


-- | Check if an expression is a primitive application
takePrimApps :: Exp n p -> Maybe (p, [Exp n p])
takePrimApps xx
 = case takeApps xx of
    (XPrim p, args) -> Just (p, args)
    _               -> Nothing


-- | Prefix an expression with some let bindings
makeLets :: [(Name n, Exp n p)] -> Exp n p -> Exp n p
makeLets bs x
 = foldr (uncurry XLet) x bs

-- | Pull out the top-level let bindings
takeLets :: Exp n p -> ([(Name n, Exp n p)], Exp n p)
takeLets xx
 = case xx of
    XLet n x y
     -> let (bs, y') = takeLets y
        in  ((n,x) : bs, y')
    _
     -> ([], xx)



-- | Collect all free variables in an expression
-- i.e. those that are not bound by lets or lambdas.
freevars
        :: Ord n
        => Exp n p
        -> Set.Set (Name n)
freevars xx
 = case xx of
    XVar n      -> Set.singleton n
    XPrim{}     -> Set.empty
    XValue{}    -> Set.empty
    XApp p q    -> freevars p <> freevars q
    XLam n _ x  -> Set.delete n (freevars x)
    XLet n x y  -> freevars x <> Set.delete n (freevars y)


-- | Collect all variable names in an expression:
-- free and bound
allvars
        :: Ord n
        => Exp n p
        -> Set.Set (Name n)
allvars xx
 = case xx of
    XVar n      -> Set.singleton n
    XPrim{}     -> Set.empty
    XValue{}    -> Set.empty
    XApp p q    -> allvars p <> allvars q
    XLam n _ x  -> Set.singleton n <> allvars x
    XLet n x y  -> Set.singleton n <> allvars x <> allvars y


-- | Substitute an expression in, but if it would require renaming
-- just give up and return Nothing
substMaybe
        :: Ord  n
        => Name n
        -> Exp  n p
        -> Exp  n p
        -> Maybe (Exp n p)
substMaybe name payload into
 = go into
 where
  payload_free = freevars payload

  go xx
   = case xx of
      XVar n
       | n == name
       -> return payload
       | otherwise
       -> return xx
      XApp p q
       -> XApp <$> go p <*> go q

      XPrim{}
       -> return xx
      XValue{}
       -> return xx

      XLam n t x
       -- If the name clashes, we can't do anything
       | (n `Set.member` payload_free) || n == name
       , name `Set.member` freevars x
       -> Nothing

       -- If name isn't mentioned in x, we don't need to do anything
       | not (name `Set.member` freevars x)
       -> return xx

       -- Name is mentioned and no clashes, so proceed
       | otherwise
       -> XLam n t <$> go x

      XLet n x1 x2
       -- If the let's name clashes with the substitution we're trying to make
       -- and the *body* of the let needs to be substituted into,
       -- we cannot proceed.
       -- (It doesn't matter if the definition, x1, mentions name because "n" is not bound there)
       | (n `Set.member` payload_free) || n == name
       , name `Set.member` freevars x2
       -> Nothing

       -- If name is not mentioned in x1 or x2, we do not need to perform any substitution.
       |  not (name `Set.member` freevars x1)
       && not (name `Set.member` freevars x2)
       -> return xx

       -- Proceed as usual
       | otherwise
       -> XLet n <$> go x1 <*> go x2


-- | Substitute an expression in, 
-- using fresh names to avoid capture
subst
        :: Ord  n
        => Name n
        -> Exp  n p
        -> Exp  n p
        -> Fresh n (Exp n p)
subst name payload into
 = go into
 where
  payload_free = freevars payload

  go xx
   = case xx of
      XVar n
       | n == name
       -> return payload
       | otherwise
       -> return xx
      XApp p q
       -> XApp <$> go p <*> go q

      XPrim{}
       -> return xx
      XValue{}
       -> return xx

      XLam n t x
       -- If the name clashes, we need to rename n
       | (n `Set.member` payload_free) || n == name
       , name `Set.member` freevars x
       -> do    n' <- fresh
                x' <- subst n (XVar n') x
                XLam n' t <$> go x'

       -- If name isn't mentioned in x, we don't need to do anything
       | not (name `Set.member` freevars x)
       -> return xx

       -- Name is mentioned and no clashes, so proceed
       | otherwise
       -> XLam n t <$> go x

      XLet n x1 x2
       -- If the let's name clashes with the substitution we're trying to make,
       -- we need to rename
       | (n `Set.member` payload_free) || n == name
       , name `Set.member` freevars x2
       -> do    n'  <- fresh
                x2' <- subst n (XVar n') x2
                XLet n' <$> go x1 <*> go x2'

       -- If name is not mentioned in x1 or x2, we do not need to perform any substitution.
       |  not (name `Set.member` freevars x1)
       && not (name `Set.member` freevars x2)
       -> return xx

       -- Proceed as usual
       | otherwise
       -> XLet n <$> go x1 <*> go x2

