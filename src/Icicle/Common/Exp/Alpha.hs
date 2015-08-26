-- | Syntactic equality modulo renaming
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Alpha (
      alphaEquality
    ) where

import              Icicle.Common.Base
import              Icicle.Common.Exp.Exp
import              P

import qualified    Data.Map                as Map
import qualified    Data.List               as List


-- | Check whether two expressions are syntactically equal,
-- but allow locally bound names to differ.
alphaEquality
        :: (Ord n, Eq p)
        => Exp a n p -> Exp a n p
        -> Bool
alphaEquality = alphaEquality' Map.empty


-- | Check alpha equality with bijection of names bound in left expression to
-- their equivalent in right
alphaEquality'
        :: (Ord n, Eq p)
        => Map.Map (Name n) (Name n)
        -> Exp a n p -> Exp a n p
        -> Bool
alphaEquality' m x1 x2
 -- Variables must match in the bijection, or not in the map at all and equal
 | XVar _ n1          <- x1
 , XVar _ n2          <- x2
 = lookupBoth m (n1, n2)

 -- Simple primitive
 | XPrim _ p1         <- x1
 , XPrim _ p2         <- x2
 = p1 == p2

 -- Base values
 | XValue _ t1 v1     <- x1
 , XValue _ t2 v2     <- x2
 =  t1 == t2
 && v1 == v2

 -- Recurse with same map
 | XApp _ x11 x12     <- x1
 , XApp _ x21 x22     <- x2
 = go x11 x21 && go x12 x22

 -- Types must match. Names can be different, so add them to bijection
 | XLam _ n1 t1 x1'   <- x1
 , XLam _ n2 t2 x2'   <- x2
 =  t1 == t2 
 && alphaEquality' (insertBoth m (n1,n2))
       x1' x2'

 -- Name isn't available under the binding expression,
 -- so only add it to the bijection for the second
 | XLet _ n1 x11 x12  <- x1
 , XLet _ n2 x21 x22  <- x2
 =  go x11 x21
 && alphaEquality' (insertBoth m (n1,n2))
       x12 x22

 -- If none of the above cases match, the expressions must be different constructors.
 | otherwise
 = False

 where
  go = alphaEquality' m


-- | Insert l=r into bijection map.
-- If there is already a binding that mentions l, it will be overwritten by the Map.insert.
-- If there is already a binding that mentions r, we must find its matching left and remove it.
insertBoth :: Ord n => Map.Map n n -> (n,n) -> Map.Map n n
insertBoth m (l,r)
 = case reverseLookup r m of
    Nothing -> Map.insert l r m
    Just l' -> Map.insert l r
             $ Map.delete l'  m

-- | Check if two names are equal.
-- If they both occur in the bijection map, check that they both match.
-- If neither occur in the map they are free variables, and must be equal.
-- Otherwise one is bound and the other isn't, which means they cannot be equal.
lookupBoth :: Ord n => Map.Map n n -> (n,n) -> Bool
lookupBoth m (l,r)
 = case (Map.lookup l m, reverseLookup r m) of
    (Just r', Just l')
     -> l == l' && r == r'
    (Nothing, Nothing)
     -> l == r
    _
     -> False

-- | Look up in map according to value. Slow
reverseLookup :: (Ord k, Eq v) => v -> Map.Map k v -> Maybe k
reverseLookup n m
 = List.lookup n
 $ fmap swap
 $ Map.toList m

