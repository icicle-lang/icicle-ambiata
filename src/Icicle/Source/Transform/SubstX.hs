{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.SubstX (
    unsafeSubstX
  , unsafeSubstQ
  ) where

import Icicle.Source.Query

import Icicle.Common.Base

import P


import qualified Data.Map as Map


-- | This is unsafe in the sense that it does not avoid capture of the payload.
-- It is fine if the only variables in the payload are fresh:
-- eg 'subst x := (5 + fresh) in x + x'
-- because we know there can be no mention of "fresh" in the target.

unsafeSubstX
    :: Ord n
    => (Map.Map (Name n) (Exp a n))
    -> Exp a n
    -> Exp a n

unsafeSubstX s x
 = case x of
    Var _ n
     | Just x' <- Map.lookup n s
     -> x'
     | otherwise
     -> x
    Nested a q
     -> Nested a $ unsafeSubstQ s q
    App a p q
     -> App a (unsafeSubstX s p) (unsafeSubstX s q)
    Prim{}
     -> x
    Case a scrut pats
     -> Case a (unsafeSubstX s scrut) (fmap goPat pats)

 where

  goPat (p,alt)
   = (p, unsafeSubstX (rempatbinds s p) alt)

  rempatbinds m p
   = case p of
      PatCon _ ps
       -> foldl rempatbinds m ps
      PatDefault
       -> m
      PatVariable n
       -> Map.delete n m


unsafeSubstQ
    :: Ord n
    => (Map.Map (Name n) (Exp a n))
    -> Query a n
    -> Query a n

unsafeSubstQ s (Query [] x)
 = Query [] (unsafeSubstX s x)

unsafeSubstQ s (Query (c:rest_cs) rest_x)
 = case c of
    -- To make this safe, we would need to
    -- look through the free vars of the map's values (the payloads)
    -- and rename this binding if any payloads
    -- refer to a variable with the same name
    Let a n x
     -> ins (Let a n (unsafeSubstX s x)) (unsafeSubstQ (Map.delete n s) q)
    LetFold a f
     -> let s' = Map.delete (foldBind f) s
            z' = unsafeSubstX s  (foldInit f)
            k' = unsafeSubstX s' (foldWork f)
            f' = f { foldInit = z', foldWork = k' }
        in  ins (LetFold a f') (unsafeSubstQ s' q)
    Windowed{}
     -> ins c q'
    Latest{}
     -> ins c q'
    GroupBy a x
     -> ins (GroupBy a (unsafeSubstX s x)) q'
    GroupFold a k v x
     -> ins (GroupFold a k v (unsafeSubstX s x)) (unsafeSubstQ (Map.delete k $ Map.delete v s) q)
    Distinct a x
     -> ins (Distinct a (unsafeSubstX s x)) q'
    Filter a x
     -> ins (Filter a   (unsafeSubstX s x)) q'
 where
  q = Query rest_cs rest_x
  q' = unsafeSubstQ s q

  ins cx' (Query cs' x')
   = Query (cx' : cs') x'

