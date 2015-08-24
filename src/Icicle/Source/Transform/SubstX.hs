{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Transform.SubstX (
    unsafeSubstTransform
  ) where

import Icicle.Source.Query
import Icicle.Source.Transform.Base

import Icicle.Common.Base

import P

import Data.Functor.Identity

import qualified Data.Map as Map


-- | This is unsafe in the sense that it does not avoid capture of the payload.
-- It is fine if the only variables in the payload are fresh:
-- eg 'subst x := (5 + fresh) in x + x'
-- because we know there can be no mention of "fresh" in the target.

unsafeSubstTransform
    :: Ord n
    => Transform Identity (Map.Map (Name n) (Exp a n)) a n

unsafeSubstTransform
 = Transform
 { transformExp     = tranx
 , transformPat     = tranp
 , transformContext = tranc
 , transformState   = Map.empty
 }
 where
  tranx s x
   = case x of
      Var _ n
       | Just x' <- Map.lookup n s
       -> return (s, x')
      _
       -> return (s, x)

  tranp s p
   = return (rempatbinds s p, p)

  rempatbinds s p
   = case p of
      PatCon _ ps
       -> foldl rempatbinds s ps
      PatDefault
       -> s
      PatVariable n
       -> Map.delete n s

  tranc s c
   = case c of
      -- To make this safe, we would need to
      -- look through the free vars of the map's values (the payloads)
      -- and rename this binding if any payloads
      -- refer to a variable with the same name
      Let _ n _
       -> return (Map.delete n s, c)
      LetFold _ f
       -> return (Map.delete (foldBind f) s, c)
      _
       -> return (s, c)

