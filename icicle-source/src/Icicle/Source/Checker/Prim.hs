-- | Types of primitives.
-- It is odd that we need to look up inside a Fresh monad.
-- However, because the types are polymorphic in the variable type, we have no way of saying "forall a".
-- So we must generate a fresh name for any forall binders.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Checker.Prim (
    primLookup
  ) where

import                  Icicle.Source.Checker.Base

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Control.Monad.Trans.Class
import                  Data.Hashable (Hashable)

import                  P

primLookup
 :: (Hashable n, Eq n)
 => a -> Prim
 -> Gen a n (FunctionType n, [Type n], Type n, GenConstraintSet a n)
primLookup ann p
 = do ft <- Gen . lift . lift $ primLookup' p
      introForalls ann ft
