-- | Simplifier for simple expressions
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Simp (
      simp
    ) where

import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Simp.Beta

import P


-- | Just perform beta reduction for now
simp :: (Show n, Show p, Ord n) => Exp n p -> Exp n p
simp = beta isSimpleValue
