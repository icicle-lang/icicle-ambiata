-- | Simplifier for simple expressions
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Simp (
      simp
    ) where

import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Simp.Beta
import Icicle.Common.Exp.Simp.ANormal
import Icicle.Common.Fresh

import P


-- | Just perform beta reduction for now
simp :: (Show n, Show p, Ord n) => a -> Exp a n p -> Fresh n (Exp a n p)
simp a_fresh xx
 = anormal a_fresh
 $ beta isSimpleValue
   xx
