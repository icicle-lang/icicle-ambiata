-- | Simplifier for simple expressions
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Simp (
      simp, simpKeepAnn, simpAnn
    ) where

import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds
import Icicle.Common.Exp.Simp.Beta
import Icicle.Common.Exp.Simp.ANormal
import Icicle.Common.Fresh

import P

import Data.Hashable

-- | Just perform beta reduction for now
simp :: (Hashable n, Eq n)
     => a -> Exp a n p -> Fresh n (Exp a n p)
simp a_fresh xx
 = anormal a_fresh
 $ beta isSimpleValue
   xx

simpKeepAnn
  :: (Hashable n, Eq n)
  => a
  -> Exp (Ann a n) n p
  -> Fresh n (Exp (Ann a n) n p)
simpKeepAnn a_fresh xx
  = anormalAllVars a_fresh
  $ beta isSimpleValue xx

simpAnn
  :: (Hashable n, Eq n)
  => a
  -> Exp a n p
  -> Fresh n (Exp (Ann a n) n p)
simpAnn a_fresh xx
  = anormalAllVars a_fresh
  $ beta isSimpleValue
  $ allvarsExp xx
