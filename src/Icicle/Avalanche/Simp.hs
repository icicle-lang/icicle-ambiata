-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  ) where

import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              Icicle.Avalanche.Program

import              P

simpAvalanche :: (Show n, Show p, Ord n) => Program n p -> Fresh n (Program n p)
simpAvalanche p
 = transformX return simp p

