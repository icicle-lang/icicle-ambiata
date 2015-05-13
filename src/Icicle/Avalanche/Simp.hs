-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  ) where

import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              Icicle.Avalanche.Program

import              P

simpAvalanche :: (Show n, Show p, Ord n) => NameState n -> Program n p -> Program n p
simpAvalanche ns p
 = transformX id (\x -> snd $ runFresh (simp x) ns) p

