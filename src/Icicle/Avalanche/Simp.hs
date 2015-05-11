-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  ) where

import              Icicle.Common.Exp

import              Icicle.Avalanche.Program

import              P

simpAvalanche :: (Show n, Show p, Ord n) => Program n p -> Program n p
simpAvalanche = transformX id simp

