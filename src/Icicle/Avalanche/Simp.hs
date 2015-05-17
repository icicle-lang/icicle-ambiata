-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  , pullLets
  ) where

import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              Icicle.Avalanche.Statement.Simp
import              Icicle.Avalanche.Program

import              P

simpAvalanche :: (Show n, Show p, Ord n) => Program n p -> Fresh n (Program n p)
simpAvalanche p
 = do p' <- transformX return simp p
      s' <- forwardStmts $ pullLets $ statements p'
      return $ p { statements = s' }

