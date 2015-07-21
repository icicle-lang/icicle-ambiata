-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  , simpFlattened
  , pullLets
  ) where

import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Statement.Simp
import              Icicle.Avalanche.Statement.Simp.Constructor
import              Icicle.Avalanche.Statement.Simp.Melt
import              Icicle.Avalanche.Program

import              P

simpAvalanche :: (Show n, Show p, Ord n, Eq p) => Program n p -> Fresh n (Program n p)
simpAvalanche p
 = do p' <- transformX return simp p
      s' <- (forwardStmts $ pullLets $ statements p')
         >>= thresher
         >>= forwardStmts
         >>= nestBlocks
         >>= thresher

      return $ p { statements = s' }

simpFlattened :: (Show n, Ord n) => Program n Prim -> Fresh n (Program n Prim)
simpFlattened p
 = do p' <- transformX return simp p
      s' <- (forwardStmts $ pullLets $ statements p')
         >>= melt
         >>= crunch
         >>= crunch
         >>= crunch
         >>= crunch
         >>= crunch

      return $ p { statements = s' }
 where
  crunch ss
   =   constructor (pullLets ss)
   >>= forwardStmts
   >>= nestBlocks
   >>= thresher


