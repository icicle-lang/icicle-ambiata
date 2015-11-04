-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  , simpFlattened
  , pullLets
  ) where

import              Icicle.Common.Exp
import              Icicle.Common.Fresh
import              Icicle.Common.Annot

import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Statement.Simp
import              Icicle.Avalanche.Statement.Simp.Constructor
import              Icicle.Avalanche.Statement.Simp.Melt
import              Icicle.Avalanche.Program

import              P


simpAvalanche
  :: (Show n, Show p, Ord n, Eq p)
  => a
  -> Program a n p
  -> Fresh n (Program a n p)
simpAvalanche a_fresh p
 = do p' <- transformX return (simp a_fresh) p
      s' <- (forwardStmts a_fresh $ pullLets $ statements p')
         >>= thresher     a_fresh
         >>= forwardStmts a_fresh
         >>= nestBlocks   a_fresh
         >>= thresher     a_fresh

      return $ p { statements = s' }

simpFlattened
  :: (Show n, Ord n)
  => Annot a
  -> Program (Annot a) n Prim
  -> Fresh n (Program (Annot a) n Prim)
simpFlattened a_fresh p
 = do p' <- transformX return (simp a_fresh) p
      let Program bd s = p'

      s' <-  melt a_fresh s
         >>= forwardStmts a_fresh . pullLets
         >>= crunch bd
         >>= crunch bd
         >>= crunch bd
         >>= crunch bd
         >>= crunch bd
         >>= crunch bd
         >>= crunch bd
         >>= crunch bd
         >>= crunch bd
         >>= return . simpStatementExps a_fresh

      return $ p { statements = s' }

 where
  crunch bd ss
   =   constructor  a_fresh  (pullLets ss)
   >>= forwardStmts a_fresh . pullLets
   >>= nestBlocks   a_fresh
   >>= thresher     a_fresh
   >>= fmap statements . transformX return (simp a_fresh) . Program bd


