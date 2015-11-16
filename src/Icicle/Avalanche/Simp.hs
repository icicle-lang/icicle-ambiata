-- | Convert Core programs to Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DoAndIfThenElse   #-}
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
  :: (Show n, Ord n, Eq a)
  => a
  -> Program a n Prim
  -> Fresh n (Program a n Prim)
simpFlattened a_fresh p
 = do p' <- transformX return (simp a_fresh) p
      let Program i bd s = p'

      s' <-  melt a_fresh s
         >>= crunchy i bd

      return $ p { statements = s' }
 where
  crunchy i bd s
   = do s' <- crunch i bd s
        if s == s'
        then return s
        else crunchy i bd s'

  crunch i bd ss
   =   constructor  a_fresh (pullLets ss)
   >>= thresher     a_fresh
   >>= forwardStmts a_fresh
   >>= nestBlocks   a_fresh
   >>= thresher     a_fresh
   >>= fmap statements . transformX return (simp a_fresh) . Program i bd

