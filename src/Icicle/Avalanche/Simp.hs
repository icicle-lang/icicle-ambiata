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
import              Icicle.Common.Annot

import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Statement.Simp
import              Icicle.Avalanche.Statement.Simp.Constructor
import              Icicle.Avalanche.Statement.Simp.Melt
import              Icicle.Avalanche.Program

import              P


simpAvalanche
  :: (Show n, Show p, Ord n, Eq p)
  => Program a n p
  -> Fresh n (Program a n p)
simpAvalanche p
 = do p' <- transformX return simp p
      s' <- forwardStmts (pullLets $ statements p')
         >>= thresher
         >>= forwardStmts
         >>= nestBlocks
         >>= thresher

      return $ p { statements = s' }

simpFlattened
  :: (Show n, Ord n, Eq a)
  => Program (Annot a) n Prim
  -> Fresh n (Program (Annot a) n Prim)
simpFlattened p
 = do p' <- transformX return simp p
      let Program i bd s = p'

      s' <-  melt s
         >>= crunchy i bd

      return $ p { statements = s' }
 where
  crunchy i bd s
   = do s' <- crunch i bd s
        if s == s'
        then return s
        else crunchy i bd s'

  crunch i bd ss
   =   constructor  (pullLets ss)
   >>= thresher
   >>= forwardStmts
   >>= nestBlocks
   >>= thresher
   >>= fmap statements . transformX return simp . Program i bd

