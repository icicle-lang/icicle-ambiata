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
import              Icicle.Common.FixT

import qualified    Icicle.Core.Exp.Prim as CorePrim
import qualified    Icicle.Core.Eval.Exp as CorePrim

import qualified    Icicle.Avalanche.Prim.Eval as Flat
import qualified    Icicle.Avalanche.Prim.Flat as Flat
import              Icicle.Avalanche.Statement.Simp
import              Icicle.Avalanche.Statement.Simp.Eval
import              Icicle.Avalanche.Statement.Simp.Constructor
import              Icicle.Avalanche.Statement.Simp.Melt
import              Icicle.Avalanche.Program

import              P

import              Control.Monad.Trans.Class


simpAvalanche
  :: (Show n, Ord n)
  => a
  -> Program a n CorePrim.Prim
  -> Fresh n (Program a n CorePrim.Prim)
simpAvalanche a_fresh p
 = do p' <- transformX return (simp a_fresh) p
      s' <- (once $ forwardStmts a_fresh $ pullLets $ statements p')
         >>= once . thresher     a_fresh
         >>= once . forwardStmts a_fresh
         >>= nestBlocks   a_fresh
         >>= once . thresher     a_fresh
         >>= transformX return (return . simpEvalX CorePrim.evalPrim CorePrim.typeOfPrim)

      return $ p { statements = s' }

simpFlattened
  :: (Show n, Ord n, Eq a)
  => a
  -> Program a n Flat.Prim
  -> Fresh n (Program a n Flat.Prim)
simpFlattened a_fresh p
 = do s' <- transformX return (simp a_fresh) (statements p)
         >>= melt a_fresh
         >>= fixpoint crunch
         -- Rename reads from accumulators
         >>= fixpoint (renameReads a_fresh)
         -- Finish off with an a-normalisation
         >>= anormal

      return $ p { statements = s' }
 where
  crunch ss
   -- Start by a-normalising, so it's ready for constructor
   =   lift (anormal ss)
   -- Remove some dead code
   >>= return .  dead
   -- Rewrite rules like (fst (a,b) => a
   >>= constructor  a_fresh
   -- Perform let-forwarding on statements, so that constant lets become free
   >>= forwardStmts a_fresh
   -- Try to evaluate any exposed primitives
   >>= transformX return (return . simpEvalX Flat.evalPrim Flat.typeOfPrim)
   -- Pull Let statements out of blocks. This just allows thresher to remove more duplicates
   >>= lift . nestBlocks   a_fresh
   -- Thresh again. Surprisingly, having both threshers makes simpFlattened twice as fast!
   >>= thresher     a_fresh

  anormal ss
   -- Expression simp: first perform beta reduction, then a-normalise.
   =   transformX return (simp a_fresh) ss
   -- finish a-normalisation by taking lets from inside expressions to statements.
   >>= return . pullLets

