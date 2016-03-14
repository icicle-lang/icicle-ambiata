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
import              Data.Hashable (Hashable)


simpAvalanche
  :: (Show n, Hashable n, Eq n)
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
  :: (Show n, Eq a, Hashable n, Eq n)
  => a
  -> Program a n Flat.Prim
  -> Fresh n (Program a n Flat.Prim)
simpFlattened a_fresh p
 = do s' <- transformX return (simp a_fresh) (statements p)
         >>= melt a_fresh
         >>= fixpoint crunch
         -- Rename reads from accumulators
         >>= fixpoint (renameReads a_fresh)
         -- Convert values to primitive constructors
         >>= return . convertValues a_fresh
         -- Finish off with an a-normalisation
         >>= anormal

      return $ p { statements = s' }
 where
  crunch ss
   -- Start by a-normalising, so it's ready for constructor
   =   lift (anormal ss)
   -- Rewrite rules like (fst (a,b) => a
   >>= constructor  a_fresh

   -- Remove some dead code.
   -- Doing this straight after constructor seems to be ideal, because
   -- constructor performs some local inlining.
   -- For example,
   -- > let x = (u, v)
   -- > let y = fst x
   -- becomes
   -- > let x = (u, v)
   -- > let y = u
   -- and now "x" is no longer mentioned, so can be removed.
   -- Doing this straight away means a smaller program for later passes.
   >>= return .  dead
   -- Perform let-forwarding on statements, so that constant lets become free
   >>= forwardStmts a_fresh
   -- Try to evaluate any exposed primitives
   >>= transformX return (return . simpEvalX Flat.evalPrim Flat.typeOfPrim)
   -- Pull Let statements out of blocks. This just allows thresher to remove more duplicates
   >>= lift . nestBlocks   a_fresh
   -- Thresh to remove duplicate expressions
   >>= thresher     a_fresh

  anormal ss
   -- Expression simp: first perform beta reduction, then a-normalise.
   =   transformX return (simp a_fresh) ss
   -- finish a-normalisation by taking lets from inside expressions to statements.
   >>= return . pullLets

