-- | Convert Core programs to Avalanche
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Icicle.Avalanche.Simp (
    simpAvalanche
  , simpFlattened
  , pullLets
  , SimpOpts(..)
  , defaultSimpOpts
  , SimpError(..)
  ) where

import           Icicle.Common.Exp
import           Icicle.Common.FixT
import           Icicle.Common.Fresh

import qualified Icicle.Core.Eval.Exp                        as Core
import qualified Icicle.Core.Exp.Prim                        as Core

import           Icicle.Avalanche.Annot
import qualified Icicle.Avalanche.Check                      as Check
import qualified Icicle.Avalanche.Prim.Eval                  as Flat
import qualified Icicle.Avalanche.Prim.Flat                  as Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Simp
import           Icicle.Avalanche.Statement.Simp.Constructor
import           Icicle.Avalanche.Statement.Simp.Eval
import           Icicle.Avalanche.Statement.Simp.Melt
import           Icicle.Avalanche.Statement.Simp.Mutate
import           Icicle.Avalanche.Statement.Statement

import           P

import           Control.Monad.Trans.Class
import           Data.Hashable                               (Hashable)
import           Data.Text                                   (Text)

import Icicle.Internal.Pretty

data SimpOpts = SimpOpts
  { simpOptsCheckCrunch :: Bool
  , simpOptsCheckSimp   :: Bool }

defaultSimpOpts :: SimpOpts
defaultSimpOpts = SimpOpts False False

optCheck
  :: (Hashable n, Eq n)
  => Bool
  -> Check.Context n
  -> Statement a n Flat.Prim
  -> Either (Check.ProgramError () n Flat.Prim) (Statement a n Flat.Prim)
optCheck doCheck ctx ss
  | doCheck
  = do _ <- Check.checkStatement Flat.flatFragment ctx $ eraseAnnotS ss
       return ss
  | otherwise
  = Right ss

--------------------------------------------------------------------------------

type TransformName = Text

data SimpError a n p
  = SimpError TransformName (Check.ProgramError a n p)
  deriving (Show)

check :: (Monad m, Pretty x)
      => (x -> Either (Check.ProgramError a n p) b)
      -> TransformName
      -> (y -> m x)
      -> Either (SimpError a n p) y
      -> m (Either (SimpError a n p) b)
check chk transformName transform val
  = case val of
      Left e -> return $ Left e
      Right x -> do
        x'    <- transform x
        let y  = chk x'
        case y of
          Left  e' -> trace (show $ pretty x') $ return $ Left $ SimpError transformName e'
          Right y' -> return $ Right y'

pass :: (Monad m)
     => (t -> m b)
     -> (Either a t -> m (Either a b))
pass transform val
  = case val of
      Left e
        -> return $ Left e
      Right x
        -> do x' <- transform x
              return $ Right x'

ret :: (Monad m)
    => Program a n p
    -> Either e (Statement a n p)
    -> m (Either e (Program a n p))
ret p ss = return $ case ss of
  Left  e -> Left e
  Right s -> Right $ p { statements = s }

--------------------------------------------------------------------------------

simpAvalanche
  :: (Show n, Hashable n, Eq n, Ord a)
  => a
  -> Program a n Core.Prim
  -> Fresh n (Program a n Core.Prim)
simpAvalanche a_fresh p
 = do p' <- transformX return (simpAnn a_fresh) p
      s' <- (once $ forwardStmts a_fresh $ pullLets $ statements p')
         >>= once . thresherWithAlpha     a_fresh
         >>= once . forwardStmts a_fresh
         >>= nestBlocks   a_fresh
         >>= once . thresherWithAlpha     a_fresh
         >>= transformX return (return . simpEvalX Core.evalPrim Core.typeOfPrim)
         >>= return .  dead
      s'' <- transformX return (return . reannotX fst) s'
      return $ p { statements = s'' }


simpFlattened
  :: forall a n . (Show n, Hashable n, Pretty n, Eq n, Eq a, Ord a)
  => a
  -> SimpOpts
  -> Program a n Flat.Prim
  -> Fresh n (Either (SimpError () n Flat.Prim) (Program a n Flat.Prim))
simpFlattened a_fresh opts p
 = do s' <-  return (Right (statements p))
         >>= check1 "simp_exp"       (transformX return (simp a_fresh))
         >>= check1 "mutate"         (return . mutate)
         >>= check1 "melt"           (melt a_fresh)
         >>= pass                    (transformX return (simpAnn a_fresh))
         >>= traverse (fixpointEither crunch)
         -- Rename reads from accumulators
         >>= check1 "rename_reads"   (fixpoint (renameReads a_fresh)) . join
         -- Convert values to primitive constructors
         >>= check1 "convert_values" (return . convertValues a_fresh)
         -- Finish off with an a-normalisation
         >>= pass                     anormal
         >>= pass                    (transformX return (return . reannotX fst))
      ret p s'
 where
  crunch :: Statement (Ann a n) n Flat.Prim
         -> FixT (Fresh n) (Either (SimpError () n Flat.Prim) (Statement (Ann a n) n Flat.Prim))
  crunch ss
   -- Start by a-normalising, so it's ready for constructor
   =   lift (anormal ss)
   -- Rewrite rules like (fst (a,b) => a
   >>= check2 "crunch_constructor"    (constructor  a_fresh) . Right

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
   >>= check2 "crunch_dead"           (return .  dead)
   -- Kill off statements that have no observable effect (no write to accumulators, etc.)
   >>= check2 "crunch_kill_no_effect" (return . killNoEffect)
   -- Perform let-forwarding on statements, so that constant lets become free
   >>= check2 "crunch_forward_stmts" (forwardStmts a_fresh)
   -- Try to evaluate any exposed primitives
   >>= check2 "crunch_constant_fold"  constantFold
   -- Pull Let statements out of blocks. This just allows thresher to remove more duplicates
   >>= check2 "crunch_nest_blocks"   (lift . nestBlocks a_fresh)
   -- Thresh to remove duplicate expressions
   >>= check2 "crunch_thresher"      (thresherNoAlpha a_fresh)

  anormal :: Statement (Ann a n) n Flat.Prim
          -> Fresh n (Statement (Ann a n) n Flat.Prim)
  anormal ss
   -- Expression simp: first perform beta reduction, then a-normalise.
   =   transformX return (simpKeepAnn a_fresh) ss
   -- finish a-normalisation by taking lets from inside expressions to statements.
   >>= return . pullLets

  constantFold
   = transformX return (return . simpEvalX Flat.evalPrim Flat.typeOfPrim)

  ctx    = Check.initialContext p
  check1 = check (optCheck (simpOptsCheckSimp   opts) ctx)
  check2 = check (optCheck (simpOptsCheckCrunch opts) ctx)
