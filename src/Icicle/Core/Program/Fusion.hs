-- | Fusing programs together
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Program.Fusion (
    FusionError (..)
  , fusePrograms
  , fuseMultiple
  ) where

import Icicle.Common.Base
import Icicle.Common.Type
import Icicle.Core.Stream
import Icicle.Core.Program.Program
import qualified Icicle.Common.Exp.Exp as X

import              P

data FusionError n
 = FusionErrorNotSameType ValType ValType
 | FusionErrorNothingToFuse
 deriving (Show)


-- | Fuse programs together, prefixing each with its name to ensure that the
-- generated program has no name clashes.
-- The two names must be distinct.
fusePrograms :: Ord n => a -> n -> Program a n -> n -> Program a n -> Either (FusionError n) (Program a n)
fusePrograms a_fresh ln lp rn rp
 = fuseProgramsDistinctNames a_fresh (prefix ln lp) (prefix rn rp)
 where
  prefix n = renameProgram (NameMod n)


-- | Fuse programs together, assuming they already have no name clashes.
fuseProgramsDistinctNames :: Ord n => a -> Program a n -> Program a n -> Either (FusionError n) (Program a n)
fuseProgramsDistinctNames a_fresh lp rp
 | inputType lp /= inputType rp
 = Left
 $ FusionErrorNotSameType (inputType lp) (inputType rp)
 | otherwise
 = return
 $ Program
 { inputName = inputName lp
 , inputType = inputType lp
 , snaptimeName = snaptimeName lp
 , precomps  = precomps  lp <> precomps  rp
 , streams   = inpbinds <> streams   lp <> streams   rp
 , postcomps = postcomps lp <> postcomps rp
 , returns   = returns   lp <> returns   rp
 }
 where
  var n    = X.XVar a_fresh n
  inpType' = PairT (inputType lp) TimeT
  val      = X.XValue a_fresh inpType' $ defaultOfType inpType'
  inpbinds
   = [ SFold (inputName    rp) inpType'  val                     (var $ inputName    lp)
     , SFold (snaptimeName rp) TimeT    (var $snaptimeName   lp) (var $ snaptimeName lp) ]


-- | Fuse a list of programs together, prefixing each with its name
fuseMultiple :: Ord n => a -> [(n, Program a n)] -> Either (FusionError n) (Program a n)
fuseMultiple a_fresh ps
 = let ps' = fmap (\(n,p) -> renameProgram (NameMod n) p) ps
   in  case ps' of
        [] -> Left FusionErrorNothingToFuse
        (f:fs) -> foldM (fuseProgramsDistinctNames a_fresh) f fs
