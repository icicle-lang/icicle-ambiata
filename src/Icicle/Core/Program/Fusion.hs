-- | Fusing programs together
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Program.Fusion (
    fusePrograms
  , fuseMultiple
  ) where

import Icicle.Common.Base
import Icicle.Common.Type
import Icicle.Core.Program.Program
import Icicle.Core.Program.Check
import Icicle.Core.Program.Error
import Icicle.Core.Exp.Combinators
import Icicle.Core.Exp.Prim
import qualified Icicle.Common.Exp.Exp as X
import qualified Icicle.Common.Exp.Prim.Minimal as Min

import              P

data FusionError n
 = FusionErrorNotSameType ValType ValType
 | FusionErrorTypeError (Maybe (ProgramError n)) (Maybe (ProgramError n))
 | FusionErrorNothingToFuse
 deriving (Show)


-- | Fuse programs together, prefixing each with its name to ensure that the
-- generated program has no name clashes.
-- The two names must be distinct.
fusePrograms :: Ord n => n -> Program n -> n -> Program n -> Either (FusionError n) (Program n)
fusePrograms ln lp rn rp
 = fuseProgramsDistinctNames (prefix ln lp) (prefix rn rp)
 where
  prefix n = renameProgram (NameMod n)


-- | Fuse programs together, assuming they already have no name clashes.
fuseProgramsDistinctNames :: Ord n => Program n -> Program n -> Either (FusionError n) (Program n)
fuseProgramsDistinctNames lp rp
 | input lp /= input rp
 = Left
 $ FusionErrorNotSameType (input lp) (input rp)
 | otherwise
 = case (checkProgram lp, checkProgram rp) of
    (Right (FunT [] lt), Right (FunT [] rt))
     -> return
      $ Program
        { input     = input lp
        , precomps  = precomps  lp <> precomps  rp
        , streams   = streams   lp <> streams   rp
        , reduces   = reduces   lp <> reduces   rp
        , postdate  = postdate'
        , postcomps = postdate'bind <> postcomps lp <> postcomps rp
        , returns   = X.XPrim (PrimMinimal $ Min.PrimConst (Min.PrimConstPair lt rt)) @~ returns lp @~ returns rp
        }
    (le, re)
     -> Left
      $ FusionErrorTypeError (leftToMaybe le) (leftToMaybe re)

 where
  -- Get new date binding for use in postcomputation.
  (postdate', postdate'bind)
  -- If both use the date, we need to bind both values
   | Just ld <- postdate lp
   , Just rd <- postdate rp
   = (Just ld, [(rd, X.XVar ld)])

   -- Only one uses the date
   | Just ld <- postdate lp
   = (Just ld, [])

   | Just rd <- postdate rp
   = (Just rd, [])

   -- Neither use the date
   | otherwise
   = (Nothing, [])


-- | Fuse a list of programs together, prefixing each with its name
fuseMultiple :: Ord n => [(n, Program n)] -> Either (FusionError n) (Program n)
fuseMultiple ps
 = let ps' = fmap (\(n,p) -> renameProgram (NameMod n) p) ps
   in  case ps' of
        [] -> Left FusionErrorNothingToFuse
        (f:fs) -> foldM fuseProgramsDistinctNames f fs
