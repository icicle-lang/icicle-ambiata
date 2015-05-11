-- | Fusing two programs together
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Program.Fusion (
    fusePrograms
  ) where

import Icicle.Common.Base
import Icicle.Common.Type
import Icicle.Core.Program.Program
import Icicle.Core.Program.Check
import Icicle.Core.Program.Error
import Icicle.Core.Exp.Combinators
import Icicle.Core.Exp.Prim
import qualified Icicle.Common.Exp.Exp as X

-- import              Data.Either.Combinators
import              P

data FusionError n
 = FusionErrorNotSameType ValType ValType
 | FusionErrorTypeError (Maybe (ProgramError n)) (Maybe (ProgramError n))

fusePrograms :: Ord n => n -> Program n -> n -> Program n -> Either (FusionError n) (Program n)
fusePrograms ln lp rn rp
 | input lp /= input rp
 = Left
 $ FusionErrorNotSameType (input lp) (input rp)
 | otherwise
 = case (checkProgram lp, checkProgram rp) of
    (Right (FunT [] lt), Right (FunT [] rt))
     -> return
      $ Program
        { input     = input lp'
        , precomps  = precomps  lp' <> precomps  rp'
        , streams   = streams   lp' <> streams   rp'
        , reduces   = reduces   lp' <> reduces   rp'
        , postdate  = postdate'
        , postcomps = postdate'bind <> postcomps lp' <> postcomps rp'
        , returns   = X.XPrim (PrimConst (PrimConstPair lt rt)) @~ returns lp' @~ returns rp'
        }
    (le, re)
     -> Left
      $ FusionErrorTypeError (leftToMaybe le) (leftToMaybe re)

 where
  prefix pre 
   = renameProgram (NameMod pre)
  lp' = prefix ln lp
  rp' = prefix rn rp

  -- Get new date binding for use in postcomputation.
  (postdate', postdate'bind)
  -- If both use the date, we need to bind both values
   | Just ld <- postdate lp'
   , Just rd <- postdate rp'
   = (Just ld, [(rd, X.XVar ld)])

   -- Only one uses the date
   | Just ld <- postdate lp'
   = (Just ld, [])

   | Just rd <- postdate rp'
   = (Just rd, [])

   -- Neither use the date
   | otherwise
   = (Nothing, [])

