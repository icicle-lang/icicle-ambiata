-- | Fusing two programs together
{-# LANGUAGE NoImplicitPrelude #-}
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
        , postcomps = postcomps lp' <> postcomps rp'
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

