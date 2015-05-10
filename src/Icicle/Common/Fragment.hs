-- | Description of a sublanguage:
--  the types of primitives
--  and which invariants to uphold
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Fragment (
      Fragment (..)
    ) where

import              Icicle.Common.Type

import              P

data Fragment p
 = Fragment
 { typeOfPrim           :: p -> Type
 , primsFullyApplied    :: Bool
 }

