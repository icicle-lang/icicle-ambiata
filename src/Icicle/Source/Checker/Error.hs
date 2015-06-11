{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Error (
    CheckError(..)
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.Type

-- import                  Icicle.Internal.Pretty

import                  P

data CheckError n
 = ErrorNoSuchVariable n
 | ErrorNoSuchFeature n
 | ErrorReturnNotAggregate (Query n) UniverseType
 | ErrorContextExpNotBool  (Context n)   UniverseType
 | ErrorContextExpNotEnum  (Context n)   UniverseType
 | ErrorContextExpNotElem  (Context n)   UniverseType
 | ErrorTypeMismatch   UniverseType UniverseType
 | ErrorApplicationOfNonPrim (Exp n)
 | ErrorPrimBadArgs          (Exp n) [UniverseType]
 deriving (Show, Eq, Ord)

