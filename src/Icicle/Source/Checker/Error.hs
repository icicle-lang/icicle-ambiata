{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Error (
    CheckError(..)
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.Type

-- import                  Icicle.Internal.Pretty

import                  P

data CheckError a n
 = ErrorNoSuchVariable a n
 | ErrorNoSuchFeature n
 | ErrorReturnNotAggregate a (Query a n) UniverseType
 | ErrorContextExpNotBool  a (Context a n)   UniverseType
 | ErrorContextExpNotEnum  a (Context a n)   UniverseType
 | ErrorContextExpNotElem  a (Context a n)   UniverseType
 | ErrorTypeMismatch       a UniverseType UniverseType
 | ErrorApplicationOfNonPrim a (Exp a n)
 | ErrorPrimBadArgs          a (Exp a n) [UniverseType]
 deriving (Show, Eq, Ord)

