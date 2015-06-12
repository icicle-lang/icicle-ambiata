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
 | ErrorReturnNotAggregate (Query a n) UniverseType
 | ErrorContextExpNotBool  (Context a n)   UniverseType
 | ErrorContextExpNotEnum  (Context a n)   UniverseType
 | ErrorContextExpNotElem  (Context a n)   UniverseType
 | ErrorTypeMismatch   UniverseType UniverseType
 | ErrorApplicationOfNonPrim (Exp a n)
 | ErrorPrimBadArgs          (Exp a n) [UniverseType]
 deriving (Show, Eq, Ord)

