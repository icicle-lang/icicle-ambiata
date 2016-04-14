-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE DeriveGeneric     #-}
module Icicle.Avalanche.Statement.Flatten.Base (
    FlattenError(..)
  , FlatM
  ) where

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat     as Flat

import qualified    Icicle.Core.Exp.Prim           as Core

import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              P

import              GHC.Generics


data FlattenError a n
 = FlattenErrorApplicationNonPrimitive (Exp a n Core.Prim)
 | FlattenErrorBareLambda (Exp a n Core.Prim)
 | FlattenErrorPrimBadArgs Core.Prim [Exp a n Core.Prim]
 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n) => NFData (FlattenError a n)

type FlatM a n
 = FreshT n (Either (FlattenError a n)) (Statement a n Flat.Prim)


