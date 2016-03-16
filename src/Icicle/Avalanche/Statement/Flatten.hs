-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Flatten (
    flatten
  , FlattenError(..)
  ) where

import Icicle.Avalanche.Statement.Flatten.Base
import Icicle.Avalanche.Statement.Flatten.Statement

