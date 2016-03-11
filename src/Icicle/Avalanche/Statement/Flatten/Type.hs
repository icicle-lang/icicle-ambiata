-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Flatten.Type (
    flatT
  ) where

import              Icicle.Common.Type

-- import              P


flatT   :: Type
        -> Type
flatT t = t

