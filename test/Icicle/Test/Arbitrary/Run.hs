{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell     #-}
module Icicle.Test.Arbitrary.Run (
    ExpectedTestSpeed(..)
  , checkArgs, checkArgsSized
  , checkAll, checkAllWith
  ) where

import           Test.QuickCheck
import           Disorder.Core.Run as Run
import           Language.Haskell.TH

import           P

-- | Default quickcheck arguments for us
checkArgs :: Args
checkArgs = checkArgsSized 100

-- | For some tests, we need a large discard ratio.
-- These are particularly front-end ones that generate totally random programs
-- and filter them to only those that typecheck.
-- While this is only needed for some tests, it doesn't hurt the others.
checkArgsSized :: Int -> Args
checkArgsSized sz = stdArgs { maxSize = sz, maxDiscardRatio = 10000 }

checkAll :: Q Exp
checkAll = [| $checkAllWith TestRunNormal checkArgs |]

checkAllWith :: Q Exp
checkAllWith = [| \speed args -> $forAllProperties (disorderCheckEnvWith speed args) |]
