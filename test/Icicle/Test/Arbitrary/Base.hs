{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary.Base where

import           Test.QuickCheck

import           P


smaller :: Gen a -> Gen a
smaller g = sized (\s -> resize (s `div` 2) g)

