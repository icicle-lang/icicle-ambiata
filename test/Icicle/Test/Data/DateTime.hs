{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Test.Data.DateTime where

import           Icicle.Data.DateTime
import           Icicle.Test.Arbitrary ()

import           P

import           System.IO

import           Test.QuickCheck

prop_date_symmetry :: DateTime -> Property
prop_date_symmetry d =
  d === unpackWord64 (packWord64 d)

return []
tests :: IO Bool
tests = $quickCheckAll
