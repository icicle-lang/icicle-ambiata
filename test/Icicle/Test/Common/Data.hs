{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Common.Data where

import           Icicle.Test.Core.Arbitrary

import           Icicle.Common.Data

import           P

import           System.IO

import           Test.QuickCheck


prop_roundtrip_valueToCore t =
  forAll (baseValueForType t) $ \bv ->
  case valueFromCore bv of
    Nothing -> discard
    Just v  ->
      case valueToCore v t of
        Nothing  -> discard
        Just bv' -> counterexample ("value = " <> show v)
                  $ bv === bv'


return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10000, maxSize = 10})

