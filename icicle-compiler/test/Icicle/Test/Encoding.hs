{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Encoding where

import           Icicle.Test.Arbitrary
import           Icicle.Encoding

import           P

import qualified Data.Set as Set

import           System.IO

import           Test.QuickCheck


-- Sanity checking the randomly generated values
-- =====================

prop_value_type e =
 forAll (valueOfEncoding e)
 $ \v -> valueSatisfiesEncoding v e

-- JSON roundtrip works
-- =====================

prop_json_roundtrip e =
 forAll  (valueOfEncoding e)
 $ \v -> (valueOfJSON e . jsonOfValue "☠") v === Right v

-- As above, but all the way to text and back
-- =====================

prop_text_roundtrip e =
 forAll  (valueOfEncoding e)
 $ \v -> (parseValue e (Set.singleton "☠") . renderValue "☠") v === Right v

return []
tests :: IO Bool
tests = $checkAll
