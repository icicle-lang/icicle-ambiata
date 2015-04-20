{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Encoding where

import           Icicle.Test.Arbitrary
import           Icicle.Encoding

import           P

import           System.IO

import           Test.QuickCheck


-- Sanity checking the randomly generated values
-- =====================

prop_value_type e =
 forAll (valueOfEncoding e)
 $ \v -> primitiveEncoding e ==> (encodingOfValue v == Just e)

-- JSON roundtrip works
-- =====================

prop_json_roundtrip e =
 forAll  (valueOfEncoding e)
 $ \v -> (valueOfJSON e . jsonOfValue "TOMBSTONE") v === Right v

-- As above, but all the way to text and back
-- =====================

prop_text_roundtrip e =
 forAll  (valueOfEncoding e)
 $ \v -> (parseValue e . renderValue "TOMBSTONE") v === Right v



return []
tests :: IO Bool
tests = $quickCheckAll

