{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Icicle.Test.Sea.Name where

import           Data.Char (chr)
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import           System.IO
import           Test.QuickCheck

import           Disorder.Core.Gen
import           Disorder.Core.Tripping
import           P

import           Icicle.Test.Arbitrary
import           Icicle.Sea.FromAvalanche.Base

prop_mangle_valid_c_identifier :: Property
prop_mangle_valid_c_identifier =
  forAll genValidUtf8 $ \text1 ->
    not (Text.null text1) ==>
    isJust . asSeaName . takeSeaName . mangleToSeaName $ text1

prop_mangle_works :: Property
prop_mangle_works =
  forAll genValidUtf8 $ \text1 ->
    not (Text.null text1) ==>
    isNothing (asSeaName text1) ==>
    isJust . asSeaName . takeSeaName . mangleToSeaName $ text1

prop_mangle_roundtrip :: Property
prop_mangle_roundtrip =
  forAll genValidUtf8 $
    tripping mangleToSeaName (Just . unmangleSeaName)

getUnmangleIdempotent :: Gen SeaName
getUnmangleIdempotent =
  let
    gen =
      fmap asSeaName .
      fmap Text.pack .
      fmap (filter (/= 'Z')) .
      fmap (filter (/= 'z')) .
      listOf .
      fmap chr $
      choose (0, 255) -- only need latin1 characters to really excercise this
  in
    fmap fromJust $
      gen `suchThat` isJust

prop_unmangle_vanilla :: Property
prop_unmangle_vanilla =
  forAll getUnmangleIdempotent $ \text ->
    takeSeaName text === unmangleSeaName text

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore checkArgs
