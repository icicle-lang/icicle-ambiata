{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Sea.Name where

import           Data.Char (chr)
import           Data.Maybe (fromJust)
import qualified Data.Text as Text

import           Disorder.Core.Gen
import           Disorder.Core.Tripping

import           Icicle.Test.Arbitrary
import           Icicle.Sea.Name

import           P

import           System.IO

import           Test.QuickCheck


prop_mangle_valid_c_identifier :: Property
prop_mangle_valid_c_identifier =
  forAll genValidUtf8 $ \text1 ->
    not (Text.null text1) ==>
    isJust . parseSeaName . renderSeaName $ mangle text1

prop_mangle_works :: Property
prop_mangle_works =
  forAll genValidUtf8 $ \text1 ->
    not (Text.null text1) ==>
    isNothing (parseSeaName text1) ==>
    isJust . parseSeaName . renderSeaName $ mangle text1

prop_mangle_roundtrip :: Property
prop_mangle_roundtrip =
  forAll genValidUtf8 $
    tripping mangle (Just . unmangle)

getUnmangleIdempotent :: Gen SeaName
getUnmangleIdempotent =
  let
    gen =
      fmap parseSeaName .
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
    renderSeaName text === unmangle text

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore checkArgs
