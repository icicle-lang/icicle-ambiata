{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Icicle.Test.Sea.Name where

import qualified Data.Text as Text
import           System.IO
import           Test.QuickCheck

import           Disorder.Core.Gen
import           P

import           Icicle.Test.Sea.Utils
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

prop_mangle_unique :: Property
prop_mangle_unique =
  forAll genValidUtf8 $ \text1 ->
  forAll genValidUtf8 $ \text2 ->
  mangleToSeaName text1 == mangleToSeaName text2 ==>
  text1 == text2

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunNormal checkArgs
