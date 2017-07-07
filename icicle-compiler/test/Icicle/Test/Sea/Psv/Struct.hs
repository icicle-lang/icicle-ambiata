{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE PatternGuards#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Psv.Struct where

import           System.IO

import           Test.QuickCheck (forAll)

import           P

import           Icicle.Sea.Eval

import           Icicle.Test.Arbitrary
import           Icicle.Test.Sea.Psv.Base


prop_success_array_of_struct_input
 | dup <- AllowDupTime
 = forAll genSupportedArrayStructFactType $ \ft ->
   forAll (genWT1 dup (SumErrorFactT ft)) $ \wt ->
   forAll (genPsvConstants wt) $ \psvConstants ->
     testForSuccess dup wt psvConstants

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $checkAllWith TestRunFewer (checkArgsSized 100)
