{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE PatternGuards#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Psv.Success where

import           P

import           System.IO

import           Test.QuickCheck (forAll)

import           Icicle.Sea.Eval

import           Icicle.Test.Arbitrary
import           Icicle.Test.Sea.Psv.Base


prop_success_psv
 | dup <- AllowDupTime
 = forAll genSumErrorFactType $ \inputType ->
   forAll (genWT1 dup inputType) $ \wt ->
   forAll (genPsvConstants wt) $ \psv ->
     testForSuccess dup wt psv

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 100)
