{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Test.Sea.Psv.Failure where

import qualified Data.List as List

import           System.IO

import           Test.QuickCheck (forAll, (==>))

import           P

import           Disorder.Core.IO

import           Icicle.Data

import           Icicle.Sea.Eval

import           Icicle.Test.Arbitrary
import           Icicle.Test.Sea.Psv.Base


prop_failure_entity_out_of_order
 | dup <- DoNotAllowDupTime
 = forAll genSumErrorFactType $ \inputType ->
   forAll (genWTA1 inputType) $ \wta ->
   forAll (genWellTypedForSingleAttribute dup wta) $ \wt ->
   List.length (List.nub (fmap eavtEntity (wtFacts wt))) > 1 ==>
   forAll (genPsvConstants wt) $ \psv ->
     testIO . testEitherT $ do
       let
         wtOutOfOrderEntities =
           wt { wtFacts = List.reverse . List.sortBy (comparing eavtEntity) . wtFacts $ wt }
       runTest wtOutOfOrderEntities psv . optExpectFailure $ dup

prop_failure_time_out_of_order
 = forAll genSumErrorFactType $ \inputType ->
   forAll (genWTA1 inputType) $ \wta ->
   forAll (genWellTypedForSingleAttribute DoNotAllowDupTime wta) $ \wt ->
   List.length (wtFacts wt) > 1 ==>
   List.all (>1)
     (fmap (List.length . List.nubBy ((==) `on` (atTime . eavtValue))) .
      List.groupBy ((==) `on` eavtEntity) . wtFacts $ wt) ==>
   forAll (genPsvConstants wt) $ \psv ->
     testIO . testEitherT $ do
       let
         wtOutOfOrderTimes =
           wt { wtFacts = List.reverse . List.sortBy (comparing (atTime . eavtValue)). wtFacts $ wt }
       runTest wtOutOfOrderTimes psv . optExpectFailure $ DoNotAllowDupTime

prop_dup_time
 = forAll genSumErrorFactType $ \inputType ->
   forAll (genWTA1 inputType) $ \wta ->
   forAll (genWellTypedForSingleAttribute AllowDupTime wta) $ \wt ->
   List.length (wtFacts wt) > 1 ==>
    forAll (genPsvConstants wt) $ \psv ->
      testIO . testEitherT $ do
        let wt' = wt { wtFacts = List.head (wtFacts wt) : wtFacts wt }
        runTest wt' psv . optExpectSuccess $ AllowDupTime
        runTest wt' psv . optExpectFailure $ DoNotAllowDupTime

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 100)
