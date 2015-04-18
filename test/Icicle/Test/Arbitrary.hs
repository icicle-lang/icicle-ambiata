{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary where

import           Icicle.Data

import           Orphanarium.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P


instance Arbitrary Entity where
  arbitrary =
    Entity <$> elements simpsons

instance Arbitrary Attribute where
  arbitrary =
    Attribute <$> elements viruses

instance Arbitrary DateTime where
  arbitrary =
    DateTime <$> elements muppets -- FIX replace with an actual DateTime

instance Arbitrary Fact' where
  arbitrary =
    Fact'
      <$> arbitrary
      <*> arbitrary
      <*> elements cooking

instance Arbitrary a => Arbitrary (AsAt a) where
  arbitrary =
    AsAt
      <$> arbitrary
      <*> arbitrary
