{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary where

import           Icicle.Common.Base (WindowUnit (..))
import           Icicle.Data
import           Icicle.Data.DateTime

import           Icicle.Test.Arbitrary.Base
import           Disorder.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P
import           Data.List (nubBy)


instance Arbitrary Entity where
  arbitrary =
    Entity <$> elements simpsons

instance Arbitrary Attribute where
  arbitrary =
    Attribute <$> elements weather

instance Arbitrary DateTime where
  arbitrary =
    unsafeDateOfYMD <$> oneof_vals [2010..2014] <*> oneof_vals [1..12] <*> oneof_vals [1..28]

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

instance Arbitrary Encoding where
  arbitrary =
    oneof_sized_vals
          [ StringEncoding
          , IntEncoding
          , DoubleEncoding
          , BooleanEncoding
          , DateEncoding ]
          [ StructEncoding . nubEq <$> listOf1 arbitrary
          , ListEncoding           <$> arbitrary ]
   where
    nubEq
     = nubBy ((==) `on` attributeOfStructField)

instance Arbitrary StructField where
  arbitrary =
    StructField <$> arbitrary <*> arbitrary <*> oneof_vals [ StringEncoding, IntEncoding, DoubleEncoding, BooleanEncoding, DateEncoding ]

instance Arbitrary StructFieldType where
  arbitrary =
    oneof [return Mandatory, return Optional]

valueOfEncoding :: Encoding -> Gen Value
valueOfEncoding e = frequency [(1, pure Tombstone), (5, valueOfEncoding' e)]

valueOfEncoding' :: Encoding -> Gen Value
valueOfEncoding' e
 = case e of
    StringEncoding
     -> StringValue  <$> arbitrary
    IntEncoding
     -> IntValue     <$> arbitrary
    DoubleEncoding
     -- Only generate doubles with small amount of precision
     -- as going to and from text can lose something.
     -> DoubleValue  . (/10) . fromIntegral
                     <$> (arbitrary :: Gen Int)
    BooleanEncoding
     -> BooleanValue <$> arbitrary
    DateEncoding
     -> DateValue    <$> arbitrary

    StructEncoding sfs
     ->  StructValue . Struct . P.concat
     <$> mapM attrValue sfs

    ListEncoding le
     ->  ListValue   . List
     <$> listOfEncoding le
 where

  attrValue (StructField Mandatory attr enc)
   = do v <- valueOfEncoding' enc
        return [(attr, v)]

  attrValue (StructField Optional attr enc)
   = do b <- arbitrary
        v <- valueOfEncoding' enc
        case b of
         True  -> return [(attr, v)]
         False -> return []

  -- This seems to generate way too large lists sometimes
  listOfEncoding le
   = smaller $ listOf (valueOfEncoding' le)

instance Arbitrary WindowUnit where
 arbitrary
  = oneof
        [ Days   <$> pos
        , Months <$> pos
        , Weeks  <$> pos ]
  where
   pos = abs <$> arbitrary
