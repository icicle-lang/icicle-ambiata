{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary where

import           Icicle.Data

import           Orphanarium.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P
import           Data.List (nubBy)


instance Arbitrary Entity where
  arbitrary =
    Entity <$> elements simpsons

instance Arbitrary Attribute where
  arbitrary =
    Attribute <$> elements viruses

instance Arbitrary DateTime where
  arbitrary =
    DateTime <$> elements muppets -- FIX replace with an actual DateTime

instance Arbitrary Date where
  arbitrary =
    Date <$> elements muppets -- FIX replace with an actual Date


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
    oneof [ return StringEncoding
          , return IntEncoding
          , return DoubleEncoding
          , return BooleanEncoding
          , return DateEncoding
          , StructEncoding . nubEq <$> smaller arbitrary
          , ListEncoding           <$> smaller arbitrary]
   where
    nubEq
     = nubBy ((==) `on` attributeOfStructField)
    
    smaller g
     = sized (\s -> resize (s `div` 2) g)

instance Arbitrary StructField where
  arbitrary =
    StructField <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary StructFieldType where
  arbitrary =
    oneof [return Mandatory, return Optional]

valueOfEncoding :: Encoding -> Gen Value
valueOfEncoding e
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
   = do v <- valueOfEncoding enc
        return [(attr, v)]

  attrValue (StructField Optional attr enc)
   = do b <- arbitrary
        v <- valueOfEncoding enc
        case b of
         True  -> return [(attr, v)]
         False -> return []

  -- This seems to generate way too large lists sometimes
  listOfEncoding le
   = smaller $ listOf (valueOfEncoding le)

  smaller g
   = sized (\s -> resize (s `div` 2) g)
