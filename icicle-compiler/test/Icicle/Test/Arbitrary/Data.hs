{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary.Data (
    Var(..)
  , fresh
  , valueOfEncoding
  , module Run
  ) where

import           Icicle.Common.Base hiding (StructField)
import           Icicle.Data
import           Icicle.Data.Time

import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Arbitrary.Run as Run

import           Disorder.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P

import           GHC.Generics

import qualified Data.List as List
import qualified Data.Text as T
import           Data.String
import           Data.Char
import           Data.Hashable (Hashable)

-- | Variables used in random Core and Avalanche programs.
data Var = Var T.Text Int
 deriving (Eq,Ord,Show,Generic)

instance Hashable Var
instance NFData Var

instance IsString Var where
  fromString s
   = let (a,n) = List.partition isAlpha s
     in  Var (T.pack a) (fromMaybe 0 $ readMaybe n)

-- | Generate a fresh variable name that isn't mentioned elsewhere in the program,
-- (assuming that the generated program doesn't mention it)
fresh :: Int -> Name Var
fresh = nameOf . NameBase . Var "_fresh"

--------------------------------------------------------------------------------

instance Arbitrary Entity where
  arbitrary =
    Entity <$> elements simpsons

instance Arbitrary Namespace where
  arbitrary = do
    -- fail violently if not the case
    Just ns <- parseNamespace <$> elements simpsons
    pure ns

instance Arbitrary InputName where
  arbitrary = do
    -- fail violently if not the case
    Just a <- parseInputName <$> elements weather
    return a

instance Arbitrary InputId where
  arbitrary =
    InputId <$> arbitrary <*> arbitrary

instance Arbitrary UnresolvedInputId where
  arbitrary =
    oneof [
        UnqualifiedInput <$> arbitrary
      , QualifiedInput <$> arbitrary
      ]

instance Arbitrary OutputName where
  arbitrary = do
    -- fail violently if not the case
    Just n <- parseOutputName <$> elements muppets
    pure n

instance Arbitrary OutputId where
  arbitrary =
    OutputId <$> arbitrary <*> arbitrary

instance Arbitrary Time where
  arbitrary = do
    potential <- timeOfYMD <$> oneof_vals [2000..2050] <*> oneof_vals [1..12] <*> oneof_vals [1..31]
    case potential of
      Just a  -> pure a
      Nothing -> discard

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
          , TimeEncoding ]
          [ StructEncoding . nubEq <$> listOf1 arbitrary
          , ListEncoding           <$> arbitrary ]
   where
    nubEq
     = List.nubBy ((==) `on` structFieldName)

instance Arbitrary StructField where
  arbitrary =
    StructField <$> arbitrary <*> arbitrary <*> oneof_vals [ StringEncoding, IntEncoding, DoubleEncoding, BooleanEncoding, TimeEncoding ]

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
    TimeEncoding
     -> TimeValue    <$> arbitrary

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
