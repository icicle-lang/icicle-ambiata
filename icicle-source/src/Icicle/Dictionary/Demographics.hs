{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary.Demographics (
    demographics
  ) where

import           Icicle.Data

import           Icicle.Dictionary.Data

import qualified Data.Set                           as Set

-- | Example demographics dictionary
-- Hard-coded for now
demographics :: Dictionary
demographics =
 Dictionary
 [ DictionaryEntry (Attribute "gender")
     (ConcreteDefinition StringEncoding Set.empty FactModeEvent) nsp
 , DictionaryEntry (Attribute "age")
     (ConcreteDefinition IntEncoding    Set.empty FactModeEvent) nsp
 , DictionaryEntry (Attribute "state_of_residence")
     (ConcreteDefinition StringEncoding Set.empty FactModeEvent) nsp
 , DictionaryEntry (Attribute "salary")
     (ConcreteDefinition IntEncoding    Set.empty FactModeEvent) nsp
 , DictionaryEntry (Attribute "injury")
     (ConcreteDefinition
        (StructEncoding
           [ StructField Mandatory (Attribute "location") StringEncoding
           , StructField Mandatory (Attribute "severity") IntEncoding]) Set.empty FactModeEvent) nsp
 ]
 []
 where nsp = Namespace "default"
