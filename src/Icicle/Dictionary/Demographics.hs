{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary.Demographics (
    demographics
  ) where

import           Icicle.Data

import           P

import           Icicle.Dictionary.Data

-- | Example demographics dictionary
-- Hard-coded for now
demographics :: Dictionary
demographics =
 Dictionary
 [ DictionaryEntry (Attribute "gender")             (ConcreteDefinition StringEncoding)
 , DictionaryEntry (Attribute "age")                (ConcreteDefinition IntEncoding)
 , DictionaryEntry (Attribute "state_of_residence") (ConcreteDefinition StringEncoding)
 , DictionaryEntry (Attribute "salary")             (ConcreteDefinition IntEncoding)
 , DictionaryEntry (Attribute "injury")             (ConcreteDefinition $ StructEncoding
                        [StructField Mandatory (Attribute "location") StringEncoding
                        ,StructField Mandatory (Attribute "severity") IntEncoding])
 ]
