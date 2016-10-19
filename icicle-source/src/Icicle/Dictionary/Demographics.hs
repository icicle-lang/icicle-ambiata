{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary.Demographics (
    demographics
  ) where

import           Icicle.Data
import           Icicle.Dictionary.Data


-- | Example demographics dictionary
-- Hard-coded for now
demographics :: Dictionary
demographics =
 Dictionary
 [ DictionaryEntry (Attribute "gender")
                   (ConcreteDefinition StringEncoding [] unkeyed)
                   nsp
 , DictionaryEntry (Attribute "age")
                   (ConcreteDefinition IntEncoding    [] unkeyed)
                   nsp
 , DictionaryEntry (Attribute "state_of_residence")
                   (ConcreteDefinition StringEncoding [] unkeyed)
                   nsp
 , DictionaryEntry (Attribute "salary")
                   (ConcreteDefinition IntEncoding    [] unkeyed)
                   nsp
 , DictionaryEntry (Attribute "injury")
                   (ConcreteDefinition
                      (StructEncoding
                        [StructField Mandatory (Attribute "location") StringEncoding
                        ,StructField Mandatory (Attribute "severity") IntEncoding])
                      [] unkeyed)
                   nsp
 ]
 []
 where nsp = Namespace "default"
