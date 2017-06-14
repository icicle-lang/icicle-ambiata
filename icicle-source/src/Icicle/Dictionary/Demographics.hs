{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary.Demographics (
    demographics
  ) where

import           Icicle.Data
import           Icicle.Dictionary.Data

import qualified Data.Set as Set
import           Data.Maybe

import           P

-- | Example demographics dictionary
-- Hard-coded for now
demographics :: Dictionary
demographics =
 Dictionary
 [ DictionaryEntry (fromJust . asAttributeName $ "gender")
                   (ConcreteDefinition StringEncoding Set.empty unkeyed)
                   nsp
 , DictionaryEntry (fromJust . asAttributeName $ "age")
                   (ConcreteDefinition IntEncoding    Set.empty unkeyed)
                   nsp
 , DictionaryEntry (fromJust . asAttributeName $ "state_of_residence")
                   (ConcreteDefinition StringEncoding Set.empty unkeyed)
                   nsp
 , DictionaryEntry (fromJust . asAttributeName $ "salary")
                   (ConcreteDefinition IntEncoding    Set.empty unkeyed)
                   nsp
 , DictionaryEntry (fromJust . asAttributeName $ "injury")
                   (ConcreteDefinition
                      (StructEncoding
                        [StructField Mandatory (fromJust . asAttributeName $ "location") StringEncoding
                        ,StructField Mandatory (fromJust . asAttributeName $ "severity") IntEncoding])
                      Set.empty unkeyed)
                   nsp
 ]
 []
 where nsp = fromJust . asNamespace $ "default"
