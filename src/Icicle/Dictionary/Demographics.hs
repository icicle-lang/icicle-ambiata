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
 [ DictionaryEntry (mkAttribute "gender")             (ConcreteDefinition StringEncoding Set.empty)
 , DictionaryEntry (mkAttribute "age")                (ConcreteDefinition IntEncoding Set.empty)
 , DictionaryEntry (mkAttribute "state_of_residence") (ConcreteDefinition StringEncoding Set.empty)
 , DictionaryEntry (mkAttribute "salary")             (ConcreteDefinition IntEncoding Set.empty)
 , DictionaryEntry (mkAttribute "injury")             (ConcreteDefinition (StructEncoding
                        [StructField Mandatory (mkAttribute "location") StringEncoding
                        ,StructField Mandatory (mkAttribute "severity") IntEncoding]) Set.empty)
 ]
 []
