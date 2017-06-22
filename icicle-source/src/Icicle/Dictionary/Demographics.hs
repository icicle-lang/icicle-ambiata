{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Icicle.Dictionary.Demographics (
    demographics
  ) where

import           Icicle.Data
import           Icicle.Dictionary.Data

import qualified Data.Set as Set

import           P

-- | Example demographics dictionary
-- Hard-coded for now
demographics :: Dictionary
demographics =
  let
    input i e =
      DictionaryInput i e Set.empty unkeyed

    inputs =
      mapOfInputs [
          input [inputid|default:gender|]
            StringEncoding

        , input [inputid|default:age|]
            IntEncoding

        , input [inputid|default:state_of_residence|]
            StringEncoding

        , input [inputid|default:salary|]
            IntEncoding

        , input [inputid|default:injury|] $
            StructEncoding [
                StructField Mandatory "location" StringEncoding
              , StructField Mandatory "severity" IntEncoding
              ]
        ]

    outputs =
      mapOfOutputs []

    functions =
      []
  in
    Dictionary inputs outputs functions
