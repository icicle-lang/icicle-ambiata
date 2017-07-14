{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Test.Runtime.Serial.Psv.Schema where

import           Disorder.Corpus (muppets, boats, viruses)
import           Disorder.Jack (Property, Jack, quickCheckAll, tripping, gamble)
import           Disorder.Jack (elements, listOfN, listOf, oneOfRec, sized)

import           Icicle.Runtime.Serial.Psv.Schema

import           P

import           System.IO (IO)


genPrimitive :: Jack PsvPrimitive
genPrimitive =
  elements [
      PsvBoolean
    , PsvInt
    , PsvDouble
    , PsvString
    , PsvDate
    ]

genStructField :: Jack PsvStructField
genStructField =
  PsvStructField
    <$> elements muppets
    <*> genEncoding

genEncoding :: Jack PsvEncoding
genEncoding =
  oneOfRec [
      PsvPrimitive <$> genPrimitive
    ] [
      PsvStruct <$> sized (\n -> listOfN 0 (n `div` 5) genStructField)
    , PsvList <$> genEncoding
    , PsvPair <$> genEncoding <*> genEncoding
    ]

genColumn :: Jack PsvColumn
genColumn =
  PsvColumn
    <$> elements boats
    <*> genEncoding

genMissingValue :: Jack PsvMissingValue
genMissingValue =
  PsvMissingValue
    <$> elements viruses

genSchema :: Jack PsvSchema
genSchema =
  PsvSchema
    <$> genMissingValue
    <*> listOf genColumn

prop_psv_pretty_schema_roundtrip :: Property
prop_psv_pretty_schema_roundtrip =
  gamble genSchema $
    tripping renderPrettyPsvSchema parsePsvSchema

prop_psv_compact_schema_roundtrip :: Property
prop_psv_compact_schema_roundtrip =
  gamble genSchema $
    tripping renderCompactPsvSchema parsePsvSchema

return []
tests :: IO Bool
tests =
  $quickCheckAll
