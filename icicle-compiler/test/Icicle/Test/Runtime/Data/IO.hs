{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Data.IO where

import qualified Data.Vector.Storable as Storable

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Runtime.Data.IO
import           Icicle.Test.Gen.Runtime.Data

import           P

import           System.IO (IO)

import qualified X.Data.Vector.Cons as Cons


prop_roundtrip_split_input_column :: Property
prop_roundtrip_split_input_column =
  property $ do
    schema <- forAll genSchema
    n_entities <- forAll . Gen.int $ Range.linear 1 5
    column0 <- forAll $ genInputColumn n_entities schema
    column1 <- forAll $ genInputColumn n_entities schema

    column <- evalEither . concatInputColumn $ Cons.from2 column0 column1

    let
      (scolumn0, scolumn1) =
        splitInputColumn (Storable.length $ inputLength column0) column

    column0 === scolumn0
    column1 === scolumn1

prop_roundtrip_split_input :: Property
prop_roundtrip_split_input =
  property $ do
    n_entities <- forAll . Gen.int $ Range.linear 1 20
    n <- forAll . Gen.int $ Range.linear 1 n_entities
    input0 <- forAll $ genInputN n_entities

    let
      inputs =
        splitInputN n input0
    annotateShow inputs

    input1 <- evalEither $ concatInput inputs

    input0 === input1

prop_roundtrip_split_output :: Property
prop_roundtrip_split_output =
  property $ do
    n_entities <- forAll . Gen.int $ Range.linear 1 20
    n <- forAll . Gen.int $ Range.linear 1 n_entities
    output0 <- forAll $ genOutputN n_entities genSnapshotKey

    let
      outputs =
        splitOutputN n output0
    annotateShow outputs

    output1 <- evalEither $ concatOutput outputs

    output0 === output1

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
