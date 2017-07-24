{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Serial.Zebra where

import           Hedgehog

import           Icicle.Runtime.Serial.Zebra
import           Icicle.Test.Gen.Runtime.Data

import           P

import           System.IO (IO)


trippingBoth :: (MonadTest m, Monad f, Show (f a), Show (f b), Eq (f a)) => a -> (a -> f b) -> (b -> f a) -> m ()
trippingBoth x to from =
  let
    original =
      pure x

    intermediate =
      to x

    roundtrip =
      from =<< intermediate
  in
    if original == roundtrip then
      success
    else do
      annotateShow intermediate
      original === roundtrip

prop_roundtrip_zebra_input_schema :: Property
prop_roundtrip_zebra_input_schema =
  property $ do
    x <- forAll genInputSchemas
    trippingBoth x encodeInputSchemas decodeInputSchemas

prop_roundtrip_zebra_column_schema :: Property
prop_roundtrip_zebra_column_schema =
  property $ do
    x <- forAll genSchema
    trippingBoth x (pure . encodeColumnSchema) decodeColumnSchema

prop_roundtrip_zebra_column :: Property
prop_roundtrip_zebra_column =
  property $ do
    schema <- forAll genSchema
    x <- forAll (genColumn schema)
    trippingBoth x encodeColumn decodeColumn

prop_roundtrip_zebra_input :: Property
prop_roundtrip_zebra_input =
  property $ do
    x <- forAll genInput
    trippingBoth x encodeInput decodeInput

prop_roundtrip_zebra_snapshot_output :: Property
prop_roundtrip_zebra_snapshot_output =
  property $ do
    x <- forAll (genOutput genSnapshotKey)
    trippingBoth x encodeSnapshotOutput decodeSnapshotOutput

prop_roundtrip_zebra_chord_output :: Property
prop_roundtrip_zebra_chord_output =
  property $ do
    x <- forAll (genOutput genChordKey)
    trippingBoth x encodeChordOutput decodeChordOutput

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
