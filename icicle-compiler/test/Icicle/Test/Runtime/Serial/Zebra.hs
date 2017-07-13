{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Serial.Zebra where

import           Disorder.Jack

import           Icicle.Runtime.Serial.Zebra
import           Icicle.Test.Arbitrary.Run
import           Icicle.Test.Gen.Runtime.Data

import           P

import           System.IO (IO)

import           Text.Show.Pretty (ppShow)


trippingBoth :: (Monad m, Show (m a), Show (m b), Eq (m a)) => (a -> m b) -> (b -> m a) -> a -> Property
trippingBoth to from x =
  let
    original =
      pure x

    intermediate =
      to x

    roundtrip =
      from =<< intermediate
  in
    counterexample "" .
    counterexample "Roundtrip failed." .
    counterexample "" .
    counterexample "=== Intermediate ===" .
    counterexample (ppShow intermediate) .
    counterexample "" $
      property (original === roundtrip)

prop_roundtrip_zebra_input_schema :: Property
prop_roundtrip_zebra_input_schema =
  gamble genInputSchemas $
    trippingBoth encodeInputSchemas decodeInputSchemas

prop_roundtrip_zebra_column_schema :: Property
prop_roundtrip_zebra_column_schema =
  gamble genSchema $
    trippingBoth (pure . encodeColumnSchema) decodeColumnSchema

prop_roundtrip_zebra_column :: Property
prop_roundtrip_zebra_column =
  gamble genSchema $ \schema ->
  gamble (genColumn schema) $
    trippingBoth encodeColumn decodeColumn

prop_roundtrip_zebra_input :: Property
prop_roundtrip_zebra_input =
  gamble genInput $
    trippingBoth encodeInput decodeInput

prop_roundtrip_zebra_snapshot_output :: Property
prop_roundtrip_zebra_snapshot_output =
  gamble (genOutput genSnapshotKey) $
    trippingBoth encodeSnapshotOutput decodeSnapshotOutput

prop_roundtrip_zebra_chord_output :: Property
prop_roundtrip_zebra_chord_output =
  gamble (genOutput genChordKey) $
    trippingBoth encodeChordOutput decodeChordOutput

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunNormal checkArgs
