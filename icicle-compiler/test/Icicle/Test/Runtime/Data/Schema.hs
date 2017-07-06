{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Data.Schema where

import qualified Data.List.NonEmpty as NonEmpty

import           Disorder.Jack

import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Schema as Schema
import           Icicle.Test.Arbitrary.Run
import           Icicle.Test.Gen.Runtime.Data

import           P

import           System.IO (IO)

import qualified X.Data.Vector.Cons as Cons


uniqueSortedFields :: Schema -> Schema
uniqueSortedFields = \case
  Schema.Unit ->
    Schema.Unit
  Schema.Bool ->
    Schema.Bool
  Schema.Int ->
    Schema.Int
  Schema.Double ->
    Schema.Double
  Schema.Time ->
    Schema.Time
  Schema.Sum x y ->
    Schema.Sum (uniqueSortedFields x) (uniqueSortedFields y)
  Schema.Option x ->
    Schema.Option (uniqueSortedFields x)
  Schema.Result x ->
    Schema.Result (uniqueSortedFields x)
  Schema.Pair x y ->
    Schema.Pair (uniqueSortedFields x) (uniqueSortedFields y)
  Schema.String ->
    Schema.String
  Schema.Array x ->
    Schema.Array (uniqueSortedFields x)
  Schema.Map x y ->
    Schema.Map (uniqueSortedFields x) (uniqueSortedFields y)

  Schema.Struct fields0 ->
    let
      fields =
        Cons.fromNonEmpty .
        NonEmpty.nubBy ((==) `on` fieldName) .
        NonEmpty.sort .
        fmap (fmap uniqueSortedFields) $
        Cons.toNonEmpty fields0
    in
      Schema.Struct fields

prop_roundtrip_schema :: Property
prop_roundtrip_schema =
  gamble genSchema $
    tripping Schema.toValType Schema.fromValType . uniqueSortedFields

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore checkArgs
