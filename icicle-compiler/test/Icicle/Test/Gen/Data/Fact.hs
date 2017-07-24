{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Test.Gen.Data.Fact (
    genEncoding
  , genStructField
  ) where

import qualified Data.Map.Strict as Map

import           Disorder.Corpus (colours)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Data.Fact

import           P


genEncoding :: Gen Encoding
genEncoding =
  Gen.recursive Gen.choice [
      pure StringEncoding
    , pure IntEncoding
    , pure DoubleEncoding
    , pure BooleanEncoding
    , pure TimeEncoding
    ] [
      StructEncoding . Map.elems . Map.fromList . fmap (\x -> (takeName x, x))
        <$> Gen.list (Range.linear 1 10) genStructField
    , ListEncoding <$> genEncoding
    ]

takeName :: StructField -> Text
takeName (StructField _ name _) =
  name

genStructField :: Gen StructField
genStructField =
  StructField
    <$> Gen.element [Mandatory, Optional]
    <*> Gen.element colours
    <*> genEncoding
