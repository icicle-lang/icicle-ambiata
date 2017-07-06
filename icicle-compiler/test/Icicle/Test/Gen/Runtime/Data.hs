{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Test.Gen.Runtime.Data where

import qualified Data.ByteString.Char8 as Char8
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Disorder.Corpus
import           Disorder.Jack

import           Icicle.Data.Name
import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Logical as Logical
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped

import           P

import qualified X.Data.Vector.Cons as Cons


genBool64 :: Jack Bool64
genBool64 =
  elements [False64, True64]

genTime64 :: Jack Time64
genTime64 =
  fmap packTime $
    UnpackedTime64
      <$> choose (1601, 2999)
      <*> choose (1, 12)
      <*> choose (1, 28)
      <*> choose (0, 86399)

genError64 :: Jack Error64
genError64 =
  elements [Tombstone64] -- FIXME Fold1NoValue64, CannotCompute64

genField :: Jack a -> Jack (Field a)
genField g =
  Field <$> elements boats <*> g

genSchema :: Jack Schema
genSchema =
  oneOfRec [
      pure Schema.Unit
    , pure Schema.Bool
    , pure Schema.Int
    , pure Schema.Double
    , pure Schema.Time
    , pure Schema.String
    ] [
      Schema.Sum <$> genSchema <*> genSchema
    , Schema.Option <$> genSchema
    , Schema.Result <$> genSchema
    , Schema.Pair <$> genSchema <*> genSchema
    , Schema.Struct . Cons.unsafeFromList <$> listOfN 1 5 (genField genSchema)
    , Schema.Array <$> genSchema
    , Schema.Map <$> genSchema <*> genSchema
    ]

genSchemaNoResult :: Jack Schema
genSchemaNoResult =
  oneOfRec [
      pure Schema.Unit
    , pure Schema.Bool
    , pure Schema.Int
    , pure Schema.Double
    , pure Schema.Time
    , pure Schema.String
    ] [
      Schema.Sum <$> genSchemaNoResult <*> genSchemaNoResult
    , Schema.Option <$> genSchemaNoResult
    , Schema.Pair <$> genSchemaNoResult <*> genSchemaNoResult
    , Schema.Struct . Cons.unsafeFromList <$> listOfN 1 5 (genField genSchemaNoResult)
    , Schema.Array <$> genSchemaNoResult
    , Schema.Map <$> genSchemaNoResult <*> genSchemaNoResult
    ]

genValue :: Schema -> Jack Value
genValue = \case
  Schema.Unit ->
    pure Logical.Unit
  Schema.Bool ->
    Logical.Bool <$> genBool64
  Schema.Int ->
    Logical.Int <$> sizedBounded
  Schema.Double ->
    Logical.Double <$> arbitrary
  Schema.Time ->
    Logical.Time <$> genTime64
  Schema.String ->
    Logical.String . Char8.pack . filter (/= '\0') <$> listOfN 0 20 arbitrary
  Schema.Sum x y ->
    oneOf [
        Logical.Left <$> genValue x
      , Logical.Right <$> genValue y
      ]
  Schema.Option x ->
    oneOf [
        pure Logical.None
      , Logical.Some <$> genValue x
      ]
  Schema.Result x ->
    oneOf [
        Logical.Error <$> genError64
      , Logical.Success <$> genValue x
      ]
  Schema.Pair x y ->
    Logical.Pair <$> genValue x <*> genValue y
  Schema.Struct fields ->
    Logical.Struct <$> traverse (genValue . fieldData) fields
  Schema.Array x ->
    Logical.Array . Boxed.fromList <$> listOfN 0 10 (genValue x)
  Schema.Map k v ->
    Logical.Map . Map.fromList <$> listOfN 0 10 ((,) <$> genValue k <*> genValue v)

genColumn :: Schema -> Jack Column
genColumn schema =
  justOf $
    rightToMaybe . Striped.fromLogical schema . Boxed.fromList <$> listOfN 0 10 (genValue schema)

genColumnN :: Int -> Schema -> Jack Column
genColumnN n schema =
  justOf $
    rightToMaybe . Striped.fromLogical schema . Boxed.fromList <$> vectorOf n (genValue schema)

genSingleton :: Schema -> Jack (Value, Column)
genSingleton schema =
  justOf $ do
    x <- genValue schema
    pure . fmap (x,) . rightToMaybe . Striped.fromLogical schema $ Boxed.singleton x

genEntityHash :: Jack EntityHash
genEntityHash =
  EntityHash
    <$> choose (0, 5)

genEntityId :: Jack EntityId
genEntityId =
  EntityId
    <$> elements simpsons

genEntityKey :: Jack EntityKey
genEntityKey =
  EntityKey
    <$> genEntityHash
    <*> genEntityId

genEntityInputColumn :: Schema -> Jack InputColumn
genEntityInputColumn schema = do
  column <- genColumn (Schema.Result schema)

  let
    n =
      Striped.length column

  times <- Storable.fromList <$> vectorOf n genTime64
  pure $ InputColumn (Storable.singleton $ fromIntegral n) times column

genInputColumn :: Int -> Jack InputColumn
genInputColumn n_entities =
  justOf $ do
    schema <- genSchemaNoResult
    columns <- vectorOf n_entities (genEntityInputColumn schema)
    pure . rightToMaybe . concatInputColumn $ Cons.unsafeFromList columns

genInputId :: Jack InputId
genInputId =
  justOf . fmap parseInputId $
    (\ns n -> ns <> ":" <> n)
      <$> elements colours
      <*> elements muppets

genInputColumns :: Int -> Jack (Map InputId InputColumn)
genInputColumns n_entities =
  Map.fromList
    <$> listOfN 1 5 ((,) <$> genInputId <*> genInputColumn n_entities)

genInput :: Jack Input
genInput = do
  entities <- Boxed.fromList <$> listOfN 1 5 genEntityKey
  Input entities
    <$> genInputColumns (Boxed.length entities)

genOutputId :: Jack OutputId
genOutputId =
  justOf . fmap parseOutputId $
    (\ns n -> ns <> ":" <> n)
      <$> elements cooking
      <*> elements southpark

genOutputColumn :: Int -> Jack Column
genOutputColumn n_entities =
  justOf $ do
    schema <- genSchemaNoResult
    columns <- vectorOf n_entities (genColumn schema)
    pure . rightToMaybe . Striped.unsafeConcat $ Cons.unsafeFromList columns

genOutputColumns :: Int -> Jack (Map OutputId Column)
genOutputColumns n_entities =
  Map.fromList
    <$> listOfN 1 5 ((,) <$> genOutputId <*> genOutputColumn n_entities)

genOutput :: Jack a -> Jack (Output a)
genOutput genKey = do
  keys <- Boxed.fromList <$> listOfN 1 5 genKey
  Output keys
    <$> genOutputColumns (Boxed.length keys)

genSnapshotKey :: Jack SnapshotKey
genSnapshotKey =
  SnapshotKey <$> genEntityKey

genChordKey :: Jack ChordKey
genChordKey =
  ChordKey
    <$> genEntityKey
    <*> elements weather
