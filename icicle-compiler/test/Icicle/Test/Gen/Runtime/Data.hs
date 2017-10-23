{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Test.Gen.Runtime.Data where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Disorder.Corpus

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Data.Name
import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Logical as Logical
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped
import           Icicle.Test.Gen.Data.Name

import           P

import qualified Prelude as Savage

import qualified X.Data.Vector.Cons as Cons


genBool64 :: Gen Bool64
genBool64 =
  Gen.element [False64, True64]

genTime64 :: Gen Time64
genTime64 =
  fmap packTime $
    UnpackedTime64
      <$> Gen.integral (Range.linearFrom 2000 1601 2999)
      <*> Gen.integral (Range.constant 1 12)
      <*> Gen.integral (Range.constant 1 28)
      <*> Gen.integral (Range.constant 0 86399)

genQueryTime :: Gen QueryTime
genQueryTime =
  QueryTime <$> genTime64

genInputTime :: Gen InputTime
genInputTime =
  InputTime <$> genTime64

genSnapshotTime :: Gen SnapshotTime
genSnapshotTime =
  SnapshotTime <$> genQueryTime

genError64 :: Gen Error64
genError64 =
  Gen.enumBounded

genResultError :: Gen Error64
genResultError = do
 -- Generate any tag, but if it's NotAnError just return Tombstone.
 e <- genError64
 case e of
  NotAnError64
    -> return Tombstone64
  _ -> return e

genTombstoneOrSuccess :: Gen Error64
genTombstoneOrSuccess =
  Gen.element [NotAnError64, Tombstone64]

genField :: Gen a -> Gen (Field a)
genField g =
  Field <$> Gen.element boats <*> g

genSchema :: Gen Schema
genSchema =
  Gen.recursive Gen.choice [
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
    , Schema.Struct . Cons.unsafeFromList <$> Gen.list (Range.linear 1 5) (genField genSchema)
    , Schema.Array <$> genSchema
    , Schema.Map <$> genSchema <*> genSchema
    ]

genChar :: Gen Char
genChar =
  Gen.filter (/= '\0') Gen.unicode

genString :: Gen ByteString
genString =
  Gen.choice [
      Gen.element viruses
    , Gen.utf8 (Range.linear 0 20) genChar
    ]

genValue :: Schema -> Gen Value
genValue = \case
  Schema.Unit ->
    pure Logical.Unit
  Schema.Bool ->
    Logical.Bool <$> genBool64
  Schema.Int ->
    Logical.Int <$> Gen.int64 Range.linearBounded
  Schema.Double ->
    Logical.Double <$> Gen.double (Range.linearFracFrom 0 (-1e308) (1e308))
  Schema.Time ->
    Logical.Time <$> genTime64
  Schema.String ->
    Logical.String <$> genString
  Schema.Sum x y ->
    Gen.choice [
        Logical.Left <$> genValue x
      , Logical.Right <$> genValue y
      ]
  Schema.Option x ->
    Gen.choice [
        pure Logical.None
      , Logical.Some <$> genValue x
      ]
  Schema.Result x ->
    Gen.choice [
        Logical.Error <$> genResultError
      , Logical.Success <$> genValue x
      ]
  Schema.Pair x y ->
    Logical.Pair <$> genValue x <*> genValue y
  Schema.Struct fields ->
    Logical.Struct <$> traverse (genValue . fieldData) fields
  Schema.Array x ->
    Logical.Array . Boxed.fromList <$> Gen.list (Range.linear 0 10) (genValue x)
  Schema.Map k v ->
    Logical.Map <$> Gen.map (Range.linear 0 10) ((,) <$> genValue k <*> genValue v)

genColumn :: Schema -> Gen Column
genColumn schema =
  Gen.just $
    rightToMaybe . Striped.fromLogical schema . Boxed.fromList <$> Gen.list (Range.linear 0 10) (genValue schema)

genColumnN :: Int -> Schema -> Gen Column
genColumnN n schema =
  Gen.just $
    rightToMaybe . Striped.fromLogical schema . Boxed.fromList <$> Gen.list (Range.singleton n) (genValue schema)

genSingleton :: Schema -> Gen (Value, Column)
genSingleton schema =
  Gen.just $ do
    x <- genValue schema
    pure . fmap (x,) . rightToMaybe . Striped.fromLogical schema $ Boxed.singleton x

genEntityHash :: Gen EntityHash
genEntityHash =
  EntityHash
    <$> Gen.word32 (Range.constant 0 5)

genEntityId :: Gen EntityId
genEntityId =
  EntityId
    <$> Gen.element simpsons

genEntityKey :: Gen EntityKey
genEntityKey = do
  eid <- genEntityId
  pure $
    EntityKey (EntityHash . fromIntegral . ByteString.length $ unEntityId eid) eid

genEntityInputColumn :: Schema -> Gen InputColumn
genEntityInputColumn schema = do
  column <- genColumn schema

  let
    n =
      Striped.length column

  times <- Storable.fromList . List.sort <$> Gen.list (Range.singleton n) genInputTime
  tombstones <- Storable.fromList <$> Gen.list (Range.singleton n) genTombstoneOrSuccess

  pure $ InputColumn (Storable.singleton $ fromIntegral n) times tombstones column

genInputColumn :: Int -> Schema -> Gen InputColumn
genInputColumn n_entities schema = do
  columns <- Gen.list (Range.singleton n_entities) (genEntityInputColumn schema)
  either (\x -> Savage.error $ "genInputColumn: " <> show x) pure . concatInputColumn $
    Cons.unsafeFromList columns

genInputColumns :: Int -> Gen (Map InputId InputColumn)
genInputColumns n_entities =
  Gen.map (Range.linear 1 5) $
    (,) <$> genInputId <*> (genInputColumn n_entities =<< genSchema)

genInputN :: Int -> Gen Input
genInputN n_entities = do
  entities <- Boxed.fromList <$> Gen.list (Range.singleton n_entities) genEntityKey
  Input entities
    <$> genInputColumns (Boxed.length entities)

genInput :: Gen Input
genInput = do
  genInputN =<< Gen.int (Range.linear 1 5)

genInputSchemas :: Gen (Map InputId Schema)
genInputSchemas =
  Map.fromList
    <$> Gen.list (Range.linear 1 5) ((,) <$> genInputId <*> genSchema)

genOutputColumn :: Int -> Gen Column
genOutputColumn n_entities =
  Gen.just $ do
    schema <- genSchema
    columns <- Gen.list (Range.singleton n_entities) (genColumn schema)
    pure . rightToMaybe . Striped.unsafeConcat $ Cons.unsafeFromList columns

genOutputColumns :: Int -> Gen (Map OutputId Column)
genOutputColumns n_entities =
  Map.fromList
    <$> Gen.list (Range.linear 1 5) ((,) <$> genOutputId <*> genOutputColumn n_entities)

genOutputN :: Int -> Gen a -> Gen (Output a)
genOutputN n genKey = do
  keys <- Boxed.fromList <$> Gen.list (Range.singleton n) genKey
  Output keys
    <$> genOutputColumns (Boxed.length keys)

genOutput :: Gen a -> Gen (Output a)
genOutput genKey = do
  n <- Gen.int (Range.linear 1 5)
  genOutputN n genKey

genSnapshotKey :: Gen SnapshotKey
genSnapshotKey =
  SnapshotKey <$> genEntityKey

genChordKey :: Gen ChordKey
genChordKey =
  ChordKey
    <$> genEntityKey
    <*> Gen.element weather
