{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Runtime.Serial.Zebra (
    ZebraError(..)

  , decodeInput
  , encodeInput

  , decodeSnapshotOutput
  , encodeSnapshotOutput

  , decodeChordOutput
  , encodeChordOutput

  , resolveDictionary
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Boxed
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Storable as Storable

import           Icicle.Data.Name
import           Icicle.Dictionary.Data
import           Icicle.Runtime.Data.IO
import           Icicle.Runtime.Data.Primitive
import           Icicle.Runtime.Data.Schema (Schema)
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Icicle

import           P

import           X.Data.Vector.Cons (Cons)
import qualified X.Data.Vector.Cons as Cons

import qualified Zebra.Table.Data as Zebra
import qualified Zebra.Table.Encoding as ZEncoding
import           Zebra.Table.Schema (SchemaError)
import qualified Zebra.Table.Schema as ZSchema
import qualified Zebra.Table.Striped as Zebra
import           Zebra.Time (TimeError)
import qualified Zebra.Time as Zebra
import qualified Zebra.X.Vector.Generic as Generic -- FIXME move to x-vector
import           Zebra.X.Vector.Segment (SegmentError)
import qualified Zebra.X.Vector.Segment as Segment -- FIXME move to x-vector


data OptionBehaviour =
    Normal
  | TopLevel

data ZebraError =
    ZebraSchemaError !SchemaError
  | ZebraTimeError !TimeError
  | ZebraUnexpectedBinaryEncoding !ZEncoding.Binary
  | ZebraUnexpectedIntEncoding !ZEncoding.Int
  | ZebraUnexpectedReversed !ZSchema.Column
  | ZebraEntityIdSegmentDescriptorMismatch !SegmentError
  | ZebraTimeKeySegmentDescriptorMismatch !SegmentError
  | ZebraExpectedEntityKey ![Zebra.Field ZSchema.Column]
  | ZebraExpectedChordKey ![Zebra.Field ZSchema.Column]
  | ZebraExpectedTimeKey ![Zebra.Field ZSchema.Column]
  | ZebraUnknownEnum ![Zebra.Variant ZSchema.Column]
  | ZebraInvalidInputId !Text
  | ZebraNoInputs
  | ZebraNoOutputs
  | ZebraResolveDictionaryError !Schema.SchemaError
    deriving (Eq, Show)

------------------------------------------------------------------------

snoc :: Cons Boxed.Vector a -> a -> Cons Boxed.Vector a
snoc xs0 x =
  let
    (hd, tl) =
      Cons.uncons xs0
  in
    Cons.from hd (tl <> Boxed.singleton x)

------------------------------------------------------------------------
-- Input: Zebra -> Icicle

zebraEpoch :: Time64
zebraEpoch =
  Time64 0x0640030100000000

decodeDate :: Zebra.Date -> Time64
decodeDate x0 =
  let
    !x =
      Zebra.toCalendarDate x0

    !unpacked =
      UnpackedTime64
        (fromIntegral . Zebra.unYear $ Zebra.dateYear x)
        (fromIntegral . Zebra.unMonth $ Zebra.dateMonth x)
        (fromIntegral . Zebra.unDay $ Zebra.dateDay x)
        0

    !packed =
      packTime unpacked
  in
    -- FIXME icicle should use the same epoch as zebra so we don't need to do this for defaults
    if packed == zebraEpoch then
      Time64 0
    else
      packed

decodeTime :: Zebra.Time -> Time64
decodeTime x0 =
  let
    Zebra.CalendarTime date time =
      Zebra.toCalendarTime x0

    !microseconds =
      Zebra.fromTimeOfDay time

    seconds :: Zebra.Seconds
    !seconds =
      fromIntegral microseconds `quot` 1000000

    !unpacked =
      UnpackedTime64
        (fromIntegral . Zebra.unYear $ Zebra.dateYear date)
        (fromIntegral . Zebra.unMonth $ Zebra.dateMonth date)
        (fromIntegral . Zebra.unDay $ Zebra.dateDay date)
        (fromIntegral seconds)

    !packed =
      packTime unpacked
  in
    -- FIXME icicle should use the same epoch as zebra so we don't need to do this for defaults
    if packed == zebraEpoch then
      Time64 0
    else
      packed

newTimeDecoder :: ZEncoding.Int -> Either ZebraError (Int64 -> Either ZebraError Time64)
newTimeDecoder = \case
  ZEncoding.Date ->
    pure $
      bimap ZebraTimeError decodeDate . ZEncoding.decodeDate
  ZEncoding.TimeSeconds ->
    pure $
      bimap ZebraTimeError decodeTime . ZEncoding.decodeTimeSeconds
  ZEncoding.TimeMilliseconds ->
    pure $
      bimap ZebraTimeError decodeTime . ZEncoding.decodeTimeMilliseconds
  ZEncoding.TimeMicroseconds ->
    pure $
      bimap ZebraTimeError decodeTime . ZEncoding.decodeTimeMicroseconds
  encoding ->
    Left $ ZebraUnexpectedIntEncoding encoding

decodeIntTime :: ZEncoding.Int -> Storable.Vector Int64 -> Either ZebraError (Storable.Vector Time64)
decodeIntTime encoding xs = do
  decoder <- newTimeDecoder encoding
  Storable.mapM decoder xs

decodeBoolTags :: Storable.Vector Zebra.Tag -> Storable.Vector Bool64
decodeBoolTags =
  Storable.map (Bool64 . fromIntegral)

decodeTombstones :: Storable.Vector Zebra.Tag -> Storable.Vector Error64
decodeTombstones =
  Storable.map $ \case
    0 ->
      Tombstone64
    _ ->
      NotAnError64

decodeField :: OptionBehaviour -> Zebra.Field Zebra.Column -> Either ZebraError (Field Icicle.Column)
decodeField option (Zebra.Field (Zebra.FieldName name) column) =
  Field name <$> decodeIcicleColumn option column

decodeIcicleNested :: OptionBehaviour -> Storable.Vector Int64 -> Zebra.Table -> Either ZebraError Icicle.Column
decodeIcicleNested option ns = \case
  Zebra.Binary _ encoding@ZEncoding.Binary _ ->
    Left $ ZebraUnexpectedBinaryEncoding encoding
  Zebra.Binary _ ZEncoding.Utf8 bs ->
    pure $ Icicle.String ns bs

  Zebra.Array _ column ->
    Icicle.Array ns <$> decodeIcicleColumn option column

  Zebra.Map _ kcolumn vcolumn ->
    Icicle.Map ns <$> decodeIcicleColumn option kcolumn <*> decodeIcicleColumn option vcolumn

decodeIcicleColumn :: OptionBehaviour -> Zebra.Column -> Either ZebraError Icicle.Column
decodeIcicleColumn option = \case
  Zebra.Unit n ->
    pure $ Icicle.Unit n

  Zebra.Int _ ZEncoding.Int xs ->
    pure $ Icicle.Int xs
  Zebra.Int _ encoding@ZEncoding.Date xs ->
    Icicle.Time <$> decodeIntTime encoding xs
  Zebra.Int _ encoding@ZEncoding.TimeSeconds xs ->
    Icicle.Time <$> decodeIntTime encoding xs
  Zebra.Int _ encoding@ZEncoding.TimeMilliseconds xs ->
    Icicle.Time <$> decodeIntTime encoding xs
  Zebra.Int _ encoding@ZEncoding.TimeMicroseconds xs ->
    Icicle.Time <$> decodeIntTime encoding xs

  Zebra.Double _ xs ->
    pure $ Icicle.Double xs

  Zebra.Enum _ tags variants ->
    case Cons.toList variants of
      [Zebra.Variant "false" (Zebra.Unit _), Zebra.Variant "true" (Zebra.Unit _)] ->
        pure $ Icicle.Bool (decodeBoolTags tags)

      [Zebra.Variant "none" (Zebra.Unit _), Zebra.Variant "some" x] ->
        case option of
          Normal ->
            Icicle.Option (decodeBoolTags tags) <$> decodeIcicleColumn option x
          TopLevel ->
            Icicle.Result (decodeTombstones tags) <$> decodeIcicleColumn Normal x

      [Zebra.Variant "left" x, Zebra.Variant "right" y] ->
        Icicle.Sum (decodeBoolTags tags) <$> decodeIcicleColumn option x <*> decodeIcicleColumn option y

      _ ->
        Left . ZebraUnknownEnum . Cons.toList $ fmap (fmap Zebra.schemaColumn) variants

  Zebra.Struct _ fields ->
    if Cons.length fields /= 2 then
      Icicle.Struct <$> traverse (decodeField option) fields
    else
      case Cons.toList fields of
        [Zebra.Field "first" x, Zebra.Field "second" y] ->
          Icicle.Pair <$> decodeIcicleColumn option x <*> decodeIcicleColumn option y
        _ ->
          Icicle.Struct <$> traverse (decodeField option) fields

  Zebra.Nested ns table ->
    decodeIcicleNested option ns table

  Zebra.Reversed x ->
    Left $ ZebraUnexpectedReversed (Zebra.schemaColumn x)

decodeInputTime :: Zebra.Column -> Either ZebraError (Storable.Vector Time64)
decodeInputTime column = do
  (_, fields) <- first ZebraSchemaError $ Zebra.takeStruct column
  case Cons.toList fields of
    [Zebra.Field "time" time0, Zebra.Field "factset_id" (Zebra.Reversed (Zebra.Int _ _ _))] -> do
      (_, encoding, time) <- first ZebraSchemaError $ Zebra.takeInt time0
      decodeIntTime encoding time

    _ ->
      Left . ZebraExpectedTimeKey . Cons.toList $ fmap (fmap Zebra.schemaColumn) fields

replicates :: Generic.Vector v a => Storable.Vector Int64 -> v a -> v a
replicates ns xs =
  -- FIXME don't use Boxed.Vector
  Generic.convert .
  Boxed.concatMap (\(ix, x) -> Boxed.replicate (fromIntegral ix) x) $
  Boxed.zip (Boxed.convert ns) (Boxed.convert xs)

decodeInputColumn :: Zebra.Column -> Either ZebraError InputColumn
decodeInputColumn column = do
  (ns0, nested0) <- first ZebraSchemaError $ Zebra.takeNested column
  (_, time0, nested1) <- first ZebraSchemaError $ Zebra.takeMap nested0
  time1 <- decodeInputTime time0

  (ns1, array) <- first ZebraSchemaError $ Zebra.takeNested nested1
  (_, value0) <- first ZebraSchemaError $ Zebra.takeArray array
  value <- decodeIcicleColumn TopLevel value0

  let
    time =
      replicates ns1 time1

  ns <-
    first ZebraTimeKeySegmentDescriptorMismatch .
      fmap (Storable.convert . fmap Storable.sum) $ Segment.reify ns0 ns1

  pure $
    InputColumn {
        inputLength =
          ns
      , inputTime =
          time
      , inputColumn =
          value
      }

decodeNamedInputColumn :: Zebra.Field Zebra.Column -> Either ZebraError (InputId, InputColumn)
decodeNamedInputColumn (Zebra.Field (Zebra.FieldName name) column) = do
  iid <- maybeToRight (ZebraInvalidInputId name) $ parseInputId name
  icolumn <- decodeInputColumn column
  pure (iid, icolumn)

decodeInputColumns :: Zebra.Column -> Either ZebraError (Map InputId InputColumn)
decodeInputColumns column = do
  (_, inputs) <- first ZebraSchemaError $ Zebra.takeStruct column
  fmap Map.fromList . traverse decodeNamedInputColumn $ Cons.toList inputs

decodeEntityHash :: Zebra.Column -> Either ZebraError (Boxed.Vector EntityHash)
decodeEntityHash column = do
  (_, _, xs) <- first ZebraSchemaError $ Zebra.takeInt column
  pure $
    fmap (EntityHash . fromIntegral) (Boxed.convert xs)

decodeUtf8 :: Zebra.Column -> Either ZebraError (Boxed.Vector ByteString)
decodeUtf8 nested = do
  (ns, bytes0) <- first ZebraSchemaError $ Zebra.takeNested nested
  (_, _, bytes) <- first ZebraSchemaError $ Zebra.takeBinary bytes0

  first ZebraEntityIdSegmentDescriptorMismatch $
    Segment.reify ns bytes

decodeEntityId :: Zebra.Column -> Either ZebraError (Boxed.Vector EntityId)
decodeEntityId nested =
  fmap EntityId <$> decodeUtf8 nested

decodeEntityKeyFields :: [Zebra.Field Zebra.Column] -> Either ZebraError (Boxed.Vector EntityKey)
decodeEntityKeyFields fields = do
  case fields of
    [Zebra.Field "entity_hash" ehash, Zebra.Field "entity_id" eid] ->
      Boxed.zipWith EntityKey
        <$> decodeEntityHash ehash
        <*> decodeEntityId eid
    _ ->
      Left $
        ZebraExpectedEntityKey (fmap (fmap Zebra.schemaColumn) fields)

decodeEntityKey :: Zebra.Column -> Either ZebraError (Boxed.Vector EntityKey)
decodeEntityKey column = do
  (_, fields) <- first ZebraSchemaError $ Zebra.takeStruct column
  decodeEntityKeyFields $ Cons.toList fields

decodeInput :: Zebra.Table -> Either ZebraError Input
decodeInput table = do
  (_, kcolumn, vcolumn) <- first ZebraSchemaError $ Zebra.takeMap table
  keys <- decodeEntityKey kcolumn
  columns <- decodeInputColumns vcolumn
  pure $
    Input keys columns

decodeNamedOutputColumn :: Zebra.Field Zebra.Column -> Either ZebraError (OutputId, Icicle.Column)
decodeNamedOutputColumn (Zebra.Field (Zebra.FieldName name) column) = do
  oid <- maybeToRight (ZebraInvalidInputId name) $ parseOutputId name
  ocolumn <- decodeIcicleColumn Normal column
  pure (oid, ocolumn)

decodeOutputColumns :: Zebra.Column -> Either ZebraError (Map OutputId Icicle.Column)
decodeOutputColumns column = do
  (_, outputs) <- first ZebraSchemaError $ Zebra.takeStruct column
  fmap Map.fromList . traverse decodeNamedOutputColumn $ Cons.toList outputs

decodeSnapshotKey :: Zebra.Column -> Either ZebraError (Boxed.Vector SnapshotKey)
decodeSnapshotKey =
  fmap (fmap SnapshotKey) . decodeEntityKey

decodeSnapshotOutput :: Zebra.Table -> Either ZebraError (Output SnapshotKey)
decodeSnapshotOutput table = do
  (_, kcolumn, vcolumn) <- first ZebraSchemaError $ Zebra.takeMap table
  keys <- decodeSnapshotKey kcolumn
  columns <- decodeOutputColumns vcolumn
  pure $
    Output keys columns

decodeChordKey :: Zebra.Column -> Either ZebraError (Boxed.Vector ChordKey)
decodeChordKey column = do
  (_, fields) <- first ZebraSchemaError $ Zebra.takeStruct column
  case Cons.toList fields of
    [ehash, eid, Zebra.Field "label" label] ->
      Boxed.zipWith ChordKey
        <$> decodeEntityKeyFields [ehash, eid]
        <*> decodeUtf8 label
    xs ->
      Left . ZebraExpectedChordKey $ fmap (fmap Zebra.schemaColumn) xs

decodeChordOutput :: Zebra.Table -> Either ZebraError (Output ChordKey)
decodeChordOutput table = do
  (_, kcolumn, vcolumn) <- first ZebraSchemaError $ Zebra.takeMap table
  keys <- decodeChordKey kcolumn
  columns <- decodeOutputColumns vcolumn
  pure $
    Output keys columns

------------------------------------------------------------------------
-- Input: Icicle -> Zebra

encodeTime :: Time64 -> Either ZebraError Zebra.Time
encodeTime x0 =
  let
    !x =
      -- FIXME icicle should use the same epoch as zebra so we don't need to do this for defaults
      if x0 == Time64 0 then
        unpackTime zebraEpoch
      else
        unpackTime x0

    !date =
      Zebra.CalendarDate
        (fromIntegral $ timeYear x)
        (fromIntegral $ timeMonth x)
        (fromIntegral $ timeDay x)

    !microseconds =
      fromIntegral (timeSeconds x) * 1000000

    !time =
      Zebra.toTimeOfDay microseconds
  in
    first ZebraTimeError . Zebra.fromCalendarTime $
      Zebra.CalendarTime date time

encodeIntTime :: Storable.Vector Time64 -> Either ZebraError Zebra.Column
encodeIntTime xs =
  Zebra.Int Zebra.DenyDefault ZEncoding.TimeSeconds
    <$> Storable.mapM (fmap ZEncoding.encodeTimeSeconds . encodeTime) xs

encodeBoolTags :: Storable.Vector Bool64 -> Storable.Vector Zebra.Tag
encodeBoolTags =
  Storable.map (fromIntegral . unBool64)

encodeTombstones :: Storable.Vector Error64 -> Storable.Vector Zebra.Tag
encodeTombstones =
  Storable.map $ \case
    Tombstone64 ->
      0
    NotAnError64 ->
      1
    _ ->
      0

encodeField :: Field Icicle.Column -> Either ZebraError (Zebra.Field Zebra.Column)
encodeField (Field name column) =
  Zebra.Field (Zebra.FieldName name) <$> encodeIcicleColumn column

encodeIcicleColumn :: Icicle.Column -> Either ZebraError Zebra.Column
encodeIcicleColumn = \case
  Icicle.Unit n ->
    pure $
      Zebra.Unit n

  Icicle.Bool xs ->
    pure $
      Zebra.Enum Zebra.DenyDefault
        (encodeBoolTags xs)
        (Cons.from2
          (Zebra.Variant "false" . Zebra.Unit $ Storable.length xs)
          (Zebra.Variant "true" . Zebra.Unit $ Storable.length xs))

  Icicle.Int xs ->
    pure $
      Zebra.Int Zebra.DenyDefault ZEncoding.Int xs

  Icicle.Double xs ->
    pure $
      Zebra.Double Zebra.DenyDefault xs

  Icicle.Time xs ->
    encodeIntTime xs

  Icicle.Sum tags x y -> do
    fmap (Zebra.Enum Zebra.DenyDefault $ encodeBoolTags tags) $
      Cons.from2
        <$> (Zebra.Variant "left" <$> encodeIcicleColumn x)
        <*> (Zebra.Variant "right" <$> encodeIcicleColumn y)

  Icicle.Option tags x ->
    fmap (Zebra.Enum Zebra.DenyDefault $ encodeBoolTags tags) $
      Cons.from2
        <$> pure (Zebra.Variant "none" . Zebra.Unit $ Storable.length tags)
        <*> (Zebra.Variant "some" <$> encodeIcicleColumn x)

  Icicle.Result tags x ->
    fmap (Zebra.Enum Zebra.DenyDefault $ encodeTombstones tags) $
      Cons.from2
        <$> pure (Zebra.Variant "none" . Zebra.Unit $ Storable.length tags)
        <*> (Zebra.Variant "some" <$> encodeIcicleColumn x)

  Icicle.Pair x y ->
    fmap (Zebra.Struct Zebra.DenyDefault) $
      Cons.from2
        <$> (Zebra.Field "first" <$> encodeIcicleColumn x)
        <*> (Zebra.Field "second" <$> encodeIcicleColumn y)

  Icicle.Struct fields ->
    Zebra.Struct Zebra.DenyDefault
      <$> traverse encodeField fields

  Icicle.String ns bss ->
    pure $
      Zebra.Nested ns (Zebra.Binary Zebra.DenyDefault ZEncoding.Utf8 bss)

  Icicle.Array ns x ->
    fmap (Zebra.Nested ns) $
      Zebra.Array Zebra.DenyDefault
        <$> encodeIcicleColumn x

  Icicle.Map ns k v ->
    fmap (Zebra.Nested ns) $
      Zebra.Map Zebra.DenyDefault
        <$> encodeIcicleColumn k
        <*> encodeIcicleColumn v

encodeFactsetId :: Int -> Zebra.Column
encodeFactsetId n =
  Zebra.Reversed . Zebra.Int Zebra.DenyDefault ZEncoding.Int $
    Storable.replicate n 0

encodeInputTime :: Storable.Vector Time64 -> Either ZebraError Zebra.Column
encodeInputTime times =
  fmap (Zebra.Struct Zebra.DenyDefault) $
    Cons.from2
      <$> (Zebra.Field "time" <$> encodeIntTime times)
      <*> pure (Zebra.Field "factset_id" . encodeFactsetId $ Storable.length times)

encodeInputColumn :: InputColumn -> Either ZebraError Zebra.Column
encodeInputColumn input =
  let
    -- FIXME don't use Boxed.Vector
    (key_counts0, (value_counts0, time0)) =
      second Boxed.unzip $
        Generic.segmentedGroup
          (fmap fromIntegral . Boxed.convert $ inputLength input)
          (Boxed.convert $ inputTime input)

    key_counts =
      Storable.map fromIntegral $ Storable.convert key_counts0

    value_counts =
      Storable.map fromIntegral $ Storable.convert value_counts0

    time =
      Storable.convert time0

    nested =
      Zebra.Nested value_counts .
      Zebra.Array Zebra.DenyDefault
  in
    fmap (Zebra.Nested key_counts) $
      Zebra.Map Zebra.AllowDefault
        <$> encodeInputTime time
        <*> (nested <$> encodeIcicleColumn (inputColumn input))

encodeInputColumns :: Map InputId InputColumn -> Either ZebraError Zebra.Column
encodeInputColumns kvs0 = do
  kvs <- maybeToRight ZebraNoInputs . Cons.fromList $ Map.toList kvs0
  fmap (Zebra.Struct Zebra.DenyDefault) . for kvs $ \(k, v) ->
    Zebra.Field
      <$> pure (Zebra.FieldName $ renderInputId k)
      <*> encodeInputColumn v

encodeEntityHash :: Boxed.Vector EntityHash -> Zebra.Column
encodeEntityHash xs = do
  Zebra.Int Zebra.DenyDefault ZEncoding.Int . Storable.convert $
    fmap (fromIntegral . unEntityHash) xs

encodeUtf8 :: Boxed.Vector ByteString -> Zebra.Column
encodeUtf8 xs =
  let
    ns =
      Storable.convert $
        fmap (fromIntegral . ByteString.length) xs

    bss =
      ByteString.concat $ Boxed.toList xs
  in
    Zebra.Nested ns (Zebra.Binary Zebra.DenyDefault ZEncoding.Utf8 bss)

encodeEntityId :: Boxed.Vector EntityId -> Zebra.Column
encodeEntityId =
  encodeUtf8 . fmap unEntityId

encodeEntityKeyFields :: Boxed.Vector EntityKey -> Cons Boxed.Vector (Zebra.Field Zebra.Column)
encodeEntityKeyFields keys =
  Cons.from2
    (Zebra.Field "entity_hash" . encodeEntityHash $ fmap entityHash keys)
    (Zebra.Field "entity_id" . encodeEntityId $ fmap entityId keys)

encodeEntityKey :: Boxed.Vector EntityKey -> Zebra.Column
encodeEntityKey =
  Zebra.Struct Zebra.DenyDefault . encodeEntityKeyFields

encodeInput :: Input -> Either ZebraError Zebra.Table
encodeInput input =
  Zebra.Map Zebra.DenyDefault
    <$> pure (encodeEntityKey $ inputEntity input)
    <*> encodeInputColumns (inputColumns input)

encodeOutputColumns :: Map OutputId Icicle.Column -> Either ZebraError Zebra.Column
encodeOutputColumns kvs0 = do
  kvs <- maybeToRight ZebraNoOutputs . Cons.fromList $ Map.toList kvs0
  fmap (Zebra.Struct Zebra.DenyDefault) . for kvs $ \(k, v) ->
    Zebra.Field
      <$> pure (Zebra.FieldName $ renderOutputId k)
      <*> encodeIcicleColumn v

encodeSnapshotKey :: Boxed.Vector SnapshotKey -> Zebra.Column
encodeSnapshotKey =
  encodeEntityKey . fmap snapshotEntity

encodeSnapshotOutput :: Output SnapshotKey -> Either ZebraError Zebra.Table
encodeSnapshotOutput output =
  Zebra.Map Zebra.DenyDefault
    <$> pure (encodeSnapshotKey $ outputEntity output)
    <*> encodeOutputColumns (outputColumns output)

encodeChordKey :: Boxed.Vector ChordKey -> Zebra.Column
encodeChordKey keys =
  let
    entity =
      encodeEntityKeyFields (fmap chordEntity keys)

    label =
      Zebra.Field "label" (encodeUtf8 $ fmap chordLabel keys)
  in
    Zebra.Struct Zebra.DenyDefault $
      entity `snoc` label

encodeChordOutput :: Output ChordKey -> Either ZebraError Zebra.Table
encodeChordOutput output =
  Zebra.Map Zebra.DenyDefault
    <$> pure (encodeChordKey $ outputEntity output)
    <*> encodeOutputColumns (outputColumns output)

------------------------------------------------------------------------
-- Schema: Zebra -> Icicle

resolveField :: Zebra.Field ZSchema.Column -> Either ZebraError (Field Schema)
resolveField (Zebra.Field (Zebra.FieldName name) column) =
  Field name <$> resolveIcicleColumn column

resolveIcicleNested :: ZSchema.Table -> Either ZebraError Schema
resolveIcicleNested = \case
  ZSchema.Binary _ encoding@ZEncoding.Binary ->
    Left $ ZebraUnexpectedBinaryEncoding encoding
  ZSchema.Binary _ ZEncoding.Utf8 ->
    pure Schema.String

  ZSchema.Array _ column ->
    Schema.Array <$> resolveIcicleColumn column

  ZSchema.Map _ kcolumn vcolumn ->
    Schema.Map <$> resolveIcicleColumn kcolumn <*> resolveIcicleColumn vcolumn

resolveIcicleColumn :: ZSchema.Column -> Either ZebraError Schema
resolveIcicleColumn = \case
  ZSchema.Unit ->
    pure Schema.Unit

  ZSchema.Int _ ZEncoding.Int ->
    pure Schema.Int
  ZSchema.Int _ ZEncoding.Date ->
    pure Schema.Time
  ZSchema.Int _ ZEncoding.TimeSeconds ->
    pure Schema.Time
  ZSchema.Int _ ZEncoding.TimeMilliseconds ->
    pure Schema.Time
  ZSchema.Int _ ZEncoding.TimeMicroseconds ->
    pure Schema.Time

  ZSchema.Double _ ->
    pure Schema.Double

  ZSchema.Enum _ variants ->
    case Cons.toList variants of
      [Zebra.Variant "false" ZSchema.Unit, Zebra.Variant "true" ZSchema.Unit] ->
        pure Schema.Bool

      [Zebra.Variant "none" ZSchema.Unit, Zebra.Variant "some" x] ->
         Schema.Option <$> resolveIcicleColumn x

      [Zebra.Variant "left" x, Zebra.Variant "right" y] ->
        Schema.Sum <$> resolveIcicleColumn x <*> resolveIcicleColumn y

      _ ->
        Left . ZebraUnknownEnum $ Cons.toList variants

  ZSchema.Struct _ fields ->
    if Cons.length fields /= 2 then
      Schema.Struct <$> traverse resolveField fields
    else
      case Cons.toList fields of
        [Zebra.Field "first" x, Zebra.Field "second" y] ->
          Schema.Pair <$> resolveIcicleColumn x <*> resolveIcicleColumn y
        _ ->
          Schema.Struct <$> traverse resolveField fields

  ZSchema.Nested table ->
    resolveIcicleNested table

  ZSchema.Reversed x ->
    Left $ ZebraUnexpectedReversed x

resolveInputColumn :: ZSchema.Column -> Either ZebraError Schema
resolveInputColumn column = do
  map <- first ZebraSchemaError $ ZSchema.takeNested column
  (_, _, nested) <- first ZebraSchemaError $ ZSchema.takeMap map
  array <- first ZebraSchemaError $ ZSchema.takeNested nested
  (_, option) <- first ZebraSchemaError $ ZSchema.takeArray array
  (_, value) <- first ZebraSchemaError $ ZSchema.takeOption option
  resolveIcicleColumn value

resolveNamedInputColumn :: Zebra.Field ZSchema.Column -> Either ZebraError (InputId, Schema)
resolveNamedInputColumn (Zebra.Field (Zebra.FieldName name) column) = do
  iid <- maybeToRight (ZebraInvalidInputId name) $ parseInputId name
  icolumn <- resolveInputColumn column
  pure (iid, icolumn)

resolveInputColumns :: ZSchema.Column -> Either ZebraError (Map InputId Schema)
resolveInputColumns column = do
  (_, inputs) <- first ZebraSchemaError $ ZSchema.takeStruct column
  fmap Map.fromList . traverse resolveNamedInputColumn $ Cons.toList inputs

mkDictionaryInput :: InputId -> Schema -> Either ZebraError DictionaryInput
mkDictionaryInput iid schema = do
  encoding <- first ZebraResolveDictionaryError $ Schema.toEncoding schema
  pure $
    DictionaryInput iid encoding Set.empty unkeyed

resolveDictionary :: ZSchema.Table -> Either ZebraError Dictionary
resolveDictionary table = do
  (_, _, vcolumn) <- first ZebraSchemaError $ ZSchema.takeMap table
  columns <- resolveInputColumns vcolumn
  inputs <- sequence $ Map.mapWithKey mkDictionaryInput columns
  pure $
    Dictionary inputs Map.empty []
