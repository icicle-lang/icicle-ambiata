{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Runtime.Serial.Zebra.Striped (
    ZebraStripedError(..)
  , renderZebraStripedError

  , decodeInput
  , encodeInput

  , decodeSnapshotOutput
  , encodeSnapshotOutput

  , decodeChordOutput
  , encodeChordOutput

  , decodeColumn
  , encodeColumn

  , shiftTable
  , shiftColumn
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Boxed
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Storable as Storable

import           Icicle.Data.Name
import           Icicle.Runtime.Data.IO
import           Icicle.Runtime.Data.Primitive
import qualified Icicle.Runtime.Data.Striped as Icicle

import           P

import           X.Data.Vector.Cons (Cons)
import qualified X.Data.Vector.Cons as Cons

import qualified Zebra.Table.Data as Zebra
import qualified Zebra.Table.Encoding as Encoding
import qualified Zebra.Table.Schema as Schema
import qualified Zebra.Table.Striped as Zebra
import           Zebra.Time (TimeError)
import qualified Zebra.Time as Zebra
import qualified Zebra.X.Vector.Generic as Generic -- FIXME move to x-vector
import           Zebra.X.Vector.Segment (SegmentError)
import qualified Zebra.X.Vector.Segment as Segment -- FIXME move to x-vector


data ZebraStripedError =
    ZebraStripedSchemaError !Schema.SchemaError
  | ZebraStripedTimeError !TimeError
  | ZebraStripedUnexpectedBinaryEncoding !Encoding.Binary
  | ZebraStripedUnexpectedIntEncoding !Encoding.Int
  | ZebraStripedUnexpectedReversed !Schema.Column
  | ZebraStripedEntityIdSegmentDescriptorMismatch !SegmentError
  | ZebraStripedTimeKeySegmentDescriptorMismatch !SegmentError
  | ZebraStripedExpectedEntityKey ![Zebra.Field Schema.Column]
  | ZebraStripedExpectedChordKey ![Zebra.Field Schema.Column]
  | ZebraStripedExpectedTimeKey ![Zebra.Field Schema.Column]
  | ZebraStripedUnknownEnum ![Zebra.Variant Schema.Column]
  | ZebraStripedUnknownOptionTag !Zebra.Tag
  | ZebraStripedUnknownResultTag !Zebra.Tag
  | ZebraStripedUnknownError64 !Error64
  | ZebraStripedInvalidInputId !Text
  | ZebraStripedNoInputs
  | ZebraStripedNoOutputs
    deriving (Eq, Show)

renderZebraStripedError :: ZebraStripedError -> Text
renderZebraStripedError = \case
  ZebraStripedSchemaError x ->
    Schema.renderSchemaError x
  ZebraStripedTimeError x ->
    Zebra.renderTimeError x
  ZebraStripedUnexpectedBinaryEncoding x ->
    "Unexpected binary encoding: " <> Text.pack (show x)
  ZebraStripedUnexpectedIntEncoding x ->
    "Unexpected int encoding: " <> Text.pack (show x)
  ZebraStripedUnexpectedReversed x ->
    "Unexpected reversed column: " <> Text.pack (show x)
  ZebraStripedEntityIdSegmentDescriptorMismatch x ->
    "Segment descriptor mismatch when decoding entity-id: " <> Segment.renderSegmentError x
  ZebraStripedTimeKeySegmentDescriptorMismatch x ->
    "Segment descriptor mismatch when decoding time key: " <> Segment.renderSegmentError x
  ZebraStripedExpectedEntityKey xs ->
    "Expected entity key columns, but found: " <> Text.pack (show xs)
  ZebraStripedExpectedChordKey xs ->
    "Expected chord key columns, but found: " <> Text.pack (show xs)
  ZebraStripedExpectedTimeKey xs ->
    "Expected time key columns, but found: " <> Text.pack (show xs)
  ZebraStripedUnknownEnum xs ->
    "Could not convert unknown enum: " <> Text.pack (show xs)
  ZebraStripedUnknownOptionTag x ->
    "Found unknown option tag: " <> Text.pack (show x)
  ZebraStripedUnknownResultTag x ->
    "Found unknown result tag: " <> Text.pack (show x)
  ZebraStripedUnknownError64 x ->
    "Found unknown error tag: " <> Text.pack (show x)
  ZebraStripedInvalidInputId x ->
    "Found invalid input-id: " <> x
  ZebraStripedNoInputs ->
    "Could not encode data in zebra as it had no inputs."
  ZebraStripedNoOutputs ->
    "Could not encode data in zebra as it had no outputs."

------------------------------------------------------------------------

-- FIXME x-vector
snoc :: Cons Boxed.Vector a -> a -> Cons Boxed.Vector a
snoc xs0 x =
  let
    (hd, tl) =
      Cons.uncons xs0
  in
    Cons.from hd (tl <> Boxed.singleton x)

-- FIXME x-vector
from4 :: a -> a -> a -> a -> Cons Boxed.Vector a
from4 x y z w =
  Cons.unsafeFromList [x, y, z, w]

-- FIXME zebra-core
takeOption :: Zebra.Column -> Either Schema.SchemaError (Zebra.Default, Storable.Vector Zebra.Tag, Zebra.Column)
takeOption x0 = do
  (def, tags, vs) <- Zebra.takeEnum x0
  case Cons.toList vs of
    [Zebra.Variant "none" (Zebra.Unit _), Zebra.Variant "some" x] ->
      pure (def, tags, x)
    _ ->
      Left . Schema.SchemaExpectedOption $ fmap (fmap Zebra.schemaColumn) vs

------------------------------------------------------------------------
-- Temporal Shift: Zebra -> Zebra
--
-- Translate all date and time encodings to time in seconds.
--
--                            Emergency temporal shift!
--                            /
--                       ___
--               D>=G==='   '.
--                     |======|
--                     |======|
--                 )--/]IIIIII]
--                    |_______|
--                    C O O O D
--                   C O  O  O D
--                  C  O  O  O  D
--                  C__O__O__O__D
--                 [_____________]
--

shiftTable :: Zebra.Table -> Zebra.Table
shiftTable = \case
  Zebra.Binary def encoding bs ->
    Zebra.Binary def encoding bs

  Zebra.Array def x ->
    Zebra.Array def (shiftColumn x)

  Zebra.Map def k v ->
    Zebra.Map def (shiftColumn k) (shiftColumn v)

shiftIntEncoding :: Encoding.Int -> Encoding.Int
shiftIntEncoding = \case
  Encoding.Int ->
    Encoding.Int

  Encoding.Date ->
    Encoding.TimeSeconds

  Encoding.TimeSeconds ->
    Encoding.TimeSeconds

  Encoding.TimeMilliseconds ->
    Encoding.TimeMilliseconds

  Encoding.TimeMicroseconds ->
    Encoding.TimeMicroseconds

shiftInt :: Encoding.Int -> Storable.Vector Int64 -> Storable.Vector Int64
shiftInt = \case
  Encoding.Int ->
    id

  Encoding.Date ->
    Storable.map (* 86400)

  Encoding.TimeSeconds ->
    id

  Encoding.TimeMilliseconds ->
    Storable.map (`quot` 1000)

  Encoding.TimeMicroseconds ->
    Storable.map (`quot` 1000000)

shiftColumn :: Zebra.Column -> Zebra.Column
shiftColumn = \case
  Zebra.Unit n ->
    Zebra.Unit n

  Zebra.Int def encoding xs ->
    Zebra.Int def (shiftIntEncoding encoding) (shiftInt encoding xs)

  Zebra.Double def xs ->
    Zebra.Double def xs

  Zebra.Enum def tags xs ->
    Zebra.Enum def tags (fmap (fmap shiftColumn) xs)

  Zebra.Struct def xs ->
    Zebra.Struct def (fmap (fmap shiftColumn) xs)

  Zebra.Nested ns x ->
    Zebra.Nested ns (shiftTable x)

  Zebra.Reversed x ->
    Zebra.Reversed (shiftColumn x)

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

newTimeDecoder :: Encoding.Int -> Either ZebraStripedError (Int64 -> Either ZebraStripedError Time64)
newTimeDecoder = \case
  Encoding.Date ->
    pure $
      bimap ZebraStripedTimeError decodeDate . Encoding.decodeDate
  Encoding.TimeSeconds ->
    pure $
      bimap ZebraStripedTimeError decodeTime . Encoding.decodeTimeSeconds
  Encoding.TimeMilliseconds ->
    pure $
      bimap ZebraStripedTimeError decodeTime . Encoding.decodeTimeMilliseconds
  Encoding.TimeMicroseconds ->
    pure $
      bimap ZebraStripedTimeError decodeTime . Encoding.decodeTimeMicroseconds
  encoding ->
    Left $ ZebraStripedUnexpectedIntEncoding encoding

decodeIntTime :: Encoding.Int -> Storable.Vector Int64 -> Either ZebraStripedError (Storable.Vector Time64)
decodeIntTime encoding xs = do
  decoder <- newTimeDecoder encoding
  Storable.mapM decoder xs

decodeBoolTags :: Storable.Vector Zebra.Tag -> Storable.Vector Bool64
decodeBoolTags =
  Storable.map (Bool64 . fromIntegral)

decodeResultTags :: Storable.Vector Zebra.Tag -> Either ZebraStripedError (Storable.Vector Error64)
decodeResultTags =
  Storable.mapM $ \case
    0 ->
      pure NotAnError64
    1 ->
      pure Tombstone64
    2 ->
      pure Fold1NoValue64
    3 ->
      pure CannotCompute64
    n ->
      Left $ ZebraStripedUnknownResultTag n

decodeField :: Zebra.Field Zebra.Column -> Either ZebraStripedError (Field Icicle.Column)
decodeField (Zebra.Field (Zebra.FieldName name) column) =
  Field name <$> decodeColumn column

decodeTable :: Storable.Vector Int64 -> Zebra.Table -> Either ZebraStripedError Icicle.Column
decodeTable ns = \case
  Zebra.Binary _ encoding@Encoding.Binary _ ->
    Left $ ZebraStripedUnexpectedBinaryEncoding encoding
  Zebra.Binary _ Encoding.Utf8 bs ->
    pure $ Icicle.String ns bs

  Zebra.Array _ column ->
    Icicle.Array ns <$> decodeColumn column

  Zebra.Map _ kcolumn vcolumn ->
    Icicle.Map ns <$> decodeColumn kcolumn <*> decodeColumn vcolumn

decodeColumn :: Zebra.Column -> Either ZebraStripedError Icicle.Column
decodeColumn = \case
  Zebra.Unit n ->
    pure $ Icicle.Unit n

  Zebra.Int _ Encoding.Int xs ->
    pure $ Icicle.Int xs
  Zebra.Int _ encoding@Encoding.Date xs ->
    Icicle.Time <$> decodeIntTime encoding xs
  Zebra.Int _ encoding@Encoding.TimeSeconds xs ->
    Icicle.Time <$> decodeIntTime encoding xs
  Zebra.Int _ encoding@Encoding.TimeMilliseconds xs ->
    Icicle.Time <$> decodeIntTime encoding xs
  Zebra.Int _ encoding@Encoding.TimeMicroseconds xs ->
    Icicle.Time <$> decodeIntTime encoding xs

  Zebra.Double _ xs ->
    pure $ Icicle.Double xs

  Zebra.Enum _ tags variants ->
    case Cons.toList variants of
      [Zebra.Variant "false" (Zebra.Unit _), Zebra.Variant "true" (Zebra.Unit _)] ->
        pure $ Icicle.Bool (decodeBoolTags tags)

      [Zebra.Variant "none" (Zebra.Unit _), Zebra.Variant "some" x] ->
        Icicle.Option (decodeBoolTags tags) <$> decodeColumn x

      [Zebra.Variant "left" x, Zebra.Variant "right" y] ->
        Icicle.Sum (decodeBoolTags tags) <$> decodeColumn x <*> decodeColumn y

      [Zebra.Variant "success" x,
       Zebra.Variant "tombstone" (Zebra.Unit _),
       Zebra.Variant "fold1_no_value" (Zebra.Unit _),
       Zebra.Variant "cannot_compute" (Zebra.Unit _)] ->
        Icicle.Result <$> decodeResultTags tags <*> decodeColumn x

      _ ->
        Left . ZebraStripedUnknownEnum . Cons.toList $ fmap (fmap Zebra.schemaColumn) variants

  Zebra.Struct _ fields ->
    if Cons.length fields /= 2 then
      Icicle.Struct <$> traverse decodeField fields
    else
      case Cons.toList fields of
        [Zebra.Field "first" x, Zebra.Field "second" y] ->
          Icicle.Pair <$> decodeColumn x <*> decodeColumn y
        _ ->
          Icicle.Struct <$> traverse decodeField fields

  Zebra.Nested ns table ->
    decodeTable ns table

  Zebra.Reversed x ->
    Left $ ZebraStripedUnexpectedReversed (Zebra.schemaColumn x)

decodeInputTime :: Zebra.Column -> Either ZebraStripedError (Storable.Vector InputTime)
decodeInputTime column = do
  (_, fields) <- first ZebraStripedSchemaError $ Zebra.takeStruct column
  case Cons.toList fields of
    [Zebra.Field "time" time0, Zebra.Field "factset_id" (Zebra.Reversed (Zebra.Int _ _ _))] -> do
      (_, encoding, time) <- first ZebraStripedSchemaError $ Zebra.takeInt time0
      Storable.map InputTime <$> decodeIntTime encoding time

    _ ->
      Left . ZebraStripedExpectedTimeKey . Cons.toList $ fmap (fmap Zebra.schemaColumn) fields

replicates :: Generic.Vector v a => Storable.Vector Int64 -> v a -> v a
replicates ns xs =
  -- FIXME don't use Boxed.Vector
  Generic.convert .
  Boxed.concatMap (\(ix, x) -> Boxed.replicate (fromIntegral ix) x) $
  Boxed.zip (Boxed.convert ns) (Boxed.convert xs)

decodeTombstoneTags :: Storable.Vector Zebra.Tag -> Either ZebraStripedError (Storable.Vector Error64)
decodeTombstoneTags =
  Storable.mapM $ \case
    0 ->
      pure Tombstone64
    1 ->
      pure NotAnError64
    n ->
      Left $ ZebraStripedUnknownOptionTag n

decodeInputColumn :: Zebra.Column -> Either ZebraStripedError InputColumn
decodeInputColumn column = do
  (ns0, nested0) <- first ZebraStripedSchemaError $ Zebra.takeNested column
  (_, time0, nested1) <- first ZebraStripedSchemaError $ Zebra.takeMap nested0
  time1 <- decodeInputTime time0

  (ns1, array) <- first ZebraStripedSchemaError $ Zebra.takeNested nested1
  (_, value0) <- first ZebraStripedSchemaError $ Zebra.takeArray array
  (_, tags, value1) <- first ZebraStripedSchemaError $ takeOption value0
  value <- decodeColumn value1

  let
    time =
      replicates ns1 time1

  tombstone <- decodeTombstoneTags tags

  ns <-
    first ZebraStripedTimeKeySegmentDescriptorMismatch .
      fmap (Storable.convert . fmap Storable.sum) $ Segment.reify ns0 ns1

  pure $
    InputColumn {
        inputLength =
          ns
      , inputTime =
          time
      , inputTombstone =
          tombstone
      , inputColumn =
          value
      }

decodeNamedInputColumn :: Zebra.Field Zebra.Column -> Either ZebraStripedError (InputId, InputColumn)
decodeNamedInputColumn (Zebra.Field (Zebra.FieldName name) column) = do
  iid <- maybeToRight (ZebraStripedInvalidInputId name) $ parseInputId name
  icolumn <- decodeInputColumn column
  pure (iid, icolumn)

decodeInputColumns :: Zebra.Column -> Either ZebraStripedError (Map InputId InputColumn)
decodeInputColumns column = do
  (_, inputs) <- first ZebraStripedSchemaError $ Zebra.takeStruct column
  fmap Map.fromList . traverse decodeNamedInputColumn $ Cons.toList inputs

decodeEntityHash :: Zebra.Column -> Either ZebraStripedError (Boxed.Vector EntityHash)
decodeEntityHash column = do
  (_, _, xs) <- first ZebraStripedSchemaError $ Zebra.takeInt column
  pure $
    fmap (EntityHash . fromIntegral) (Boxed.convert xs)

decodeUtf8 :: Zebra.Column -> Either ZebraStripedError (Boxed.Vector ByteString)
decodeUtf8 nested = do
  (ns, bytes0) <- first ZebraStripedSchemaError $ Zebra.takeNested nested
  (_, _, bytes) <- first ZebraStripedSchemaError $ Zebra.takeBinary bytes0

  first ZebraStripedEntityIdSegmentDescriptorMismatch $
    Segment.reify ns bytes

decodeEntityId :: Zebra.Column -> Either ZebraStripedError (Boxed.Vector EntityId)
decodeEntityId nested =
  fmap EntityId <$> decodeUtf8 nested

decodeEntityKeyFields :: [Zebra.Field Zebra.Column] -> Either ZebraStripedError (Boxed.Vector EntityKey)
decodeEntityKeyFields fields = do
  case fields of
    [Zebra.Field "entity_hash" ehash, Zebra.Field "entity_id" eid] ->
      Boxed.zipWith EntityKey
        <$> decodeEntityHash ehash
        <*> decodeEntityId eid
    _ ->
      Left $
        ZebraStripedExpectedEntityKey (fmap (fmap Zebra.schemaColumn) fields)

decodeEntityKey :: Zebra.Column -> Either ZebraStripedError (Boxed.Vector EntityKey)
decodeEntityKey column = do
  (_, fields) <- first ZebraStripedSchemaError $ Zebra.takeStruct column
  decodeEntityKeyFields $ Cons.toList fields

decodeInput :: Zebra.Table -> Either ZebraStripedError Input
decodeInput table = do
  (_, kcolumn, vcolumn) <- first ZebraStripedSchemaError $ Zebra.takeMap table
  keys <- decodeEntityKey kcolumn
  columns <- decodeInputColumns vcolumn
  pure $
    Input keys columns

decodeNamedOutputColumn :: Zebra.Field Zebra.Column -> Either ZebraStripedError (OutputId, Icicle.Column)
decodeNamedOutputColumn (Zebra.Field (Zebra.FieldName name) column) = do
  oid <- maybeToRight (ZebraStripedInvalidInputId name) $ parseOutputId name
  ocolumn <- decodeColumn column
  pure (oid, ocolumn)

decodeOutputColumns :: Zebra.Column -> Either ZebraStripedError (Map OutputId Icicle.Column)
decodeOutputColumns column = do
  (_, outputs) <- first ZebraStripedSchemaError $ Zebra.takeStruct column
  fmap Map.fromList . traverse decodeNamedOutputColumn $ Cons.toList outputs

decodeSnapshotKey :: Zebra.Column -> Either ZebraStripedError (Boxed.Vector SnapshotKey)
decodeSnapshotKey =
  fmap (fmap SnapshotKey) . decodeEntityKey

decodeSnapshotOutput :: Zebra.Table -> Either ZebraStripedError (Output SnapshotKey)
decodeSnapshotOutput table = do
  (_, kcolumn, vcolumn) <- first ZebraStripedSchemaError $ Zebra.takeMap table
  keys <- decodeSnapshotKey kcolumn
  columns <- decodeOutputColumns vcolumn
  pure $
    Output keys columns

decodeChordKey :: Zebra.Column -> Either ZebraStripedError (Boxed.Vector ChordKey)
decodeChordKey column = do
  (_, fields) <- first ZebraStripedSchemaError $ Zebra.takeStruct column
  case Cons.toList fields of
    [ehash, eid, Zebra.Field "label" label] ->
      Boxed.zipWith ChordKey
        <$> decodeEntityKeyFields [ehash, eid]
        <*> decodeUtf8 label
    xs ->
      Left . ZebraStripedExpectedChordKey $ fmap (fmap Zebra.schemaColumn) xs

decodeChordOutput :: Zebra.Table -> Either ZebraStripedError (Output ChordKey)
decodeChordOutput table = do
  (_, kcolumn, vcolumn) <- first ZebraStripedSchemaError $ Zebra.takeMap table
  keys <- decodeChordKey kcolumn
  columns <- decodeOutputColumns vcolumn
  pure $
    Output keys columns

------------------------------------------------------------------------
-- Input: Icicle -> Zebra

encodeTime :: Time64 -> Either ZebraStripedError Zebra.Time
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
    first ZebraStripedTimeError . Zebra.fromCalendarTime $
      Zebra.CalendarTime date time

encodeIntTime :: Storable.Vector Time64 -> Either ZebraStripedError Zebra.Column
encodeIntTime xs =
  Zebra.Int Zebra.DenyDefault Encoding.TimeSeconds
    <$> Storable.mapM (fmap Encoding.encodeTimeSeconds . encodeTime) xs

encodeBoolTags :: Storable.Vector Bool64 -> Storable.Vector Zebra.Tag
encodeBoolTags =
  Storable.map (fromIntegral . unBool64)

encodeErrorTags :: Storable.Vector Error64 -> Either ZebraStripedError (Storable.Vector Zebra.Tag)
encodeErrorTags =
  Storable.mapM $ \case
    NotAnError64 ->
      pure 0
    Tombstone64 ->
      pure 1
    Fold1NoValue64 ->
      pure 2
    CannotCompute64 ->
      pure 3
    n ->
      Left $ ZebraStripedUnknownError64 n

encodeField :: Field Icicle.Column -> Either ZebraStripedError (Zebra.Field Zebra.Column)
encodeField (Field name column) =
  Zebra.Field (Zebra.FieldName name) <$> encodeColumn column

encodeColumn :: Icicle.Column -> Either ZebraStripedError Zebra.Column
encodeColumn = \case
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
      Zebra.Int Zebra.DenyDefault Encoding.Int xs

  Icicle.Double xs ->
    pure $
      Zebra.Double Zebra.DenyDefault xs

  Icicle.Time xs ->
    encodeIntTime xs

  Icicle.Sum tags x y -> do
    fmap (Zebra.Enum Zebra.DenyDefault $ encodeBoolTags tags) $
      Cons.from2
        <$> (Zebra.Variant "left" <$> encodeColumn x)
        <*> (Zebra.Variant "right" <$> encodeColumn y)

  Icicle.Option tags x ->
    fmap (Zebra.Enum Zebra.DenyDefault $ encodeBoolTags tags) $
      Cons.from2
        <$> pure (Zebra.Variant "none" . Zebra.Unit $ Storable.length tags)
        <*> (Zebra.Variant "some" <$> encodeColumn x)

  Icicle.Result tags x ->
    Zebra.Enum Zebra.DenyDefault
      <$> encodeErrorTags tags
      <*> (from4
        <$> (Zebra.Variant "success" <$> encodeColumn x)
        <*> pure (Zebra.Variant "tombstone" . Zebra.Unit $ Storable.length tags)
        <*> pure (Zebra.Variant "fold1_no_value" . Zebra.Unit $ Storable.length tags)
        <*> pure (Zebra.Variant "cannot_compute" . Zebra.Unit $ Storable.length tags))

  Icicle.Pair x y ->
    fmap (Zebra.Struct Zebra.DenyDefault) $
      Cons.from2
        <$> (Zebra.Field "first" <$> encodeColumn x)
        <*> (Zebra.Field "second" <$> encodeColumn y)

  Icicle.Struct fields ->
    Zebra.Struct Zebra.DenyDefault
      <$> traverse encodeField fields

  Icicle.String ns bss ->
    pure $
      Zebra.Nested ns (Zebra.Binary Zebra.DenyDefault Encoding.Utf8 bss)

  Icicle.Array ns x ->
    fmap (Zebra.Nested ns) $
      Zebra.Array Zebra.DenyDefault
        <$> encodeColumn x

  Icicle.Map ns k v ->
    fmap (Zebra.Nested ns) $
      Zebra.Map Zebra.DenyDefault
        <$> encodeColumn k
        <*> encodeColumn v

encodeFactsetId :: Int -> Zebra.Column
encodeFactsetId n =
  Zebra.Reversed . Zebra.Int Zebra.DenyDefault Encoding.Int $
    Storable.replicate n 0

encodeInputTime :: Storable.Vector InputTime -> Either ZebraStripedError Zebra.Column
encodeInputTime times =
  fmap (Zebra.Struct Zebra.DenyDefault) $
    Cons.from2
      <$> (Zebra.Field "time" <$> encodeIntTime (Storable.map unInputTime times))
      <*> pure (Zebra.Field "factset_id" . encodeFactsetId $ Storable.length times)

encodeTombstoneTags :: Storable.Vector Error64 -> Storable.Vector Zebra.Tag
encodeTombstoneTags =
  Storable.map $ \case
    Tombstone64 ->
      0
    NotAnError64 ->
      1
    _ ->
      0

encodeInputColumn :: InputColumn -> Either ZebraStripedError Zebra.Column
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

    tags =
      encodeTombstoneTags $ inputTombstone input

    option x =
      Zebra.Enum Zebra.DenyDefault tags $
        Cons.from2
          (Zebra.Variant "none" . Zebra.Unit $ Storable.length tags)
          (Zebra.Variant "some" x)
  in
    fmap (Zebra.Nested key_counts) $
      Zebra.Map Zebra.AllowDefault
        <$> encodeInputTime time
        <*> (nested . option <$> encodeColumn (inputColumn input))

encodeInputColumns :: Map InputId InputColumn -> Either ZebraStripedError Zebra.Column
encodeInputColumns kvs0 = do
  kvs <- maybeToRight ZebraStripedNoInputs . Cons.fromList $ Map.toList kvs0
  fmap (Zebra.Struct Zebra.DenyDefault) . for kvs $ \(k, v) ->
    Zebra.Field
      <$> pure (Zebra.FieldName $ renderInputId k)
      <*> encodeInputColumn v

encodeEntityHash :: Boxed.Vector EntityHash -> Zebra.Column
encodeEntityHash xs = do
  Zebra.Int Zebra.DenyDefault Encoding.Int . Storable.convert $
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
    Zebra.Nested ns (Zebra.Binary Zebra.DenyDefault Encoding.Utf8 bss)

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

encodeInput :: Input -> Either ZebraStripedError Zebra.Table
encodeInput input =
  Zebra.Map Zebra.DenyDefault
    <$> pure (encodeEntityKey $ inputKey input)
    <*> encodeInputColumns (inputColumns input)

encodeOutputColumns :: Map OutputId Icicle.Column -> Either ZebraStripedError Zebra.Column
encodeOutputColumns kvs0 = do
  kvs <- maybeToRight ZebraStripedNoOutputs . Cons.fromList $ Map.toList kvs0
  fmap (Zebra.Struct Zebra.DenyDefault) . for kvs $ \(k, v) ->
    Zebra.Field
      <$> pure (Zebra.FieldName $ renderOutputId k)
      <*> encodeColumn v

encodeSnapshotKey :: Boxed.Vector SnapshotKey -> Zebra.Column
encodeSnapshotKey =
  encodeEntityKey . fmap snapshotEntity

encodeSnapshotOutput :: Output SnapshotKey -> Either ZebraStripedError Zebra.Table
encodeSnapshotOutput output =
  Zebra.Map Zebra.DenyDefault
    <$> pure (encodeSnapshotKey $ outputKey output)
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

encodeChordOutput :: Output ChordKey -> Either ZebraStripedError Zebra.Table
encodeChordOutput output =
  Zebra.Map Zebra.DenyDefault
    <$> pure (encodeChordKey $ outputKey output)
    <*> encodeOutputColumns (outputColumns output)
