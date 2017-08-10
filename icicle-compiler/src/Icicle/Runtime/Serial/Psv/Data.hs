{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Runtime.Serial.Psv.Data (
    SerialPsvDataError(..)
  , renderSerialPsvDataError

  , encodeSnapshotOutput
  , encodeChordOutput
  ) where

import qualified Anemone.Pretty as Anemone

import qualified Data.Aeson as Aeson
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as Storable

import           Icicle.Data.Name
import           Icicle.Internal.Aeson
import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Logical as Logical
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped

import           P

import           Text.Show.Pretty (ppShow)
import           Text.Printf (printf)

import qualified X.Data.Vector as Boxed

import qualified Zebra.X.Vector.Segment as Segment


data SerialPsvDataError =
    SerialPsvDataUnsupportedSchema !Schema
  | SerialPsvDataUnsupportedValue !Value
  | SerialPsvDataSegmentError !Schema !Segment.SegmentError
  | SerialPsvDataStripedError !Striped.StripedError
  | SerialPsvDataOutputError !OutputId !SerialPsvDataError
  | SerialPsvDataMissingValue !Error64
    deriving (Eq, Show)

renderSerialPsvDataError :: SerialPsvDataError -> Text
renderSerialPsvDataError = \case
  SerialPsvDataUnsupportedSchema x ->
    "Unsupported PSV output: " <> Text.pack (ppShow x)
  SerialPsvDataUnsupportedValue x ->
    "Unsupported PSV output: " <> Text.pack (ppShow x)
  SerialPsvDataSegmentError s x ->
    "Error encoding PSV, invalid segment descriptor for " <> Text.pack (ppShow s) <> ": " <> Segment.renderSegmentError x
  SerialPsvDataStripedError x ->
    "Error encoding PSV: " <> Striped.renderStripedError x
  SerialPsvDataOutputError oid x ->
    "Error encoding <" <> renderOutputId oid <> "> output: " <> renderSerialPsvDataError x
  SerialPsvDataMissingValue x ->
    "Internal error, found missing value: " <> Text.pack (ppShow x)

encodeBool :: Bool64 -> Builder
encodeBool = \case
  False64 ->
    "false"
  _ ->
    "true"
{-# INLINE encodeBool #-}

encodeInt :: Int64 -> Builder
encodeInt =
  Builder.int64Dec
{-# INLINE encodeInt #-}

encodeDouble :: Double -> Builder
encodeDouble =
  Builder.byteString . Anemone.renderDouble
{-# INLINE encodeDouble #-}

encodeDateString :: Time64 -> String
encodeDateString x =
  let
    UnpackedTime64 y m d _ =
      unpackTime x
  in
    printf "%04d-%02d-%02d" y m d

encodeDate :: Time64 -> Builder
encodeDate =
  Builder.byteString . Char8.pack . encodeDateString

encodeMissing :: Eq a => Builder -> a -> a -> Builder -> Builder
encodeMissing missing tag_ok tag value =
  if tag_ok == tag then
    value
  else
    missing
{-# INLINE encodeMissing #-}

ppValue :: Value -> Either SerialPsvDataError Aeson.Value
ppValue value =
  case value of
    Logical.Unit ->
      pure $ Aeson.object []
    Logical.Bool False64 ->
      pure $ Aeson.toJSON False
    Logical.Bool _ ->
      pure $ Aeson.toJSON True
    Logical.Int x ->
      pure $ Aeson.toJSON x
    Logical.Double x ->
      pure $ Aeson.toJSON x
    Logical.Time x ->
      pure . Aeson.toJSON $ encodeDateString x
    Logical.Left _ ->
      Left $
        SerialPsvDataUnsupportedValue value
    Logical.Right _ ->
      Left $
        SerialPsvDataUnsupportedValue value
    Logical.None ->
      -- FIXME should we really treat option like result?
      Left $
        SerialPsvDataMissingValue Tombstone64
    Logical.Some x ->
      ppValue x
    Logical.Error x ->
      Left $
        SerialPsvDataMissingValue x
    Logical.Success x ->
      ppValue x
    Logical.Pair x0 y0 -> do
      x <- ppValue x0
      y <- ppValue y0
      pure $
        Aeson.toJSON [x, y]
    Logical.Struct _ ->
      Left $
        SerialPsvDataUnsupportedValue value
    Logical.String x ->
      pure . Aeson.toJSON $ Text.decodeUtf8 x
    Logical.Array xs ->
      Aeson.toJSON <$> traverse ppValue xs
    Logical.Map kvs ->
      Aeson.toJSON <$> traverse (bitraverse ppValue ppValue) (Map.toList kvs)

encodeValues :: Either SerialPsvDataError (Boxed.Vector Aeson.Value) -> Either SerialPsvDataError Builder
encodeValues xs0 =
  case xs0 of
    Right xs ->
      pure . Builder.lazyByteString $ encodeCompactJson' [] (Aeson.Array xs)
    Left (SerialPsvDataMissingValue _) ->
      pure "NA"
    Left x ->
      Left x

encodeArray :: Boxed.Vector Value -> Either SerialPsvDataError Builder
encodeArray =
  encodeValues . traverse ppValue

encodeMap :: Boxed.Vector (Value, Value) -> Either SerialPsvDataError Builder
encodeMap =
  encodeValues . fmap (fmap Aeson.toJSON) . traverse (bitraverse ppValue ppValue)

encodeColumn :: Column -> Either SerialPsvDataError (Boxed.Vector Builder)
encodeColumn column =
  case column of
    Striped.Unit n ->
      pure $
        Boxed.replicate n "{}"

    Striped.Bool xs ->
      pure $
        Boxed.map encodeBool $ Storable.convert xs

    Striped.Int xs ->
      pure $
        Boxed.map encodeInt $ Storable.convert xs

    Striped.Double xs ->
      pure $
        Boxed.map encodeDouble $ Storable.convert xs

    Striped.Time xs ->
      pure $
        Boxed.map encodeDate $ Storable.convert xs

    Striped.Sum _ _ _ -> do
      Left $
        SerialPsvDataUnsupportedSchema (Striped.schema column)

    Striped.Option tags x -> do
      -- FIXME should we treat option like result?
      values <- encodeColumn x
      pure $
        Boxed.zipWith (encodeMissing "NA" True64) (Storable.convert tags) values

    Striped.Result tags x -> do
      values <- encodeColumn x
      pure $
        Boxed.zipWith (encodeMissing "NA" NotAnError64) (Storable.convert tags) values

    Striped.Pair _ _ ->
      Left $
        SerialPsvDataUnsupportedSchema (Striped.schema column)

    Striped.Struct _ ->
      -- FIXME
      Left $
        SerialPsvDataUnsupportedSchema (Striped.schema column)

    Striped.String ns bs ->
      bimap (SerialPsvDataSegmentError Schema.String) (fmap Builder.byteString) $
        Segment.reify ns bs

    Striped.Array ns c -> do
      xs <- first SerialPsvDataStripedError $ Striped.toLogical c
      xss <- first (SerialPsvDataSegmentError (Striped.schema column)) $ Segment.reify ns xs
      traverse encodeArray xss

    Striped.Map ns k v -> do
      kvs <- first SerialPsvDataStripedError $ Boxed.zip <$> Striped.toLogical k <*> Striped.toLogical v
      kvss <- first (SerialPsvDataSegmentError (Striped.schema column)) $ Segment.reify ns kvs
      traverse encodeMap kvss

encodeSnapshotKey :: SnapshotKey -> Builder
encodeSnapshotKey =
  Builder.byteString . unEntityId . entityId . snapshotEntity

encodeChordKey :: ChordKey -> Builder
encodeChordKey x =
  Builder.byteString (unEntityId . entityId $ chordEntity x) <> "|" <>
  Builder.byteString (chordLabel x)

encodeColumns :: Boxed.Vector (Boxed.Vector Builder) -> Boxed.Vector Builder
encodeColumns =
  Boxed.map (mconcat . List.intersperse "|" . Boxed.toList) .
  Boxed.transpose

encodeRows :: Boxed.Vector Builder -> Builder
encodeRows =
  mconcat . Boxed.toList . fmap (<> "\n")

encodeOutput :: (key -> Builder) -> Output key -> Either SerialPsvDataError Builder
encodeOutput encodeKey output = do
  let
    key =
      fmap encodeKey (outputKey output)

    encode (k, v) =
      first (SerialPsvDataOutputError k) (encodeColumn v)

  columns <- traverse encode . Map.toList $ outputColumns output

  pure .
    encodeRows . encodeColumns . Boxed.fromList $ key : columns

encodeSnapshotOutput :: Output SnapshotKey -> Either SerialPsvDataError Builder
encodeSnapshotOutput output =
  encodeOutput encodeSnapshotKey output

encodeChordOutput :: Output ChordKey -> Either SerialPsvDataError Builder
encodeChordOutput output =
  encodeOutput encodeChordKey output
