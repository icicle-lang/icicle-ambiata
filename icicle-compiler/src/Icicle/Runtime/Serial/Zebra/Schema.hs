{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Runtime.Serial.Zebra.Schema (
    ZebraSchemaError(..)
  , renderZebraSchemaError

  , decodeDictionary
  , encodeDictionary

  , decodeInputSchemas
  , encodeInputSchemas

  , decodeColumnSchema
  , encodeColumnSchema
  ) where

import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Icicle.Data.Name
import           Icicle.Dictionary.Data
import qualified Icicle.Runtime.Data.Primitive as Icicle
import qualified Icicle.Runtime.Data.Schema as Icicle

import           P

import qualified X.Data.Vector.Cons as Cons

import qualified Zebra.Table.Data as Zebra
import qualified Zebra.Table.Encoding as Encoding
import qualified Zebra.Table.Schema as Zebra


data ZebraSchemaError =
    ZebraSchemaError !Zebra.SchemaError
  | ZebraSchemaUnexpectedBinaryEncoding !Encoding.Binary
  | ZebraSchemaUnexpectedReversed !Zebra.Column
  | ZebraSchemaUnknownEnum ![Zebra.Variant Zebra.Column]
  | ZebraSchemaInvalidInputId !Text
  | ZebraSchemaNoInputs
  | ZebraSchemaReflectDictionaryError !Icicle.SchemaError
  | ZebraSchemaReifyDictionaryError !Icicle.SchemaError
    deriving (Eq, Show)

renderZebraSchemaError :: ZebraSchemaError -> Text
renderZebraSchemaError = \case
  ZebraSchemaError x ->
    Zebra.renderSchemaError x
  ZebraSchemaUnexpectedBinaryEncoding x ->
    "Unexpected binary encoding: " <> Text.pack (show x)
  ZebraSchemaUnexpectedReversed x ->
    "Unexpected reversed column: " <> Text.pack (show x)
  ZebraSchemaUnknownEnum xs ->
    "Could not convert unknown enum: " <> Text.pack (show xs)
  ZebraSchemaInvalidInputId x ->
    "Found invalid input-id: " <> x
  ZebraSchemaNoInputs ->
    "Could not encode schema in zebra as it had no inputs."
  ZebraSchemaReflectDictionaryError x ->
    "Error converting a schema in to a dictionary: " <> Icicle.renderSchemaError x
  ZebraSchemaReifyDictionaryError x ->
    "Error converting a dictionary in to a schema: " <> Icicle.renderSchemaError x


------------------------------------------------------------------------
-- Schema: Zebra -> Icicle

decodeFieldSchema :: Zebra.Field Zebra.Column -> Either ZebraSchemaError (Icicle.Field Icicle.Schema)
decodeFieldSchema (Zebra.Field (Zebra.FieldName name) column) =
  Icicle.Field name <$> decodeColumnSchema column

decodeTableSchema :: Zebra.Table -> Either ZebraSchemaError Icicle.Schema
decodeTableSchema = \case
  Zebra.Binary _ encoding@Encoding.Binary ->
    Left $ ZebraSchemaUnexpectedBinaryEncoding encoding
  Zebra.Binary _ Encoding.Utf8 ->
    pure Icicle.String

  Zebra.Array _ column ->
    Icicle.Array <$> decodeColumnSchema column

  Zebra.Map _ kcolumn vcolumn ->
    Icicle.Map <$> decodeColumnSchema kcolumn <*> decodeColumnSchema vcolumn

decodeColumnSchema :: Zebra.Column -> Either ZebraSchemaError Icicle.Schema
decodeColumnSchema = \case
  Zebra.Unit ->
    pure Icicle.Unit

  Zebra.Int _ Encoding.Int ->
    pure Icicle.Int
  Zebra.Int _ Encoding.Date ->
    pure Icicle.Time
  Zebra.Int _ Encoding.TimeSeconds ->
    pure Icicle.Time
  Zebra.Int _ Encoding.TimeMilliseconds ->
    pure Icicle.Time
  Zebra.Int _ Encoding.TimeMicroseconds ->
    pure Icicle.Time

  Zebra.Double _ ->
    pure Icicle.Double

  Zebra.Enum _ variants ->
    case Cons.toList variants of
      [Zebra.Variant "false" Zebra.Unit, Zebra.Variant "true" Zebra.Unit] ->
        pure Icicle.Bool

      [Zebra.Variant "none" Zebra.Unit, Zebra.Variant "some" x] ->
         Icicle.Option <$> decodeColumnSchema x

      [Zebra.Variant "left" x, Zebra.Variant "right" y] ->
        Icicle.Sum <$> decodeColumnSchema x <*> decodeColumnSchema y

      [Zebra.Variant "success" x,
       Zebra.Variant "tombstone" Zebra.Unit,
       Zebra.Variant "fold1_no_value" Zebra.Unit,
       Zebra.Variant "cannot_compute" Zebra.Unit,
       Zebra.Variant "not_a_number" Zebra.Unit,
       Zebra.Variant "index_out_of_bounds" Zebra.Unit] ->
        Icicle.Result <$> decodeColumnSchema x

      _ ->
        Left . ZebraSchemaUnknownEnum $ Cons.toList variants

  Zebra.Struct _ fields ->
    if Cons.length fields /= 2 then
      Icicle.Struct <$> traverse decodeFieldSchema fields
    else
      case Cons.toList fields of
        [Zebra.Field "first" x, Zebra.Field "second" y] ->
          Icicle.Pair <$> decodeColumnSchema x <*> decodeColumnSchema y
        _ ->
          Icicle.Struct <$> traverse decodeFieldSchema fields

  Zebra.Nested table ->
    decodeTableSchema table

  Zebra.Reversed x ->
    Left $ ZebraSchemaUnexpectedReversed x

decodeInputSchema :: Zebra.Column -> Either ZebraSchemaError Icicle.Schema
decodeInputSchema column = do
  map <- first ZebraSchemaError $ Zebra.takeNested column
  (_, _, nested) <- first ZebraSchemaError $ Zebra.takeMap map
  array <- first ZebraSchemaError $ Zebra.takeNested nested
  (_, option) <- first ZebraSchemaError $ Zebra.takeArray array
  (_, value) <- first ZebraSchemaError $ Zebra.takeOption option
  decodeColumnSchema value

decodeNamedInputSchema :: Zebra.Field Zebra.Column -> Either ZebraSchemaError (InputId, Icicle.Schema)
decodeNamedInputSchema (Zebra.Field (Zebra.FieldName name) column) = do
  iid <- maybeToRight (ZebraSchemaInvalidInputId name) $ parseInputId name
  icolumn <- decodeInputSchema column
  pure (iid, icolumn)

decodeInputSchemas :: Zebra.Table -> Either ZebraSchemaError (Map InputId Icicle.Schema)
decodeInputSchemas table = do
  (_, _, vcolumn) <- first ZebraSchemaError $ Zebra.takeMap table
  (_, inputs) <- first ZebraSchemaError $ Zebra.takeStruct vcolumn
  fmap Map.fromList . traverse decodeNamedInputSchema $ Cons.toList inputs

mkDictionaryInput :: InputId -> Icicle.Schema -> Either ZebraSchemaError DictionaryInput
mkDictionaryInput iid schema = do
  encoding <- first ZebraSchemaReflectDictionaryError $ Icicle.toEncoding schema
  pure $
    DictionaryInput iid encoding Set.empty unkeyed

decodeDictionary :: Zebra.Table -> Either ZebraSchemaError Dictionary
decodeDictionary table = do
  schemas <- decodeInputSchemas table
  inputs <- sequence $ Map.mapWithKey mkDictionaryInput schemas
  pure $
    Dictionary inputs Map.empty []

------------------------------------------------------------------------
-- Schema: Icicle -> Zebra

encodeFieldSchema :: Icicle.Field Icicle.Schema -> Zebra.Field Zebra.Column
encodeFieldSchema (Icicle.Field name column) =
  Zebra.Field (Zebra.FieldName name) $
    case column of
      Icicle.Option x ->
        Zebra.option Zebra.AllowDefault (encodeColumnSchema x)
      x ->
        encodeColumnSchema x

encodeColumnSchema :: Icicle.Schema -> Zebra.Column
encodeColumnSchema = \case
  Icicle.Unit ->
    Zebra.Unit

  Icicle.Int ->
    Zebra.Int Zebra.DenyDefault Encoding.Int

  Icicle.Time ->
    Zebra.Int Zebra.DenyDefault Encoding.TimeSeconds

  Icicle.Double ->
    Zebra.Double Zebra.DenyDefault

  Icicle.Bool ->
    Zebra.bool Zebra.DenyDefault

  Icicle.Sum x y ->
    Zebra.either Zebra.DenyDefault (encodeColumnSchema x) (encodeColumnSchema y)

  Icicle.Option x ->
    Zebra.option Zebra.DenyDefault (encodeColumnSchema x)

  Icicle.Result x ->
    Zebra.Enum Zebra.DenyDefault $
      Cons.unsafeFromList [
          (Zebra.Variant "success" $ encodeColumnSchema x)
        , (Zebra.Variant "tombstone" Zebra.Unit)
        , (Zebra.Variant "fold1_no_value" Zebra.Unit)
        , (Zebra.Variant "cannot_compute" Zebra.Unit)
        , (Zebra.Variant "not_a_number" Zebra.Unit)
        , (Zebra.Variant "index_out_of_bounds" Zebra.Unit)
        ]

  Icicle.Pair x y ->
    Zebra.pair Zebra.DenyDefault (encodeColumnSchema x) (encodeColumnSchema y)

  Icicle.Struct fields ->
    Zebra.Struct Zebra.DenyDefault (fmap encodeFieldSchema fields)

  Icicle.String ->
    Zebra.Nested $
      Zebra.Binary Zebra.DenyDefault Encoding.Utf8

  Icicle.Array x ->
    Zebra.Nested $
      Zebra.Array Zebra.DenyDefault (encodeColumnSchema x)

  Icicle.Map k v->
    Zebra.Nested $
      Zebra.Map Zebra.DenyDefault (encodeColumnSchema k) (encodeColumnSchema v)

encodeTimeSchema :: Zebra.Column
encodeTimeSchema =
  Zebra.Struct Zebra.DenyDefault $
    Cons.from2
      (Zebra.Field "time" $
        Zebra.Int Zebra.DenyDefault Encoding.TimeSeconds)
      (Zebra.Field "factset_id" .
        Zebra.Reversed $ Zebra.Int Zebra.DenyDefault Encoding.Int)

encodeInputSchema :: Icicle.Schema -> Zebra.Column
encodeInputSchema schema =
  Zebra.Nested $
    Zebra.Map Zebra.AllowDefault
      encodeTimeSchema
      (Zebra.Nested . Zebra.Array Zebra.DenyDefault .
        Zebra.option Zebra.DenyDefault $ encodeColumnSchema schema)

encodeNamedInputSchema :: InputId -> Icicle.Schema -> Zebra.Field Zebra.Column
encodeNamedInputSchema iid schema =
  Zebra.Field
    (Zebra.FieldName $ renderInputId iid)
    (encodeInputSchema schema)

encodeInputSchemas :: Map InputId Icicle.Schema -> Either ZebraSchemaError Zebra.Table
encodeInputSchemas inputs = do
  xs <- maybeToRight ZebraSchemaNoInputs . Cons.fromList $ Map.toList inputs
  pure .
    Zebra.Map Zebra.DenyDefault encodeEntitySchema $
      Zebra.Struct Zebra.DenyDefault $
        fmap (uncurry encodeNamedInputSchema) xs

encodeEntitySchema :: Zebra.Column
encodeEntitySchema =
  Zebra.Struct Zebra.DenyDefault $
    Cons.from2
      (Zebra.Field "entity_hash" $
        Zebra.Int Zebra.DenyDefault Encoding.Int)
      (Zebra.Field "entity_id" .
        Zebra.Nested $ Zebra.Binary Zebra.DenyDefault Encoding.Utf8)

encodeDictionary :: Dictionary -> Either ZebraSchemaError Zebra.Table
encodeDictionary =
  bind encodeInputSchemas .
  first ZebraSchemaReifyDictionaryError .
  traverse (Icicle.fromEncoding . inputEncoding) .
  dictionaryInputs
