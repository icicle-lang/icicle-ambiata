{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Runtime.Serial.Psv.Schema (
    SerialPsvSchemaError(..)
  , renderSerialPsvSchemaError

  , PsvSchema(..)
  , PsvMissingValue(..)
  , PsvColumn(..)
  , PsvEncoding(..)
  , PsvPrimitive(..)
  , PsvStructField(..)

  , encodePsvSnapshotSchema
  , encodePsvChordSchema
  , encodePsvColumn
  , encodePsvEncoding

  , renderPrettyPsvSchema
  , renderCompactPsvSchema
  , parsePsvSchema

  , pSchema
  , pColumn
  , pEncoding
  , pStructField
  , pPrimitive

  , ppSchema
  , ppColumn
  , ppEncoding
  , ppStructField
  , ppPrimitive
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Vector as Boxed

import           Icicle.Data.Name
import           Icicle.Internal.Aeson
import qualified Icicle.Runtime.Data.Schema as Icicle

import           P

import           Text.Show.Pretty (ppShow)


data SerialPsvSchemaError =
    SerialPsvSchemaError !AesonDecodeError
  | SerialPsvUnsupportedSchema !Icicle.Schema
    deriving (Eq, Show)

renderSerialPsvSchemaError :: SerialPsvSchemaError -> Text
renderSerialPsvSchemaError = \case
  SerialPsvSchemaError err ->
    renderAesonDecodeError err
  SerialPsvUnsupportedSchema x ->
    "Unsupported PSV output: " <> Text.pack (ppShow x)

data PsvPrimitive =
    PsvBoolean
  | PsvInt
  | PsvDouble
  | PsvString
  | PsvDate
    deriving (Eq, Ord, Show)

data PsvStructField =
  PsvStructField {
      fieldName :: !Text
    , fieldEncoding :: !PsvEncoding
    } deriving (Eq, Ord, Show)

data PsvEncoding =
    PsvPrimitive !PsvPrimitive
  | PsvStruct ![PsvStructField]
  | PsvList !PsvEncoding
  | PsvPair !PsvEncoding !PsvEncoding
    deriving (Eq, Ord, Show)

data PsvColumn =
  PsvColumn {
      columnName :: !Text
    , columnEncoding :: !PsvEncoding
    } deriving (Eq, Ord, Show)

newtype PsvMissingValue =
  PsvMissingValue {
      unPsvMissingValue :: Text
    } deriving (Eq, Ord, Show)

data PsvSchema =
  PsvSchema {
      schemaMissingValue :: !PsvMissingValue
    , schemaColumns :: ![PsvColumn]
    } deriving (Eq, Ord, Show)

pPrimitive :: Aeson.Value -> Aeson.Parser PsvPrimitive
pPrimitive =
  Aeson.withText "PsvPrimitive" $ \case
    "boolean" ->
      pure PsvBoolean
    "int" ->
      pure PsvInt
    "double" ->
      pure PsvDouble
    "string" ->
      pure PsvString
    "date" ->
      pure PsvDate
    x ->
      fail $ "Unknown primitive encoding: " <> Text.unpack x

ppPrimitive :: PsvPrimitive -> Aeson.Value
ppPrimitive = \case
  PsvBoolean ->
    ppText "boolean"
  PsvInt ->
    ppText "int"
  PsvDouble ->
    ppText "double"
  PsvString ->
    ppText "string"
  PsvDate ->
    ppText "date"

pStructField :: Aeson.Value -> Aeson.Parser (Int, PsvStructField)
pStructField =
  Aeson.withObject "PsvStructField" $ \o -> do
    name <- withKey "name" o pText
    index <- withKey "index" o pInt
    encoding <- withKey "encoding" o pEncoding
    pure (index, PsvStructField name encoding)

ppStructField :: Int -> PsvStructField -> Aeson.Value
ppStructField ix (PsvStructField name encoding) =
  Aeson.object [
      "index" .=
        ppInt ix -- this doesn't even make sense!

    , "name" .=
        ppText name

    , "encoding" .=
        ppEncoding encoding
    ]

pIndexedArray :: String -> (Aeson.Value -> Aeson.Parser (Int, a)) -> Aeson.Value -> Aeson.Parser [a]
pIndexedArray expected p =
  Aeson.withArray expected $ \xs -> do
    ixs <- traverse p xs
    pure .
      fmap snd .
      List.sortBy (comparing fst) $
      Boxed.toList ixs

pEncoding :: Aeson.Value -> Aeson.Parser PsvEncoding
pEncoding =
  pEnum "PsvEncoding" $ \case
    "primitive" ->
      Just $
        fmap PsvPrimitive . pPrimitive

    "struct" ->
      Just $
        fmap PsvStruct . pIndexedArray "PsvStruct (fields)" pStructField

    "listof" ->
      Just $
        fmap PsvList . pEncoding

    "pairof" ->
      Just $
        Aeson.withArray "PsvPair (contents)" $ \xs0 ->
          case Boxed.toList xs0 of
            [x, y] ->
              PsvPair <$> pEncoding x <*> pEncoding y
            xs ->
              fail $
                "Expected two element array for contents of pair fields but found: " <>
                show (length xs) <> " element array"

    x ->
      fail $ "Unknown encoding: " <> Text.unpack x

ppEncoding :: PsvEncoding -> Aeson.Value
ppEncoding = \case
  PsvPrimitive x ->
    ppEnum "primitive" $
      ppPrimitive x

  PsvStruct fields ->
    ppEnum "struct" . Aeson.Array .
      Boxed.imap ppStructField $ Boxed.fromList fields

  PsvList x ->
    ppEnum "listof" $
      ppEncoding x

  PsvPair x y ->
    ppEnum "pairof" . Aeson.Array $
      Boxed.fromList [ppEncoding x, ppEncoding y]

pColumn :: Aeson.Value -> Aeson.Parser (Int, PsvColumn)
pColumn =
  Aeson.withObject "PsvColumn" $ \o -> do
    name <- withKey "name" o pText
    index <- withKey "index" o pInt
    encoding <- withKey "encoding" o pEncoding
    pure (index, PsvColumn name encoding)

ppColumn :: Int -> PsvColumn -> Aeson.Value
ppColumn ix (PsvColumn name encoding) =
  Aeson.object [
      "index" .=
        ppInt ix

    , "name" .=
        ppText name

    , "encoding" .=
        ppEncoding encoding
    ]

pSchema :: Aeson.Value -> Aeson.Parser PsvSchema
pSchema =
  Aeson.withObject "PsvSchema" $ \o -> do
    expectKey "" "version" o pText "1"
    expectKey "" "encoding_version" o pText "1"

    () <-
      withKey "entity_id" o .
      Aeson.withObject "entity_id" $ \x -> do
        expectKey "entity_id" "encoding" x pPrimitive PsvString
        expectKey "entity_id" "index" x pInt 0

    missing <-
      withKey "global_properties" o $
      Aeson.withObject "global_properties" $ \x -> do
        PsvMissingValue <$> withKey "missing_value" x pText

    columns <-
      withKey "attributes" o $
        pIndexedArray "PsvSchema (columns)" pColumn

    pure $
      PsvSchema missing columns

ppSchema :: PsvSchema -> Aeson.Value
ppSchema x =
  Aeson.object [
      "version" .=
        ppText "1"

    , "encoding_version" .=
        ppText "1"

    , "global_properties" .=
        Aeson.object [
            "missing_value" .=
              ppText (unPsvMissingValue $ schemaMissingValue x)
          ]

    , "entity_id" .=
        Aeson.object [
            "index" .=
              ppInt 0
          , "encoding" .=
              ppPrimitive PsvString
          ]

    , "attributes" .=
        Aeson.Array (Boxed.imap (ppColumn . (+ 1)) . Boxed.fromList $ schemaColumns x)
    ]

schemaKeyOrder :: [Text]
schemaKeyOrder = [
    "version"
  , "encoding_version"
  , "global_properties"
  , "entity_id"
  , "attributes"
  , "index"
  , "name"
  , "encoding"
  ]

renderPrettyPsvSchema :: PsvSchema -> Text
renderPrettyPsvSchema =
  encodePrettyJson schemaKeyOrder . ppSchema

renderCompactPsvSchema :: PsvSchema -> Text
renderCompactPsvSchema =
  encodeCompactJson schemaKeyOrder . ppSchema

parsePsvSchema :: Text -> Either SerialPsvSchemaError PsvSchema
parsePsvSchema =
  first SerialPsvSchemaError . decodeJson pSchema

------------------------------------------------------------------------
-- Schema: Icicle -> Walrus

encodePsvEncoding :: Icicle.Schema -> Either SerialPsvSchemaError PsvEncoding
encodePsvEncoding schema =
  case schema of
    Icicle.Unit ->
      pure $ PsvStruct []

    Icicle.Int ->
      pure $ PsvPrimitive PsvInt

    Icicle.Time ->
      pure $ PsvPrimitive PsvDate

    Icicle.Double ->
      pure $ PsvPrimitive PsvDouble

    Icicle.Bool ->
      pure $ PsvPrimitive PsvBoolean

    Icicle.Sum _ _ ->
      Left $
        SerialPsvUnsupportedSchema schema

    Icicle.Option x ->
      encodePsvEncoding x

    Icicle.Result x ->
      encodePsvEncoding x

    Icicle.Pair _ _ ->
      Left $
        SerialPsvUnsupportedSchema schema

    Icicle.Struct _ ->
      -- FIXME
      Left $
        SerialPsvUnsupportedSchema schema

    Icicle.String ->
      pure $ PsvPrimitive PsvString

    Icicle.Array x ->
      PsvList <$> encodePsvEncoding x

    Icicle.Map k v->
      PsvList <$> (PsvPair <$> encodePsvEncoding k <*> encodePsvEncoding v)

encodePsvColumn :: OutputId -> Icicle.Schema -> Either SerialPsvSchemaError PsvColumn
encodePsvColumn name column =
  PsvColumn (renderOutputId name) <$> encodePsvEncoding column

encodePsvSnapshotSchema :: Map OutputId Icicle.Schema -> Either SerialPsvSchemaError PsvSchema
encodePsvSnapshotSchema kvs =
  PsvSchema (PsvMissingValue "NA") <$> traverse (uncurry encodePsvColumn) (Map.toList kvs)

psvLabelColumn :: PsvColumn
psvLabelColumn =
  PsvColumn "label" (PsvPrimitive PsvString)

encodePsvChordSchema :: Map OutputId Icicle.Schema -> Either SerialPsvSchemaError PsvSchema
encodePsvChordSchema kvs = do
  columns <- traverse (uncurry encodePsvColumn) (Map.toList kvs)
  pure $
    PsvSchema (PsvMissingValue "NA") (psvLabelColumn : columns)
