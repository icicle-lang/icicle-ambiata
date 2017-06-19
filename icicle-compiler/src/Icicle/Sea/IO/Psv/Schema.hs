{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.IO.Psv.Schema (
    PsvSchemaDecodeError(..)
  , renderPsvSchemaDecodeError

  , PsvSchema(..)
  , PsvMissingValue(..)
  , PsvColumn(..)
  , PsvEncoding(..)
  , PsvPrimitive(..)
  , PsvStructField(..)

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

import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import           Data.Aeson.Internal ((<?>))
import qualified Data.Aeson.Internal as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Boxed

import           P


data PsvSchemaDecodeError =
    PsvSchemaDecodeError !Aeson.JSONPath !Text
    deriving (Eq, Show)

renderPsvSchemaDecodeError :: PsvSchemaDecodeError -> Text
renderPsvSchemaDecodeError = \case
  PsvSchemaDecodeError path msg ->
    Text.pack . Aeson.formatError path $ Text.unpack msg

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

pText :: Aeson.Value -> Aeson.Parser Text
pText =
  Aeson.parseJSON

ppText :: Text -> Aeson.Value
ppText =
  Aeson.toJSON

pInt :: Aeson.Value -> Aeson.Parser Int
pInt =
  Aeson.parseJSON

ppInt :: Int -> Aeson.Value
ppInt =
  Aeson.toJSON

pEnum :: String -> (Text -> Maybe (Aeson.Value -> Aeson.Parser a)) -> Aeson.Value -> Aeson.Parser a
pEnum expected mkParser =
  Aeson.withObject expected $ \o ->
    case HashMap.toList o of
      [(name, value)] -> do
        case mkParser name of
          Nothing ->
            fail (expected <> ": unknown variant: " <> Text.unpack name) <?> Aeson.Key name
          Just parser ->
            parser value <?> Aeson.Key name
      [] ->
        fail $
          "Expected " <> expected <> " but found an object with no members."
      kvs ->
        fail $
          "Expected " <> expected <> " but found an object with more than one member." <>
          "\n  " <> List.intercalate ", " (fmap (Text.unpack . fst) kvs)

ppEnum :: Text -> Aeson.Value -> Aeson.Value
ppEnum name value =
  Aeson.object [
      name .= value
    ]

withKey :: Text -> Aeson.Object -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Parser a
withKey key o p = do
  x <- o .: key
  p x <?> Aeson.Key key

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

expect :: (Eq a, Show a) => Text -> Text -> Aeson.Object -> (Aeson.Value -> Aeson.Parser a) -> a -> Aeson.Parser ()
expect prefix key o p expected = do
  x <- withKey key o p
  unless (x == expected) . fail $
    "Expected " <> Text.unpack (prefix <> key) <> " to be <" <> show expected <> "> but was <" <> show x <> ">"

pSchema :: Aeson.Value -> Aeson.Parser PsvSchema
pSchema =
  Aeson.withObject "PsvSchema" $ \o -> do
    expect "" "version" o pText "1"
    expect "" "encoding_version" o pText "1"

    () <-
      withKey "entity_id" o .
      Aeson.withObject "entity_id" $ \x -> do
        expect "entity_id/" "encoding" x pPrimitive PsvString
        expect "entity_id/" "index" x pInt 0

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

prettyConfig :: [Text] -> Aeson.Config
prettyConfig keyOrder =
  Aeson.defConfig {
      Aeson.confIndent =
        Aeson.Spaces 2
    , Aeson.confCompare =
        Aeson.keyOrder keyOrder
    }

compactConfig :: [Text] -> Aeson.Config
compactConfig keyOrder =
  (prettyConfig keyOrder) {
      Aeson.confIndent =
        Aeson.Spaces 0
    }

encodePrettyJson :: [Text] -> Aeson.Value -> Text
encodePrettyJson keyOrder =
  Text.decodeUtf8 . Lazy.toStrict . Aeson.encodePretty' (prettyConfig keyOrder)

encodeCompactJson :: [Text] -> Aeson.Value -> Text
encodeCompactJson keyOrder =
  Text.decodeUtf8 . Lazy.toStrict . Aeson.encodePretty' (compactConfig keyOrder)

decodeJson :: (Aeson.Value -> Aeson.Parser a) -> Text -> Either PsvSchemaDecodeError a
decodeJson p =
  first (uncurry PsvSchemaDecodeError . second Text.pack) .
    Aeson.eitherDecodeStrictWith Aeson.value' (Aeson.iparse p) .
    Text.encodeUtf8

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
  (<> "\n") . encodePrettyJson schemaKeyOrder . ppSchema

renderCompactPsvSchema :: PsvSchema -> Text
renderCompactPsvSchema =
  encodeCompactJson schemaKeyOrder . ppSchema

parsePsvSchema :: Text -> Either PsvSchemaDecodeError PsvSchema
parsePsvSchema =
  decodeJson pSchema
