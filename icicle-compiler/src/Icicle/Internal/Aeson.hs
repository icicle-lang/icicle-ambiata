{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Internal.Aeson (
    AesonDecodeError(..)
  , renderAesonDecodeError

  , decodeJson
  , encodePrettyJson
  , encodeCompactJson

  , prettyConfig
  , compactConfig

  , pText
  , pRendered
  , pInt
  , pUnit
  , pEnum
  , pVector
  , pNonEmpty
  , pList
  , pMap
  , withKey
  , expectKey

  , ppText
  , ppRendered
  , ppInt
  , ppUnit
  , ppEnum
  , ppVector
  , ppNonEmpty
  , ppList
  , ppMap
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
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Boxed

import           P


data AesonDecodeError =
    AesonDecodeError !Aeson.JSONPath !Text
    deriving (Eq, Show)

renderAesonDecodeError :: AesonDecodeError -> Text
renderAesonDecodeError = \case
  AesonDecodeError path msg ->
    Text.pack . Aeson.formatError path $ Text.unpack msg

decodeJson :: (Aeson.Value -> Aeson.Parser a) -> Text -> Either AesonDecodeError a
decodeJson p =
  first (uncurry AesonDecodeError . second Text.pack) .
    Aeson.eitherDecodeStrictWith Aeson.value' (Aeson.iparse p) .
    Text.encodeUtf8

encodePrettyJson :: [Text] -> Aeson.Value -> Text
encodePrettyJson keyOrder =
  Text.decodeUtf8 . Lazy.toStrict . (<> "\n") . Aeson.encodePretty' (prettyConfig keyOrder)

encodeCompactJson :: [Text] -> Aeson.Value -> Text
encodeCompactJson keyOrder =
  Text.decodeUtf8 . Lazy.toStrict . Aeson.encodePretty' (compactConfig keyOrder)

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

pText :: Aeson.Value -> Aeson.Parser Text
pText =
  Aeson.parseJSON

ppText :: Text -> Aeson.Value
ppText =
  Aeson.toJSON

pRendered :: String -> (Text -> Maybe a) -> Aeson.Value -> Aeson.Parser a
pRendered expected parse x = do
  txt <- pText x
  case parse txt of
    Nothing ->
      fail $
        "Not a valid " <> expected <> " <" <> Text.unpack txt <> ">"
    Just name ->
      pure name

ppRendered :: (a -> Text) -> a -> Aeson.Value
ppRendered render =
  ppText . render

pInt :: Aeson.Value -> Aeson.Parser Int
pInt =
  Aeson.parseJSON

ppInt :: Int -> Aeson.Value
ppInt =
  Aeson.toJSON

pUnit :: Aeson.Value -> Aeson.Parser ()
pUnit =
  Aeson.withObject "Unit (i.e. {})" $ \o ->
    if HashMap.null o then
      pure ()
    else
      fail $
        "Expected an object containing unit (i.e. {})," <>
        "\nbut found an object with one or more members"

ppUnit :: Aeson.Value
ppUnit =
  Aeson.Object HashMap.empty

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

pVector :: String -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser (Boxed.Vector a)
pVector expected pValue =
  Aeson.withArray expected $
    traverse pValue

ppVector :: (a -> Aeson.Value) -> Boxed.Vector a -> Aeson.Value
ppVector ppValue xs =
  Aeson.Array $
    fmap ppValue xs

pList :: String -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser [a]
pList expected pValue =
  fmap Boxed.toList . pVector expected pValue

ppList :: (a -> Aeson.Value) -> [a] -> Aeson.Value
ppList ppValue =
  ppVector ppValue . Boxed.fromList

pNonEmpty :: String -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Value -> Aeson.Parser (NonEmpty a)
pNonEmpty expected pValue v = do
  xs0 <- pList expected pValue v
  case xs0 of
    [] ->
      fail $
        "Expected " <> expected <> " which must be non-empty, but found an empty array"
    x : xs ->
      pure $
        x :| xs

ppNonEmpty :: (a -> Aeson.Value) -> NonEmpty a -> Aeson.Value
ppNonEmpty ppValue =
  ppList ppValue . toList

pMap :: Ord k => String -> (Text -> Maybe k) -> (Aeson.Value -> Aeson.Parser v) -> Aeson.Value -> Aeson.Parser (Map k v)
pMap expected parseKey pValue =
  let
    parse (k0, v0) =
      case parseKey k0 of
        Nothing ->
          fail ("failed to parse key of map: " <> Text.unpack k0) <?> Aeson.Key k0
        Just k -> do
          v <- pValue v0 <?> Aeson.Key k0
          pure (k, v)
  in
    Aeson.withObject expected $
      fmap Map.fromList . traverse parse . HashMap.toList

ppMap :: (k -> Text) -> (v -> Aeson.Value) -> Map k v -> Aeson.Value
ppMap renderKey ppValue =
  let
    keyed (k, v) =
      renderKey k .=
        ppValue v
  in
    Aeson.object . fmap keyed . Map.toList

withKey :: Text -> Aeson.Object -> (Aeson.Value -> Aeson.Parser a) -> Aeson.Parser a
withKey key o p = do
  x <- o .: key
  p x <?> Aeson.Key key

expectKey :: (Eq a, Show a) => Text -> Text -> Aeson.Object -> (Aeson.Value -> Aeson.Parser a) -> a -> Aeson.Parser ()
expectKey prefix key0 o p expected = do
  x <- withKey key0 o p
  unless (x == expected) . fail $
    let
      key =
        if Text.null prefix then
          key0
        else
          prefix <> "/" <> key0
    in
      "Expected " <> Text.unpack key <> " to be <" <> show expected <> "> but was <" <> show x <> ">"
