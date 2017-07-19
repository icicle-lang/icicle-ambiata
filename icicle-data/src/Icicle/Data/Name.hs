{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Data.Name (
    Namespace
  , parseNamespace
  , renderNamespace

  , InputName
  , parseInputName
  , renderInputName

  , InputId(..)
  , parseInputId
  , renderInputId

  , UnresolvedInputId(..)
  , parseUnresolvedInputId
  , renderUnresolvedInputId
  , findInputId
  , resolveInputId
  , lookupInputId
  , unresolvedInputName

  , OutputName
  , parseOutputName
  , renderOutputName

  , OutputId(..)
  , parseOutputId
  , renderOutputId

  -- * Quasi-quoters
  , namespace
  , inputname
  , inputid
  , outputname
  , outputid
  ) where

import           Data.Data (Data)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (String)
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty

import           Language.Haskell.TH.Quote (QuasiQuoter)

import           P

import qualified Prelude as Savage

import           X.Text.Show (gshowsPrec)
import           X.Language.Haskell.TH (qparse, dataExp)


-- | An icicle namespace.
--
--   This is a constrained string:
--     - It must start with:
--       - lower case [a-z]
--     - It contains only characters:
--       - lower case [a-z]
--       - numbers [0-9]
--       - underscores '_'
--       - dashes '-'
--     - It must not be empty.
--     - Its UTF-8 encoding must be 255 bytes or less.
--       - Due to the other restrictions: bytes == characters
--
--   /These restrictions are compatible with ivory fact/feature namespaces./
--
newtype Namespace =
  Namespace {
      unNamespace :: Text
    } deriving (Eq, Ord, Data, Generic)

-- | The name of an icicle input.
--
--   This is a constrained string:
--     - It must start with:
--       - lower case [a-z]
--     - It contains only characters:
--       - lower case [a-z]
--       - numbers [0-9]
--       - underscores '_'
--       - dashes '-'
--     - It must not be empty.
--     - Its UTF-8 encoding must be 255 bytes or less.
--       - Due to the other restrictions: bytes == characters
--
--   /These restrictions are compatible with ivory fact names./
--
newtype InputName =
  InputName {
      unInputName :: Text
    } deriving (Eq, Ord, Data, Generic)

-- | A namespace qualified icicle input name.
--
data InputId =
  InputId {
      inputNamespace :: !Namespace
    , inputName :: !InputName
    } deriving (Eq, Ord, Data, Generic)

-- | An input name which may or may not be qualified with a namespace.
--
data UnresolvedInputId =
    QualifiedInput !InputId
  | UnqualifiedInput !InputName
    deriving (Eq, Ord, Show, Data, Generic)

-- | The name of an icicle output.
--
--   This is a constrained string:
--     - It must start with:
--       - lower case [a-z]
--     - It contains only characters:
--       - lower case [a-z]
--       - numbers [0-9]
--       - underscores '_'
--       - dashes '-'
--     - It must not be empty.
--     - Its UTF-8 encoding must be 255 bytes or less.
--       - Due to the other restrictions: bytes == characters
--
--   /These restrictions are compatible with ivory feature names./
--
newtype OutputName =
  OutputName {
      unOutputName :: Text
    } deriving (Eq, Ord, Data, Generic)

-- | A namespace qualified icicle output name.
--
data OutputId =
  OutputId {
      outputNamespace :: !Namespace
    , outputName :: !OutputName
    } deriving (Eq, Ord, Data, Generic)

instance NFData Namespace
instance NFData InputName
instance NFData InputId
instance NFData UnresolvedInputId
instance NFData OutputName
instance NFData OutputId

instance Show Namespace where
  showsPrec =
    gshowsPrec

instance Show InputName where
  showsPrec =
    gshowsPrec

instance Show InputId where
  showsPrec =
    gshowsPrec

instance Show OutputName where
  showsPrec =
    gshowsPrec

instance Show OutputId where
  showsPrec =
    gshowsPrec

instance Pretty Namespace where
  pretty =
    text . Text.unpack . renderNamespace

instance Pretty InputName where
  pretty =
    text . Text.unpack . renderInputName

instance Pretty InputId where
  pretty =
    text . Text.unpack . renderInputId

instance Pretty UnresolvedInputId where
  pretty = \case
    UnqualifiedInput x ->
      pretty x
    QualifiedInput x ->
      pretty x

instance Pretty OutputName where
  pretty =
    text . Text.unpack . renderOutputName

instance Pretty OutputId where
  pretty =
    text . Text.unpack . renderOutputId

parseHeadTail :: (Char -> Bool) -> (Char -> Bool) -> Text -> Maybe Text
parseHeadTail validHead validTail xs =
  case Text.uncons xs of
    Nothing ->
      Nothing
    Just (y, ys) ->
      if validHead y && Text.all validTail ys && Text.length xs <= 255 then
        Just xs
      else
        Nothing

namespaceValidHead :: Char -> Bool
namespaceValidHead c =
  (c >= 'a' && c <= 'z')

namespaceValidTail :: Char -> Bool
namespaceValidTail c =
     namespaceValidHead c
  || (c >= '0' && c <= '9')
  || (c == '-')
  || (c == '_')

nameValidHead :: Char -> Bool
nameValidHead c =
  c >= 'a' && c <= 'z'

nameValidTail :: Char -> Bool
nameValidTail c =
     nameValidHead c
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || (c == '-')
  || (c == '+')
  || (c == '_')
  || (c == '/')
  || (c == '%')
  || (c == '.')
  || (c == ',')
  || (c == '(')
  || (c == ')')
  || (c == '?')

parseNamespace :: Text -> Maybe Namespace
parseNamespace =
  fmap Namespace . parseHeadTail namespaceValidHead namespaceValidTail

renderNamespace :: Namespace -> Text
renderNamespace =
  unNamespace

parseInputName :: Text -> Maybe InputName
parseInputName =
  fmap InputName . parseHeadTail nameValidHead nameValidTail

renderInputName :: InputName -> Text
renderInputName =
  unInputName

parseOutputName :: Text -> Maybe OutputName
parseOutputName =
  fmap OutputName . parseHeadTail nameValidHead nameValidTail

renderOutputName :: OutputName -> Text
renderOutputName =
  unOutputName

renderInputId :: InputId -> Text
renderInputId (InputId ns name) =
  renderNamespace ns <> ":" <> renderInputName name

parseInputId :: Text -> Maybe InputId
parseInputId txt =
  case Text.breakOn ":" txt of
    (ns, name) ->
      InputId
        <$> parseNamespace ns
        <*> parseInputName (Text.drop 1 name)

renderOutputId :: OutputId -> Text
renderOutputId (OutputId ns name) =
  renderNamespace ns <> ":" <> renderOutputName name

parseOutputId :: Text -> Maybe OutputId
parseOutputId txt =
  case Text.breakOn ":" txt of
    (ns, name) ->
      OutputId
        <$> parseNamespace ns
        <*> parseOutputName (Text.drop 1 name)

qmaybe :: Data a => String -> (Text -> Maybe a) -> QuasiQuoter
qmaybe description parse =
  qparse $ \s ->
    case parse (Text.pack s) of
      Nothing ->
        Savage.error $ "Failed to parse " <> show s <> " as " <> description
      Just m ->
        dataExp m

-- | :: Namespace
namespace :: QuasiQuoter
namespace =
  qmaybe "an icicle namespace" parseNamespace

-- | :: InputName
inputname :: QuasiQuoter -- :: InputName
inputname =
  qmaybe "an icicle input name" parseInputName

-- | :: InputId
inputid :: QuasiQuoter -- :: InputId
inputid =
  qmaybe "an icicle input-id" parseInputId

-- | :: OutputName
outputname :: QuasiQuoter -- :: OutputName
outputname =
  qmaybe "an icicle output name" parseOutputName

-- | :: OutputId
outputid :: QuasiQuoter -- :: OutputId
outputid =
  qmaybe "an icicle output-id" parseOutputId

parseUnresolvedInputId :: Text -> Maybe UnresolvedInputId
parseUnresolvedInputId x0 =
  case parseInputId x0 of
    Nothing ->
      fmap UnqualifiedInput $ parseInputName x0
    Just x ->
      pure $ QualifiedInput x

renderUnresolvedInputId :: UnresolvedInputId -> Text
renderUnresolvedInputId = \case
  UnqualifiedInput x ->
    renderInputName x
  QualifiedInput x ->
    renderInputId x

findInputId :: InputName -> [InputId] -> Maybe InputId
findInputId needle inputs =
  flip List.find inputs $ \(InputId _ name) ->
    name == needle

resolveInputId :: UnresolvedInputId -> [InputId] -> Maybe InputId
resolveInputId unresolved inputs =
  case unresolved of
    UnqualifiedInput x ->
      findInputId x inputs

    QualifiedInput x ->
      pure x

lookupInputId :: UnresolvedInputId -> Map InputId v -> Maybe v
lookupInputId unresolved kvs = do
  iid <- resolveInputId unresolved $ Map.keys kvs
  Map.lookup iid kvs

unresolvedInputName :: UnresolvedInputId -> InputName
unresolvedInputName = \case
  UnqualifiedInput x ->
    x
  QualifiedInput x ->
    inputName x
