{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Data (
    Entity (..)
  , Namespace
  , Attribute
  , FeatureId (..)
  , FeatureName
  , Fact (..)
  , Fact' (..)
  , AsAt (..)
  , Value (..)
  , Struct (..)
  , List (..)
  , Time
  , Encoding (..)
  , StructField (..)
  , StructFieldType (..)
  , attributeOfStructField
  , asAttributeName
  , takeAttributeName
  , asNamespace
  , takeNamespace
  ) where


import           Icicle.Internal.Pretty
import           Icicle.Data.Time

import qualified Data.Text as Text
import           GHC.Generics

import           P


newtype Entity = Entity {
    getEntity :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Pretty Entity where
  pretty =
    pretty . getEntity

newtype Namespace = Namespace {
    getNamespace :: NamespaceName
  } deriving (Eq, Ord, Show, Generic)

instance Pretty Namespace where
  pretty =
    pretty . getNamespace

newtype Attribute = Attribute {
    getAttribute :: FeatureName
  } deriving (Eq, Ord, Show, Generic)

instance Pretty Attribute where
  pretty =
    pretty . getAttribute

data FeatureId = FeatureId {
    fidNamespace :: Namespace
  , fidAttribute :: Attribute
  } deriving (Eq, Ord, Show, Generic)

data Fact =
  Fact {
      factEntity    :: Entity
    , factAttribute :: Attribute
    , factValue     :: Value
    } deriving (Eq, Show, Generic)

data Fact' =
  Fact' {
      factEntity'    :: Entity
    , factAttribute' :: Attribute
    , factValue'     :: Text
    } deriving (Eq, Show, Generic)

data AsAt a =
  AsAt {
      atFact :: a
    , atTime :: Time
    } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | A name value that must:
--     * Be a valid Ivory feature name.
--     * Have a conversion to a SeaName (valid C identifier).
--
newtype FeatureName = FeatureName {
    getFeatureName :: Text
  } deriving (Eq, Ord, Show, Generic)

-- | As `FeatureName`, but can also be empty.
newtype NamespaceName = NamespaceName {
    getNamespaceName :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Pretty FeatureName where
  pretty (FeatureName x) =
    text (Text.unpack x)

instance Pretty NamespaceName where
  pretty (NamespaceName x) =
    text (Text.unpack x)

featureNameValidHead :: Char -> Bool
featureNameValidHead c =
  c >= 'a' && c <= 'z'
{-# INLINE featureNameValidHead #-}

featureNameValidTail :: Char -> Bool
featureNameValidTail c =
  featureNameValidHead c ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') ||
  (c == '-') ||
  (c == '+') ||
  (c == '_') ||
  (c == '/') ||
  (c == '%') ||
  (c == '.') ||
  (c == ',') ||
  (c == '(') ||
  (c == ')') ||
  (c == '?')
{-# INLINE featureNameValidTail #-}

asFeatureName :: Text -> Maybe FeatureName
asFeatureName t =
  case Text.unpack t of
    x:xs | featureNameValidHead x && all featureNameValidTail xs ->
      Just (FeatureName t)
    _ ->
      Nothing
{-# INLINE asFeatureName #-}

asNamespaceName :: Text -> Maybe NamespaceName
asNamespaceName t
  | Text.null t =
      Just (NamespaceName t)
  | otherwise =
      case Text.unpack t of
        x:xs | featureNameValidHead x && all featureNameValidTail xs ->
          Just (NamespaceName t)
        _ ->
          Nothing
{-# INLINE asNamespaceName #-}

asAttributeName :: Text -> Maybe Attribute
asAttributeName t =
  Attribute <$> asFeatureName t
{-# INLINE asAttributeName #-}

takeAttributeName :: Attribute -> Text
takeAttributeName =
  getFeatureName . getAttribute
{-# INLINE takeAttributeName #-}

asNamespace :: Text -> Maybe Namespace
asNamespace t =
  Namespace <$> asNamespaceName t
{-# INLINE asNamespace #-}

takeNamespace :: Namespace -> Text
takeNamespace =
  getNamespaceName . getNamespace
{-# INLINE takeNamespace #-}

--------------------------------------------------------------------------------

data Value =
    StringValue     Text
  | IntValue        Int
  | DoubleValue     Double
  | BooleanValue    Bool
  | TimeValue       Time
  | StructValue     Struct
  | ListValue       List
  | PairValue       Value Value
  | MapValue        [(Value, Value)]
  | Tombstone
  deriving (Eq, Show, Generic)

instance Pretty Value where
  pretty v = case v of
    StringValue  t  -> text $ show t
    IntValue     i  -> int i
    DoubleValue  d  -> double d
    BooleanValue b  -> pretty b
    TimeValue    d  -> pretty $ renderTime d
    StructValue  s  -> pretty s
    ListValue    l  -> pretty l
    PairValue v1 v2 -> encloseSep lparen rparen comma
                                     [pretty v1, pretty v2]
    MapValue vs     -> pretty vs
    Tombstone       -> text "tombstone"

--------------------------------------------------------------------------------

data Struct =
  Struct    [(Attribute, Value)]
  deriving (Eq, Show, Generic)

instance Pretty Struct where
  pretty (Struct avs) = pretty avs

--------------------------------------------------------------------------------

data List =
  List      [Value]
  deriving (Eq, Show, Generic)

instance Pretty List where
  pretty (List vs) = pretty vs

--------------------------------------------------------------------------------

data Encoding =
    StringEncoding
  | IntEncoding
  | DoubleEncoding
  | BooleanEncoding
  | TimeEncoding
  | StructEncoding  [StructField]
  | ListEncoding    Encoding
  deriving (Eq, Show, Generic)

instance Pretty Encoding where
  pretty e
   = case e of
      StringEncoding    -> "String"
      IntEncoding       -> "Int"
      DoubleEncoding    -> "Double"
      BooleanEncoding   -> "Bool"
      TimeEncoding      -> "Time"
      StructEncoding ss -> "Struct" <+> pretty ss
      ListEncoding l    -> "[" <> pretty l <> "]"


data StructField =
    StructField StructFieldType Attribute Encoding
  deriving (Eq, Show, Generic)

instance Pretty StructField where
 pretty (StructField Mandatory attr enc)
  = pretty attr <+> ":" <+> pretty enc
 pretty (StructField Optional attr enc)
  = "optional" <+> pretty attr <+> ":" <+> pretty enc

attributeOfStructField :: StructField -> Attribute
attributeOfStructField (StructField _ attr _)
  = attr


data StructFieldType =
    Mandatory
  | Optional
  deriving (Eq, Ord, Show, Generic)
