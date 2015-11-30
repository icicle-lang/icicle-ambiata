{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
module Icicle.Data (
    Entity (..)
  , Namespace (..)
  , Attribute
  , FeatureId (..)
  , Fact (..)
  , Fact' (..)
  , AsAt (..)
  , Value (..)
  , Struct (..)
  , List (..)
  , DateTime
  , Encoding (..)
  , StructField (..)
  , StructFieldType (..)
  , attributeOfStructField
  , mkAttribute
  , getAttribute
  ) where

import           Data.Text
import           Icicle.Internal.Pretty

import           Icicle.Data.DateTime

import           P


newtype Entity =
  Entity {
      getEntity     :: Text
    } deriving (Eq, Ord, Show)

instance Pretty Entity where
  pretty (Entity t) = text (unpack t)

newtype Namespace =
  Namespace {
      getNamespace  :: Text
    } deriving (Eq, Ord, Show)

newtype Attribute =
  Attribute {
      getAttribute  :: Text
    } deriving (Eq, Ord, Show)

data FeatureId =
  FeatureId {
    fidNamespace :: Namespace
  , fidAttribute :: Attribute
  }


instance Pretty Attribute where
  pretty (Attribute t) = text (unpack t)

data Fact =
  Fact {
      entity        :: Entity
    , attribute     :: Attribute
    , value         :: Value
    } deriving (Eq, Show)


data Fact' =
  Fact' {
      entity'       :: Entity
    , attribute'    :: Attribute
    , value'        :: Text
    } deriving (Eq, Show)


data AsAt a =
  AsAt {
      fact          :: a
    , time          :: DateTime
    } deriving (Eq, Show, Functor)

mkAttribute :: Text -> Attribute
mkAttribute = Attribute . toLower

--------------------------------------------------------------------------------

data Value =
    StringValue     Text
  | IntValue        Int
  | DoubleValue     Double
  | BooleanValue    Bool
  | DateValue       DateTime
  | StructValue     Struct
  | ListValue       List
  | PairValue       Value Value
  | MapValue        [(Value, Value)]
  | Tombstone
  deriving (Eq, Show)

instance Pretty Value where
  pretty v = case v of
    StringValue  t  -> text $ show t
    IntValue     i  -> int i
    DoubleValue  d  -> double d
    BooleanValue b  -> pretty b
    DateValue    d  -> pretty $ renderDate d
    StructValue  s  -> pretty s
    ListValue    l  -> pretty l
    PairValue v1 v2 -> encloseSep lparen rparen comma
                                     [pretty v1, pretty v2]
    MapValue vs     -> pretty vs
    Tombstone       -> text "tombstone"

--------------------------------------------------------------------------------

data Struct =
  Struct    [(Attribute, Value)]
  deriving (Eq, Show)

instance Pretty Struct where
  pretty (Struct avs) = pretty avs

--------------------------------------------------------------------------------

data List =
  List      [Value]
  deriving (Eq, Show)

instance Pretty List where
  pretty (List vs) = pretty vs

--------------------------------------------------------------------------------

data Encoding =
    StringEncoding
  | IntEncoding
  | DoubleEncoding
  | BooleanEncoding
  | DateEncoding
  | StructEncoding  [StructField]
  | ListEncoding    Encoding
  deriving (Eq, Show)

instance Pretty Encoding where
  pretty e
   = case e of
      StringEncoding    -> "String"
      IntEncoding       -> "Int"
      DoubleEncoding    -> "Double"
      BooleanEncoding   -> "Bool"
      DateEncoding      -> "Date"
      StructEncoding ss -> "Struct" <+> pretty ss
      ListEncoding l    -> "[" <> pretty l <> "]"


data StructField =
    StructField StructFieldType Attribute Encoding
  deriving (Eq, Show)

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
  deriving (Eq, Show)
