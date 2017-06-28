{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Data.Fact (
    Entity(..)
  , Fact(..)
  , Fact'(..)
  , AsAt(..)
  , Value(..)
  , Struct(..)
  , List(..)
  , Encoding(..)
  , StructField(..)
  , StructFieldType(..)
  , structFieldName
  ) where

import           Icicle.Data.Name
import           Icicle.Data.Time
import           Icicle.Internal.Pretty

import           GHC.Generics

import           P


newtype Entity =
  Entity {
      getEntity :: Text
    } deriving (Eq, Ord, Show, Generic)

data Fact =
  Fact {
      factEntity    :: Entity
    , factAttribute :: InputName
    , factValue     :: Value
    } deriving (Eq, Show, Generic)

data Fact' =
  Fact' {
      factEntity'    :: Entity
    , factAttribute' :: InputName
    , factValue'     :: Text
    } deriving (Eq, Show, Generic)

data AsAt a =
  AsAt {
      atFact :: a
    , atTime :: Time
    } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

instance Pretty Entity where
  pretty =
    pretty . getEntity

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
  Struct [(Text, Value)]
  deriving (Eq, Show, Generic)

instance Pretty Struct where
  pretty (Struct avs) = pretty avs

--------------------------------------------------------------------------------

data List =
  List [Value]
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
  StructField StructFieldType Text Encoding
  deriving (Eq, Show, Generic)

instance Pretty StructField where
 pretty (StructField Mandatory attr enc)
  = pretty attr <+> ":" <+> pretty enc
 pretty (StructField Optional attr enc)
  = "optional" <+> pretty attr <+> ":" <+> pretty enc

structFieldName :: StructField -> Text
structFieldName (StructField _ attr _) =
  attr

data StructFieldType =
    Mandatory
  | Optional
    deriving (Eq, Ord, Show, Generic)
