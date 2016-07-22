{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable    #-}

module Icicle.Data (
    Entity (..)
  , Namespace (..)
  , Attribute (..)
  , FeatureId (..)
  , NormalisedFact (..)
  , Fact (..)
  , FactMode (..)
  , AsAt (..)
  , Value (..)
  , Struct (..)
  , List (..)
  , Time
  , Encoding (..)
  , StructField (..)
  , StructFieldType (..)
  , attributeOfStructField
  , factAsAt
  ) where


import           Data.Text
import           Icicle.Internal.Pretty

import           Icicle.Data.Time

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

instance Pretty Namespace where
  pretty (Namespace x) = text (unpack x)


newtype Attribute =
  Attribute {
      getAttribute  :: Text
    } deriving (Eq, Ord, Show)

instance Pretty Attribute where
  pretty (Attribute t) = text (unpack t)


data FeatureId =
  FeatureId {
    fidNamespace :: Namespace
  , fidAttribute :: Attribute
  }


data FactMode =
    FactModeEvent
  | FactModeStateSparse
  | FactModeStateDense
  deriving (Eq, Show)

instance Pretty FactMode where
 pretty FactModeEvent       = "event"
 pretty FactModeStateSparse = "sparse_state"
 pretty FactModeStateDense  = "dense_state"


data NormalisedFact = NormalisedFact {
    factNormalised :: Fact Value
  , factMode       :: FactMode }
  deriving (Eq, Show)


data Fact a = Fact {
    factEntity    :: Entity
  , factAttribute :: Attribute
  , factValue     :: a
  } deriving (Eq, Show)


data AsAt a =
  AsAt {
      atFact :: a
    , atTime :: Time
    } deriving (Eq, Show, Functor, Foldable, Traversable)

factAsAt :: AsAt NormalisedFact -> Fact Value
factAsAt = factNormalised . atFact

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
  deriving (Eq, Show)

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
  | TimeEncoding
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
      TimeEncoding      -> "Time"
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
  deriving (Eq, Ord, Show)

