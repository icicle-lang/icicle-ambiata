{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Data (
    Entity (..)
  , Attribute (..)
  , Fact (..)
  , Fact' (..)
  , AsAt (..)
  , Value (..)
  , Struct (..)
  , List (..)
  , Date (..)
  , DateTime (..)
  , Encoding (..)
  , StructField (..)
  , StructFieldType (..)
  , attributeOfStructField
  ) where

import           Data.Text
import qualified Text.PrettyPrint.Leijen as PP

import           Icicle.Data.DateTime

import           P


newtype Entity =
  Entity {
      getEntity     :: Text
    } deriving (Eq, Ord, Show)

instance PP.Pretty Entity where
  pretty (Entity t) = PP.text (unpack t)

newtype Attribute =
  Attribute {
      getAttribute  :: Text
    } deriving (Eq, Ord, Show)

instance PP.Pretty Attribute where
  pretty (Attribute t) = PP.text (unpack t)

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
    } deriving (Eq, Show)

--------------------------------------------------------------------------------

data Value =
    StringValue     Text
  | IntValue        Int
  | DoubleValue     Double
  | BooleanValue    Bool
  | DateValue       Date
  | StructValue     Struct
  | ListValue       List
  | PairValue       Value Value
  | MapValue        [(Value, Value)]
  | Tombstone
  deriving (Eq, Show)

instance PP.Pretty Value where
  pretty v = case v of
    StringValue  t  -> PP.text $ show t
    IntValue     i  -> PP.int i
    DoubleValue  d  -> PP.double d
    BooleanValue b  -> PP.pretty b
    DateValue    d  -> PP.pretty d
    StructValue  s  -> PP.pretty s
    ListValue    l  -> PP.pretty l
    PairValue v1 v2 -> PP.encloseSep PP.lparen PP.rparen PP.comma
                                     [PP.pretty v1, PP.pretty v2]
    MapValue vs     -> PP.pretty vs
    Tombstone       -> PP.text "tombstone"

--------------------------------------------------------------------------------

data Struct =
  Struct    [(Attribute, Value)]
  deriving (Eq, Show)

instance PP.Pretty Struct where
  pretty (Struct avs) = PP.pretty avs

--------------------------------------------------------------------------------

data List =
  List      [Value]
  deriving (Eq, Show)

instance PP.Pretty List where
  pretty (List vs) = PP.pretty vs

--------------------------------------------------------------------------------

data Date =
  Date {
      getDate       :: Text -- FIX complete, make these real...
    } deriving (Eq, Show)

instance PP.Pretty Date where
  pretty (Date t) = PP.text (unpack t)

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


data StructField =
    StructField StructFieldType Attribute Encoding
  deriving (Eq, Show)

attributeOfStructField :: StructField -> Attribute
attributeOfStructField (StructField _ attr _)
  = attr


data StructFieldType =
    Mandatory
  | Optional
  deriving (Eq, Show)
