{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Runtime.Data.Schema (
    Schema(..)

  , fromValType
  , toValType

  , SchemaError(..)
  , renderSchemaError
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed

import           GHC.Generics (Generic)

import           Icicle.Common.Type
import           Icicle.Runtime.Data.Primitive

import           P hiding (Sum)

import           X.Data.Vector.Cons (Cons)
import qualified X.Data.Vector.Cons as Cons


data Schema =
    Unit
  | Bool
  | Int
  | Double
  | Time

  | Sum !Schema !Schema
  | Option !Schema
  | Result !Schema

  | Pair !Schema !Schema
  | Struct !(Cons Boxed.Vector (Field Schema))

  | String
  | Array !Schema
  | Map !Schema !Schema
    deriving (Eq, Ord, Show, Generic)

data SchemaError =
    SchemaFoundEmptyStruct
  | SchemaFoundStandloneError
  | SchemaFoundFactIdentifier
  | SchemaFoundBuf !Int !ValType
    deriving (Eq, Show)

renderSchemaError :: SchemaError -> Text
renderSchemaError = \case
  SchemaFoundEmptyStruct ->
    "Found illegal empty struct when converting ValType to Schema"
  SchemaFoundStandloneError ->
    "Found illegal standlone error when converting ValType to Schema"
  SchemaFoundFactIdentifier ->
    "Found illegal fact identifier error when converting ValType to Schema"
  SchemaFoundBuf _n _t ->
    "Found illegal buffer error when converting ValType to Schema"

fromValType :: ValType -> Either SchemaError Schema
fromValType = \case
  UnitT ->
    pure Unit
  BoolT ->
    pure Bool
  IntT ->
    pure Int
  DoubleT ->
    pure Double
  TimeT ->
    pure Time

  SumT ErrorT y ->
    Result <$> fromValType y
  SumT x y ->
    Sum <$> fromValType x <*> fromValType y
  OptionT x ->
    Option <$> fromValType x

  PairT x y ->
    Pair <$> fromValType x <*> fromValType y
  StructT (StructType fields) ->
    case Cons.fromList $ Map.toList fields of
      Nothing ->
        Left SchemaFoundEmptyStruct
      Just kvs ->
        Struct <$> traverse (\(StructField k, v) -> Field k <$> fromValType v) kvs

  StringT ->
    pure $ String
  ArrayT x ->
    Array <$> fromValType x
  MapT k v ->
    Map <$> fromValType k <*> fromValType v

  ErrorT ->
    Left SchemaFoundStandloneError

  FactIdentifierT ->
    Left SchemaFoundFactIdentifier

  BufT n x ->
    Left $ SchemaFoundBuf n x

toValType :: Schema -> ValType
toValType = \case
  Unit ->
    UnitT
  Bool ->
    BoolT
  Int ->
    IntT
  Double ->
    DoubleT
  Time ->
    TimeT

  Sum x y ->
    SumT (toValType x) (toValType y)
  Option x ->
    OptionT (toValType x)
  Result x ->
    SumT ErrorT (toValType x)

  Pair x y ->
    PairT (toValType x) (toValType y)
  Struct fields ->
    StructT . StructType . Map.fromList . fmap (\(Field k v) -> (StructField k, toValType v)) $ Cons.toList fields

  String ->
    StringT
  Array x ->
    ArrayT (toValType x)
  Map k v ->
    MapT (toValType k) (toValType v)
