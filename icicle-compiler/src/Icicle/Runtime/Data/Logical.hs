{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Icicle.Runtime.Data.Logical (
    Value(..)
  , defaultValue

  , takeUnit
  , takeBool
  , takeInt
  , takeDouble
  , takeTime
  , takeSum
  , takeOption
  , takeResult
  , takePair
  , takeStruct
  , takeString
  , takeArray
  , takeMap

  , LogicalError(..)
  , renderLogicalError
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed

import           GHC.Generics (Generic)

import           Icicle.Runtime.Data.Primitive
import           Icicle.Runtime.Data.Schema (Schema)
import qualified Icicle.Runtime.Data.Schema as Schema

import           P hiding (Left, Right)

import           X.Data.Vector.Cons (Cons)


data Value =
    Unit
  | Bool !Bool64
  | Int !Int64
  | Double !Double
  | Time !Time64

  | Left !Value
  | Right !Value

  | None
  | Some !Value

  | Error !Error64
  | Success !Value

  | Pair !Value !Value
  | Struct !(Cons Boxed.Vector Value)

  | String !ByteString
  | Array !(Boxed.Vector Value)
  | Map !(Map Value Value)
    deriving (Eq, Ord, Show, Generic)

data LogicalError =
    LogicalExpectedUnit !Value
  | LogicalExpectedBool !Value
  | LogicalExpectedInt !Value
  | LogicalExpectedDouble !Value
  | LogicalExpectedTime !Value
  | LogicalExpectedSum !Value
  | LogicalExpectedOption !Value
  | LogicalExpectedResult !Value
  | LogicalExpectedPair !Value
  | LogicalExpectedStruct !Value
  | LogicalExpectedString !Value
  | LogicalExpectedArray !Value
  | LogicalExpectedMap !Value
    deriving (Eq, Ord, Show)

renderLogicalError :: LogicalError -> Text
renderLogicalError = \case
  LogicalExpectedUnit x ->
    "Expected unit, but was: " <> ppSchema x
  LogicalExpectedBool x ->
    "Expected bool, but was: " <> ppSchema x
  LogicalExpectedInt x ->
    "Expected int, but was: " <> ppSchema x
  LogicalExpectedDouble x ->
    "Expected double, but was: " <> ppSchema x
  LogicalExpectedTime x ->
    "Expected time, but was: " <> ppSchema x
  LogicalExpectedSum x ->
    "Expected sum, but was: " <> ppSchema x
  LogicalExpectedOption x ->
    "Expected option, but was: " <> ppSchema x
  LogicalExpectedResult x ->
    "Expected result, but was: " <> ppSchema x
  LogicalExpectedPair x ->
    "Expected pair, but was: " <> ppSchema x
  LogicalExpectedStruct x ->
    "Expected struct, but was: " <> ppSchema x
  LogicalExpectedString x ->
    "Expected string, but was: " <> ppSchema x
  LogicalExpectedArray x ->
    "Expected array, but was: " <> ppSchema x
  LogicalExpectedMap x ->
    "Expected map, but was: " <> ppSchema x

ppSchema :: Value -> Text
ppSchema = \case
  Unit ->
    "unit"
  Bool _ ->
    "bool"
  Int _ ->
    "int"
  Double _ ->
    "double"
  Time _ ->
    "time"
  Left _ ->
    "sum"
  Right _ ->
    "sum"
  None ->
    "option"
  Some _ ->
    "option"
  Error _ ->
    "result"
  Success _ ->
    "result"
  Pair _ _ ->
    "pair"
  Struct _ ->
    "struct"
  String _ ->
    "string"
  Array _ ->
    "array"
  Map _ ->
    "map"

defaultValue :: Schema -> Value
defaultValue = \case
  Schema.Unit ->
    Unit
  Schema.Bool ->
    Bool False64
  Schema.Int ->
    Int 0
  Schema.Double ->
    Double 0
  Schema.Time ->
    Time (Time64 0)
  Schema.Sum x _ ->
    Left $ defaultValue x
  Schema.Option _ ->
    None
  Schema.Result _ ->
    Error Tombstone64
  Schema.Pair x y ->
    Pair (defaultValue x) (defaultValue y)
  Schema.Struct fs ->
    Struct $ fmap (defaultValue . fieldData) fs
  Schema.String ->
    String ByteString.empty
  Schema.Array _ ->
    Array Boxed.empty
  Schema.Map _ _ ->
    Map Map.empty
{-# INLINABLE defaultValue #-}

takeUnit :: Value -> Either LogicalError ()
takeUnit = \case
  Unit ->
    Either.Right ()
  x ->
    Either.Left $ LogicalExpectedUnit x
{-# INLINE takeUnit #-}

takeBool :: Value -> Either LogicalError Bool64
takeBool = \case
  Bool x ->
    Either.Right x
  x ->
    Either.Left $ LogicalExpectedBool x
{-# INLINE takeBool #-}

takeInt :: Value -> Either LogicalError Int64
takeInt = \case
  Int x ->
    Either.Right x
  x ->
    Either.Left $ LogicalExpectedInt x
{-# INLINE takeInt #-}

takeDouble :: Value -> Either LogicalError Double
takeDouble = \case
  Double x ->
    Either.Right x
  x ->
    Either.Left $ LogicalExpectedDouble x
{-# INLINE takeDouble #-}

takeTime :: Value -> Either LogicalError Time64
takeTime = \case
  Time x ->
    Either.Right x
  x ->
    Either.Left $ LogicalExpectedTime x
{-# INLINE takeTime #-}

takeSum :: Value -> Either LogicalError (Either Value Value)
takeSum = \case
  Left x ->
    Either.Right (Either.Left x)
  Right x ->
    Either.Right (Either.Right x)
  x ->
    Either.Left $ LogicalExpectedSum x
{-# INLINE takeSum #-}

takeOption :: Value -> Either LogicalError (Maybe Value)
takeOption = \case
  None ->
    Either.Right Nothing
  Some x ->
    Either.Right (Just x)
  x ->
    Either.Left $ LogicalExpectedOption x
{-# INLINE takeOption #-}

takeResult :: Value -> Either LogicalError (Either Error64 Value)
takeResult = \case
  Error x ->
    Either.Right (Either.Left x)
  Success x ->
    Either.Right (Either.Right x)
  x ->
    Either.Left $ LogicalExpectedResult x
{-# INLINE takeResult #-}

takePair :: Value -> Either LogicalError (Value, Value)
takePair = \case
  Pair x y ->
    Either.Right (x, y)
  x ->
    Either.Left $ LogicalExpectedPair x
{-# INLINE takePair #-}

takeStruct :: Value -> Either LogicalError (Cons Boxed.Vector Value)
takeStruct = \case
  Struct xs ->
    Either.Right xs
  x ->
    Either.Left $ LogicalExpectedStruct x
{-# INLINE takeStruct #-}

takeString :: Value -> Either LogicalError ByteString
takeString = \case
  String xs ->
    Either.Right xs
  x ->
    Either.Left $ LogicalExpectedString x
{-# INLINE takeString #-}

takeArray :: Value -> Either LogicalError (Boxed.Vector Value)
takeArray = \case
  Array xs ->
    Either.Right xs
  x ->
    Either.Left $ LogicalExpectedArray x
{-# INLINE takeArray #-}

takeMap :: Value -> Either LogicalError (Map Value Value)
takeMap = \case
  Map xs ->
    Either.Right xs
  x ->
    Either.Left $ LogicalExpectedMap x
{-# INLINE takeMap #-}
