{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Icicle.Runtime.Data.Logical (
    Value(..)
  , defaultValue

  , toBaseValue
  , fromBaseValue
  , fromTopValue

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

  , prettyValue

  , LogicalError(..)
  , renderLogicalError
  ) where

import qualified Anemone.Pretty as Anemone

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Either as Either
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Boxed

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import           Icicle.Common.Type
import qualified Icicle.Data.Time as Icicle
import           Icicle.Internal.Pretty
import           Icicle.Runtime.Data.Primitive
import           Icicle.Runtime.Data.Schema (Schema)
import qualified Icicle.Runtime.Data.Schema as Schema

import           P hiding (Left, Right)

import           X.Data.Vector.Cons (Cons)
import qualified X.Data.Vector.Cons as Cons


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
  | LogicalSchemaMismatch !Schema !Value
  | LogicalCannotConvertFromBaseValue !ValType !BaseValue
  | LogicalCannotConvertToBaseValue !ValType !Value
    deriving (Eq, Ord, Show)

renderLogicalError :: LogicalError -> Text
renderLogicalError = \case
  LogicalExpectedUnit x ->
    "Expected unit, but was: " <> ppValueSchema x
  LogicalExpectedBool x ->
    "Expected bool, but was: " <> ppValueSchema x
  LogicalExpectedInt x ->
    "Expected int, but was: " <> ppValueSchema x
  LogicalExpectedDouble x ->
    "Expected double, but was: " <> ppValueSchema x
  LogicalExpectedTime x ->
    "Expected time, but was: " <> ppValueSchema x
  LogicalExpectedSum x ->
    "Expected sum, but was: " <> ppValueSchema x
  LogicalExpectedOption x ->
    "Expected option, but was: " <> ppValueSchema x
  LogicalExpectedResult x ->
    "Expected result, but was: " <> ppValueSchema x
  LogicalExpectedPair x ->
    "Expected pair, but was: " <> ppValueSchema x
  LogicalExpectedStruct x ->
    "Expected struct, but was: " <> ppValueSchema x
  LogicalExpectedString x ->
    "Expected string, but was: " <> ppValueSchema x
  LogicalExpectedArray x ->
    "Expected array, but was: " <> ppValueSchema x
  LogicalExpectedMap x ->
    "Expected map, but was: " <> ppValueSchema x
  LogicalSchemaMismatch s v ->
    "Expected " <> Text.pack (show s) <> ", but was: " <> Text.pack (show v)
  LogicalCannotConvertFromBaseValue t v ->
    "Cannot convert BaseValue to Logical.Value:" <>
    "\n" <>
    "\n type =" <>
    "\n   " <> Text.pack (show t) <>
    "\n" <>
    "\n value =" <>
    "\n   " <> Text.pack (show v)
  LogicalCannotConvertToBaseValue t v ->
    "Cannot convert Logical.Value to BaseValue:" <>
    "\n" <>
    "\n type =" <>
    "\n   " <> Text.pack (show t) <>
    "\n" <>
    "\n value =" <>
    "\n   " <> Text.pack (show v)

ppValueSchema :: Value -> Text
ppValueSchema = \case
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

prettyValue :: Int -> Schema -> Value -> Either LogicalError Doc
prettyValue p schema = \case
  Unit
    | Schema.Unit <- schema
    ->
      pure $
        prettyPunctuation "()"

  Bool (Bool64 0)
    | Schema.Bool <- schema
    ->
      pure $
        annotate AnnConstant "False"

  Bool (Bool64 _)
    | Schema.Bool <- schema
    ->
      pure $
        annotate AnnConstant "True"

  Int x
    | Schema.Int <- schema
    ->
      pure $
        annotate AnnConstant $ pretty x

  Double x
    | Schema.Double <- schema
    ->
      pure $
        annotate AnnConstant . pretty . Char8.unpack $ Anemone.renderDouble x

  Time x
    | Schema.Time <- schema
    ->
      pure $
        annotate AnnConstant . prettyText $ renderTime x

  Left x
    | Schema.Sum s _ <- schema
    -> do
      px <- prettyValue appPrec1 s x
      pure . parensWhenArg p $
        prettyConstructor "Left" <+> px

  Right x
    | Schema.Sum _ s <- schema
    -> do
      px <- prettyValue appPrec1 s x
      pure . parensWhenArg p $
        prettyConstructor "Right" <+> px

  None
    | Schema.Option _ <- schema
    ->
      pure $
        prettyConstructor "None"

  Some x
    | Schema.Option s <- schema
    -> do
      px <- prettyValue appPrec1 s x
      pure . parensWhenArg p $
        prettyConstructor "Some" <+> px

  Error Tombstone64
    | Schema.Result _ <- schema
    ->
      pure $
        annotate AnnError "Tombstone"

  Error Fold1NoValue64
    | Schema.Result _ <- schema
    ->
      pure $
        annotate AnnError "Fold1NoValue"

  Error CannotCompute64
    | Schema.Result _ <- schema
    ->
      pure $
        annotate AnnError "CannotCompute"

  Error (Error64 x)
    | Schema.Result _ <- schema
    ->
      pure $
        annotate AnnError ("Error" <> text (show x))

  Success x
    | Schema.Result s <- schema
    ->
      prettyValue p s x

  Pair x y
    | Schema.Pair sx sy <- schema
    -> do
      px <- prettyValue 0 sx x
      py <- prettyValue 0 sy y
      pure $
        prettyPunctuation "(" <> px <> prettyPunctuation "," <+> py <> prettyPunctuation ")"

  Struct xs0
    | Schema.Struct sxs0 <- schema
    , Cons.length xs0 == Cons.length sxs0
    -> do
      let
        xs =
          Cons.toList xs0

        sxs =
          Cons.toList sxs0

        takeField (Field name s) x = do
          px <- prettyValue 0 s x
          pure (pretty name, px)

        ppField n x =
          n <+> prettyPunctuation "=" <+> x

      pxs <- zipWithM takeField sxs xs

      pure $
        prettyStruct ppField hcat pxs

  String x
    | Schema.String <- schema
    ->
      pure .
        annotate AnnConstant $ pretty (show x)

  Array xs
    | Schema.Array s <- schema
    -> do
      pxs <- traverse (prettyValue 0 s) (Boxed.toList xs)
      pure $
        prettySep hcat (prettyPunctuation "[") (prettyPunctuation "]") pxs

  Map kvs
    | Schema.Map sk sv <- schema
    -> do
      pkvs <-
        for (Map.toList kvs) $ \(k, v) ->
          (,) <$> prettyValue 0 sk k <*> prettyValue 0 sv v

      let
        ppPair k v =
          k <+> prettyPunctuation "->" <+> v

      pure $
        prettyStruct ppPair hcat pkvs

  value ->
    Either.Left $ LogicalSchemaMismatch schema value

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
{-# INLINABLE takeUnit #-}

takeBool :: Value -> Either LogicalError Bool64
takeBool = \case
  Bool x ->
    Either.Right x
  x ->
    Either.Left $ LogicalExpectedBool x
{-# INLINABLE takeBool #-}

takeInt :: Value -> Either LogicalError Int64
takeInt = \case
  Int x ->
    Either.Right x
  x ->
    Either.Left $ LogicalExpectedInt x
{-# INLINABLE takeInt #-}

takeDouble :: Value -> Either LogicalError Double
takeDouble = \case
  Double x ->
    Either.Right x
  x ->
    Either.Left $ LogicalExpectedDouble x
{-# INLINABLE takeDouble #-}

takeTime :: Value -> Either LogicalError Time64
takeTime = \case
  Time x ->
    Either.Right x
  x ->
    Either.Left $ LogicalExpectedTime x
{-# INLINABLE takeTime #-}

takeSum :: Value -> Either LogicalError (Either Value Value)
takeSum = \case
  Left x ->
    Either.Right (Either.Left x)
  Right x ->
    Either.Right (Either.Right x)
  x ->
    Either.Left $ LogicalExpectedSum x
{-# INLINABLE takeSum #-}

takeOption :: Value -> Either LogicalError (Maybe Value)
takeOption = \case
  None ->
    Either.Right Nothing
  Some x ->
    Either.Right (Just x)
  x ->
    Either.Left $ LogicalExpectedOption x
{-# INLINABLE takeOption #-}

takeResult :: Value -> Either LogicalError (Either Error64 Value)
takeResult = \case
  Error x ->
    Either.Right (Either.Left x)
  Success x ->
    Either.Right (Either.Right x)
  x ->
    Either.Left $ LogicalExpectedResult x
{-# INLINABLE takeResult #-}

takePair :: Value -> Either LogicalError (Value, Value)
takePair = \case
  Pair x y ->
    Either.Right (x, y)
  x ->
    Either.Left $ LogicalExpectedPair x
{-# INLINABLE takePair #-}

takeStruct :: Value -> Either LogicalError (Cons Boxed.Vector Value)
takeStruct = \case
  Struct xs ->
    Either.Right xs
  x ->
    Either.Left $ LogicalExpectedStruct x
{-# INLINABLE takeStruct #-}

takeString :: Value -> Either LogicalError ByteString
takeString = \case
  String xs ->
    Either.Right xs
  x ->
    Either.Left $ LogicalExpectedString x
{-# INLINABLE takeString #-}

takeArray :: Value -> Either LogicalError (Boxed.Vector Value)
takeArray = \case
  Array xs ->
    Either.Right xs
  x ->
    Either.Left $ LogicalExpectedArray x
{-# INLINABLE takeArray #-}

takeMap :: Value -> Either LogicalError (Map Value Value)
takeMap = \case
  Map xs ->
    Either.Right xs
  x ->
    Either.Left $ LogicalExpectedMap x
{-# INLINABLE takeMap #-}

------------------------------------------------------------------------

toBaseValue :: ValType -> Value -> Either LogicalError BaseValue
toBaseValue typ = \case
  Unit
    | UnitT <- typ
    ->
      pure VUnit

  Bool False64
    | BoolT <- typ
    ->
      pure $ VBool False

  Bool _
    | BoolT <- typ
    ->
      pure $ VBool True

  Int x
    | IntT <- typ
    ->
      pure $ VInt (fromIntegral x)

  Double x
    | DoubleT <- typ
    ->
      pure $ VDouble x

  Time x
    | TimeT <- typ
    ->
      pure . VTime . Icicle.timeOfPacked $ unTime64 x

  String x
    | StringT <- typ
    ->
      pure . VString $ Text.decodeUtf8 x

  Left x
    | SumT xt _ <- typ
    ->
      VLeft <$> toBaseValue xt x

  Right x
    | SumT _ xt <- typ
    ->
      VRight <$> toBaseValue xt x

  None
    | OptionT _ <- typ
    ->
      pure VNone

  Some x
    | OptionT xt <- typ
    ->
      VSome <$> toBaseValue xt x

  Error x
    | SumT ErrorT _ <- typ
    ->
      pure . VLeft . VError . fromMaybe ExceptTombstone $ fromError64 x

  Success x
    | SumT ErrorT xt <- typ
    ->
      VRight <$> toBaseValue xt x

  Pair x y
    | PairT xt yt <- typ
    ->
      VPair <$> toBaseValue xt x <*> toBaseValue yt y

  Struct xs0
    | StructT (StructType fs) <- typ
    , Map.size fs == length xs0
    -> do
      let
        go (name, xt) x = do
          (name,) <$> toBaseValue xt x

      kvs <- zipWithM go (Map.toList fs) (Cons.toList xs0)

      pure . VStruct $ Map.fromList kvs

  Array xs
    | ArrayT xt <- typ
    ->
      VArray <$> traverse (toBaseValue xt) (Boxed.toList xs)

  Map kvs
    | MapT kt vt <- typ
    ->
      VMap . Map.fromList <$> traverse (bitraverse (toBaseValue kt) (toBaseValue vt)) (Map.toList kvs)

  x ->
    Either.Left $ LogicalCannotConvertToBaseValue typ x

fromBaseValue :: ValType -> BaseValue -> Either LogicalError Value
fromBaseValue typ = \case
  VUnit
    | UnitT <- typ
    ->
      pure Unit

  VBool False
    | BoolT <- typ
    ->
      pure $ Bool False64

  VBool True
    | BoolT <- typ
    ->
      pure $ Bool True64

  VInt x
    | IntT <- typ
    ->
      pure . Int $ fromIntegral x

  VDouble x
    | DoubleT <- typ
    ->
      pure $ Double x

  VTime x
    | TimeT <- typ
    ->
      pure . Time . Time64 $ Icicle.packedOfTime x

  VString x
    | StringT <- typ
    ->
      pure . String $ Text.encodeUtf8 x

  VArray xs
    | ArrayT t <- typ
    ->
      Array . Boxed.fromList <$> mapM (fromBaseValue t) xs

  VPair a b
    | PairT ta tb <- typ
    ->
      Pair
        <$> fromBaseValue ta a
        <*> fromBaseValue tb b

  VLeft x
    | SumT t _ <- typ
    ->
      Left <$> fromBaseValue t x

  VRight x
    | SumT _ t <- typ
    ->
      Right <$> fromBaseValue t x

  VNone
    | OptionT _ <- typ
    ->
      pure None

  VSome x
    | OptionT t <- typ
    ->
      Some <$> fromBaseValue t x

  VMap x
    | MapT tk tv <- typ
    , ks <- Map.keys x
    , vs <- Map.elems x
    -> do ks' <- mapM (fromBaseValue tk) ks
          vs' <- mapM (fromBaseValue tv) vs
          pure . Map . Map.fromList $ List.zip ks' vs'

  VStruct xs
    | StructT struct <- typ
    , types <- getStructType struct
    , (v:vs) <- Map.elems (Map.intersectionWith (,) types xs)
    -> do v' <- uncurry fromBaseValue $ v
          vs' <- mapM (uncurry fromBaseValue) vs
          pure . Struct . Cons.fromNonEmpty $ v' :| vs'

  VBuf xs
    | BufT _ t <- typ
    ->
      Array . Boxed.fromList <$> mapM (fromBaseValue t) xs

  VError e
    | ErrorT <- typ
    ->
      pure . Error $ fromExceptionInfo e

  x ->
    Either.Left $ LogicalCannotConvertFromBaseValue typ x

fromTopValue :: ValType -> BaseValue -> Either LogicalError (Error64, Value)
fromTopValue t = \case
  VRight v ->
    (NotAnError64,) <$> fromBaseValue t v

  VLeft (VError ExceptTombstone) ->
    (Tombstone64,) <$> fromBaseValue t (defaultOfType t)

  x ->
    Either.Left $ LogicalCannotConvertFromBaseValue t x
