{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Icicle.Runtime.Data.Striped (
    Column(..)

  , schema

  , arrayCount
  , toArrays
  , fromArrays

  , toLogical
  , fromLogical

  , StripedError(..)
  , renderStripedError
  ) where

import           Anemone.Foreign.Mempool (Mempool)

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.State.Class (get, put)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(..))

import qualified Data.ByteString as ByteString
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Boxed

import           GHC.Generics (Generic)

import           Icicle.Runtime.Array (Array, ArrayError, ArrayLength)
import qualified Icicle.Runtime.Array as Array
import           Icicle.Runtime.Data.Logical (LogicalError)
import qualified Icicle.Runtime.Data.Logical as Logical
import           Icicle.Runtime.Data.Primitive
import           Icicle.Runtime.Data.Schema (Schema)
import qualified Icicle.Runtime.Data.Schema as Schema

import           P hiding (Sum, lefts, rights, empty)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, left, hoistEither)
import           X.Data.Vector.Cons (Cons)
import qualified X.Data.Vector.Cons as Cons
import qualified X.Data.Vector.Storable as Storable

import qualified Zebra.X.Vector.Segment as Segment


data Column =
    Unit !Int
  | Bool !(Storable.Vector Bool64)
  | Int !(Storable.Vector Int64)
  | Double !(Storable.Vector Double)
  | Time !(Storable.Vector Time64)

  | Sum !(Storable.Vector Bool64) !Column !Column
  | Option !(Storable.Vector Bool64) !Column
  | Result !(Storable.Vector Error64) !Column

  | Pair !Column !Column
  | Struct !(Cons Boxed.Vector (Field Column))

  | String !(Storable.Vector Int64) ByteString
  | Array !(Storable.Vector Int64) !Column
  | Map !(Storable.Vector Int64) !Column !Column
    deriving (Eq, Ord, Show, Generic)

data StripedError =
    StripedArrayError !ArrayError
  | StripedArraysExhausted !Schema
  | StripedArraysRemaining !Schema !Int
  | StripedMismatchedMeltedArrays !Schema
  | StripedNestedEmpty
  | StripedNestedLengthMismatch !Schema !Segment.SegmentError
  | StripedLogicalError !LogicalError
    deriving (Eq, Show)

renderStripedError :: StripedError -> Text
renderStripedError = \case
  StripedArrayError x ->
    Array.renderArrayError x

  StripedArraysExhausted x ->
    "The stack of icicle runtime arrays was exhausted while trying to translate: " <> Text.pack (show x)

  StripedArraysRemaining x n ->
    "The stack of icicle runtime arrays had <" <> Text.pack (show n) <> "> leftovers while trying to translate: " <> Text.pack (show x)

  StripedMismatchedMeltedArrays x ->
    "Melted arrays had mismatched lengths for: " <> Text.pack (show x)

  StripedNestedEmpty ->
    "Internal error, nested array had no melted arrays"

  StripedNestedLengthMismatch s x ->
    "Failed to split nested array: " <> Text.pack (show s) <>
    "\n" <> Segment.renderSegmentError x

  StripedLogicalError x ->
    Logical.renderLogicalError x

schema :: Column -> Schema
schema = \case
  Unit _ ->
    Schema.Unit
  Bool _ ->
    Schema.Bool
  Int _ ->
    Schema.Int
  Double _ ->
    Schema.Double
  Time _ ->
    Schema.Time
  Sum _ x y ->
    Schema.Sum (schema x) (schema y)
  Option _ x ->
    Schema.Option (schema x)
  Result _ x ->
    Schema.Result (schema x)
  Pair x y ->
    Schema.Pair (schema x) (schema y)
  Struct fields ->
    Schema.Struct $ fmap (fmap schema) fields
  String _ _ ->
    Schema.String
  Array _ x ->
    Schema.Array (schema x)
  Map _ k v ->
    Schema.Map (schema k) (schema v)

empty :: Schema -> Column
empty = \case
  Schema.Unit ->
    Unit 0
  Schema.Bool ->
    Bool Storable.empty
  Schema.Int ->
    Int Storable.empty
  Schema.Double ->
    Double Storable.empty
  Schema.Time ->
    Time Storable.empty
  Schema.Sum x y ->
    Sum Storable.empty (empty x) (empty y)
  Schema.Option x ->
    Option Storable.empty (empty x)
  Schema.Result x ->
    Result Storable.empty (empty x)
  Schema.Pair x y ->
    Pair (empty x) (empty y)
  Schema.Struct fields ->
    Struct (fmap (fmap empty) fields)
  Schema.String ->
    String Storable.empty ByteString.empty
  Schema.Array x ->
    Array Storable.empty (empty x)
  Schema.Map k v ->
    Map Storable.empty (empty k) (empty v)

(+!) :: Int -> Int -> Int
(+!) !x !y =
  x + y
{-# INLINE (+!) #-}

arrayCount :: Schema -> Int
arrayCount = \case
  Schema.Unit ->
    1
  Schema.Bool ->
    1
  Schema.Int ->
    1
  Schema.Double ->
    1
  Schema.Time ->
    1
  Schema.Sum x y ->
    1 +! arrayCount x +! arrayCount y
  Schema.Option x ->
    1 +! arrayCount x
  Schema.Result x ->
    1 +! arrayCount x
  Schema.Pair x y ->
    arrayCount x +! arrayCount y
  Schema.Struct fields ->
    Boxed.sum $! Cons.toVector $! fmap (arrayCount . fieldData) fields
  Schema.String ->
    1
  Schema.Array x ->
    arrayCount x
  Schema.Map k v ->
    arrayCount k +! arrayCount v

toArrays :: Mempool -> Column -> EitherT StripedError IO [Array]
toArrays pool = \case
  Unit n ->
    liftIO . fmap pure $
      Array.replicate pool (fromIntegral n) unit

  Bool xs ->
    bimapT StripedArrayError pure $
      Array.fromVector pool xs

  Int xs ->
    bimapT StripedArrayError pure $
      Array.fromVector pool xs

  Double xs ->
    bimapT StripedArrayError pure $
      Array.fromVector pool xs

  Time xs ->
    bimapT StripedArrayError pure $
      Array.fromVector pool xs

  Sum isLefts x0 y0 -> do
    (\i x y -> i <> x <> y)
      <$> bimapT StripedArrayError pure (Array.fromVector pool isLefts)
      <*> toArrays pool x0
      <*> toArrays pool y0

  Option isSomes x -> do
    (<>)
      <$> bimapT StripedArrayError pure (Array.fromVector pool isSomes)
      <*> toArrays pool x

  Result isErrors x -> do
    (<>)
      <$> bimapT StripedArrayError pure (Array.fromVector pool isErrors)
      <*> toArrays pool x

  Pair x y ->
    (<>)
      <$> toArrays pool x
      <*> toArrays pool y

  Struct fields ->
    concat <$> traverse (toArrays pool . fieldData) (toList fields)

  String ns bss ->
    bimapT StripedArrayError pure $
      Array.fromStringSegments pool (Array.makeDescriptor ns) bss

  Array ns x -> do
    xss <- toArrays pool x
    traverse (firstT StripedArrayError . Array.fromArraySegments pool (Array.makeDescriptor ns)) xss

  Map ns k v -> do
    xss <- (<>) <$> toArrays pool k <*> toArrays pool v
    traverse (firstT StripedArrayError . Array.fromArraySegments pool (Array.makeDescriptor ns)) xss

take1 :: Schema -> EitherT StripedError (StateT [Array] IO) Array
take1 s = do
  xs0 <- get
  case xs0 of
    [] ->
      left $ StripedArraysExhausted s
    x : xs -> do
      put xs
      pure x

takeN :: Schema -> EitherT StripedError (StateT [Array] IO) [Array]
takeN s = do
  xs0 <- get

  let
    !n =
      arrayCount s

  if n > length xs0 then
    left $ StripedArraysExhausted s
  else do
    let
      (xs1, xs2) =
        splitAt (arrayCount s) xs0

    put xs2
    pure xs1

takeUnit :: EitherT StripedError (StateT [Array] IO) Column
takeUnit = do
  x <- take1 Schema.Unit
  liftIO . fmap (Unit . fromIntegral) $ Array.length x

takeBool :: EitherT StripedError (StateT [Array] IO) Column
takeBool = do
  x <- take1 Schema.Bool
  hoist lift . bimapT StripedArrayError Bool $ Array.toVector x

takeInt :: EitherT StripedError (StateT [Array] IO) Column
takeInt = do
  x <- take1 Schema.Int
  hoist lift . bimapT StripedArrayError Int $ Array.toVector x

takeDouble :: EitherT StripedError (StateT [Array] IO) Column
takeDouble = do
  x <- take1 Schema.Double
  hoist lift . bimapT StripedArrayError Double $ Array.toVector x

takeTime :: EitherT StripedError (StateT [Array] IO) Column
takeTime = do
  x <- take1 Schema.Time
  hoist lift . bimapT StripedArrayError Time $ Array.toVector x

takeBoolTag :: Schema -> EitherT StripedError (StateT [Array] IO) (Storable.Vector Bool64)
takeBoolTag s = do
  x <- take1 s
  hoist lift . firstT StripedArrayError $ Array.toVector x

takeErrorTag :: Schema -> EitherT StripedError (StateT [Array] IO) (Storable.Vector Error64)
takeErrorTag s = do
  x <- take1 s
  hoist lift . firstT StripedArrayError $ Array.toVector x

checkLengths :: Monad m => Schema -> [Storable.Vector ArrayLength] -> EitherT StripedError m ()
checkLengths s = \case
  [] ->
    pure ()
  ns : nss ->
    if all (ns ==) nss then
      pure ()
    else
      left $ StripedMismatchedMeltedArrays s

takeNested :: Mempool -> Schema -> EitherT StripedError (StateT [Array] IO) (Storable.Vector Int64, Column)
takeNested pool s = do
  xss0 <- takeN s

  nxss0 <- liftIO $ traverse (Array.toArraySegments pool) xss0
  checkLengths s $ fmap fst nxss0

  case nxss0 of
    [] ->
      left StripedNestedEmpty

    (ns, xs) : nxss -> do
      column <- hoist lift $ fromArrays pool s (xs : fmap snd nxss)
      pure (Array.takeDescriptor ns, column)

takeColumn :: Mempool -> Schema -> EitherT StripedError (StateT [Array] IO) Column
takeColumn pool s =
  case s of
    Schema.Unit ->
      takeUnit

    Schema.Bool ->
      takeBool

    Schema.Int ->
      takeInt

    Schema.Double ->
      takeDouble

    Schema.Time ->
      takeTime

    Schema.Sum x y ->
      Sum
        <$> takeBoolTag (Schema.Sum x y)
        <*> takeColumn pool x
        <*> takeColumn pool y

    Schema.Option x ->
      Option
        <$> takeBoolTag (Schema.Option x)
        <*> takeColumn pool x

    Schema.Result x ->
      Result
        <$> takeErrorTag (Schema.Result x)
        <*> takeColumn pool x

    Schema.Pair x y ->
      Pair
        <$> takeColumn pool x
        <*> takeColumn pool y

    Schema.Struct fields ->
     Struct
       <$> traverse (traverse (takeColumn pool)) fields

    Schema.String -> do
      x <- take1 Schema.String
      (ns, bs) <- liftIO $ Array.toStringSegments x
      pure $
        String (Array.takeDescriptor ns) bs

    Schema.Array sx -> do
      (ns, x) <- takeNested pool sx
      pure $
        Array ns x

    Schema.Map sk sv -> do
      (nsk, k) <- takeNested pool sk
      (nsv, v) <- takeNested pool sv
      if nsk /= nsv then
        left $ StripedMismatchedMeltedArrays (Schema.Map sk sv)
      else
        pure $
          Map nsk k v

fromArrays :: Mempool -> Schema -> [Array] -> EitherT StripedError IO Column
fromArrays pool s xs0 = do
  (ecolumn, xs) <- liftIO $ runStateT (runEitherT (takeColumn pool s)) xs0
  column <- hoistEither ecolumn
  case xs of
    [] ->
      pure column
    _ ->
      left $ StripedArraysRemaining s (length xs)

toLogical :: Column -> Either StripedError (Boxed.Vector Logical.Value)
toLogical = \case
  Unit n ->
    pure $
      Boxed.replicate n Logical.Unit

  Bool xs ->
    pure .
      fmap Logical.Bool $ Storable.convert xs

  Int xs ->
    pure .
      fmap Logical.Int $ Storable.convert xs

  Double xs ->
    pure .
      fmap Logical.Double $ Storable.convert xs

  Time xs ->
    pure .
      fmap Logical.Time $ Storable.convert xs

  Sum isRights xs ys ->
    Boxed.zipWith3 (\b x y -> if fromBool64 b then Logical.Right y else Logical.Left x)
      <$> pure (Storable.convert isRights)
      <*> toLogical xs
      <*> toLogical ys

  Option isSomes xs -> do
    Boxed.zipWith (\b x -> if fromBool64 b then Logical.Some x else Logical.None)
      <$> pure (Storable.convert isSomes)
      <*> toLogical xs

  Result err xs -> do
    Boxed.zipWith (\e x -> if isError e then Logical.Error e else Logical.Success x)
      <$> pure (Storable.convert err)
      <*> toLogical xs

  Pair xs ys ->
    Boxed.zipWith Logical.Pair
      <$> toLogical xs
      <*> toLogical ys

  Struct fields ->
    fmap Logical.Struct . Cons.transposeCV
      <$> traverse (toLogical . fieldData) fields

  String ns bs -> do
    bss <- first (StripedNestedLengthMismatch . schema $ String ns bs) $ Segment.reify ns bs
    pure $ fmap Logical.String bss

  Array ns c -> do
    xs <- toLogical c
    xss <- first (StripedNestedLengthMismatch . schema $ Array ns c) $ Segment.reify ns xs
    pure $ fmap Logical.Array xss

  Map ns k v -> do
    kvs <- Boxed.zip <$> toLogical k <*> toLogical v
    kvss <- first (StripedNestedLengthMismatch . schema $ Map ns k v) $ Segment.reify ns kvs
    pure $ fmap (Logical.Map . Map.fromList . Boxed.toList) kvss

fromLogical :: Schema -> Boxed.Vector Logical.Value -> Either StripedError Column
fromLogical s values =
  case Cons.fromVector values of
    Nothing ->
      pure $ empty s
    Just values1 ->
      case s of
        Schema.Unit ->
          pure . Unit $ Boxed.length values

        Schema.Bool ->
          Bool . Storable.convert
            <$> first StripedLogicalError (traverse Logical.takeBool values)

        Schema.Int ->
          Int . Storable.convert
            <$> first StripedLogicalError (traverse Logical.takeInt values)

        Schema.Double ->
          Double . Storable.convert
            <$> first StripedLogicalError (traverse Logical.takeDouble values)

        Schema.Time ->
          Time . Storable.convert
            <$> first StripedLogicalError (traverse Logical.takeTime values)

        Schema.Sum x y -> do
          sums <- first StripedLogicalError $ traverse Logical.takeSum values

          let
            tags =
              Storable.convert $ fmap (fromBool . isRight) sums

          lefts <- fromLogical x $ fmap (either id (const $ Logical.defaultValue x)) sums
          rights <- fromLogical y $ fmap (either (const $ Logical.defaultValue y) id) sums

          pure $
            Sum tags lefts rights

        Schema.Option x -> do
          options <- first StripedLogicalError $ traverse Logical.takeOption values

          let
            tags =
              Storable.convert $ fmap (fromBool . isJust) options

          somes <- fromLogical x $ fmap (fromMaybe (Logical.defaultValue x)) options

          pure $
            Option tags somes

        Schema.Result x -> do
          results <- first StripedLogicalError $ traverse Logical.takeResult values

          let
            errors =
              Storable.convert $ fmap (either id (const NotAnError64)) results

          successes <- fromLogical x $ fmap (either (const $ Logical.defaultValue x) id) results

          pure $
            Result errors successes

        Schema.Pair x y -> do
          (xs, ys) <- Boxed.unzip <$> first StripedLogicalError (traverse Logical.takePair values)
          Pair
            <$> fromLogical x xs
            <*> fromLogical y ys

        Schema.Struct fields -> do
          xss <- Cons.transpose <$> first StripedLogicalError (traverse Logical.takeStruct values1)
          Struct
            <$> Cons.zipWithM fromField fields (fmap Cons.toVector xss)

        Schema.String -> do
          bss <- first StripedLogicalError $ traverse Logical.takeString values
          pure $
            String
              (Storable.convert $ fmap (fromIntegral . ByteString.length) bss)
              (ByteString.concat $ Boxed.toList bss)

        Schema.Array sx -> do
          xss <- first StripedLogicalError $ traverse Logical.takeArray values
          column <- fromLogical sx . Boxed.concat $ Boxed.toList xss

          let
            ns =
              Storable.convert $ fmap (fromIntegral . Boxed.length) xss

          pure $
            Array ns column

        Schema.Map sk sv -> do
          kvss <- first StripedLogicalError $ traverse Logical.takeMap values

          let
            ns =
              Storable.convert $ fmap (fromIntegral . Map.size) kvss

            (ks0, vs0) =
              Boxed.unzip $ Boxed.concatMap (Boxed.fromList . Map.toList) kvss

          ks <- fromLogical sk ks0
          vs <- fromLogical sv vs0

          pure $
            Map ns ks vs

fromField :: Field Schema -> Boxed.Vector Logical.Value -> Either StripedError (Field Column)
fromField field =
  fmap (field $>) .
  fromLogical (fieldData field)
{-# INLINABLE fromField #-}
