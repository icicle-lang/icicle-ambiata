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

  , empty
  , schema
  , length
  , unsafeConcat
  , unsafeAppend
  , splitAt

  , meltedCount
  , toArrays
  , headAnys
  , fromArrays
  , fromAnys

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
import           Data.Biapplicative (biliftA2, biliftA3)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Boxed

import           Foreign.Storable (Storable)

import           GHC.Generics (Generic)

import           Icicle.Runtime.Data.Any (Any64, AnyError)
import qualified Icicle.Runtime.Data.Any as Any
import           Icicle.Runtime.Data.Array (Array, ArrayError)
import qualified Icicle.Runtime.Data.Array as Array
import           Icicle.Runtime.Data.Logical (LogicalError)
import qualified Icicle.Runtime.Data.Logical as Logical
import           Icicle.Runtime.Data.Primitive
import           Icicle.Runtime.Data.Schema (Schema)
import qualified Icicle.Runtime.Data.Schema as Schema

import           P hiding (Sum, lefts, rights, empty, length, splitAt)

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
  | StripedAnyError !AnyError
  | StripedHeadEmptyColumn !Schema
  | StripedMeltedExhausted !Schema
  | StripedMeltedRemaining !Schema !Int
  | StripedMismatchedMeltedArrays !Schema
  | StripedNestedEmpty
  | StripedNestedLengthMismatch !Schema !Segment.SegmentError
  | StripedLogicalError !LogicalError
  | StripedAppendColumnMismatch !Schema !Schema
  | StripedAppendFieldMismatch !(Field Schema) !(Field Schema)
  | StripedConcatEmptyVector
    deriving (Eq, Show)

renderStripedError :: StripedError -> Text
renderStripedError = \case
  StripedArrayError x ->
    Array.renderArrayError x

  StripedAnyError x ->
    Any.renderAnyError x

  StripedHeadEmptyColumn x ->
    "Tried to take head of empty column: " <> Text.pack (show x)

  StripedMeltedExhausted x ->
    "The stack of melted icicle runtime arrays/values was exhausted while trying to translate: " <> Text.pack (show x)

  StripedMeltedRemaining x n ->
    "The stack of melted icicle runtime arrays/values had <" <> Text.pack (show n) <> "> leftovers while trying to translate: " <> Text.pack (show x)

  StripedMismatchedMeltedArrays x ->
    "Melted arrays had mismatched lengths for: " <> Text.pack (show x)

  StripedNestedEmpty ->
    "Internal error, nested array had no melted arrays"

  StripedNestedLengthMismatch s x ->
    "Failed to split nested array: " <> Text.pack (show s) <>
    "\n" <> Segment.renderSegmentError x

  StripedLogicalError x ->
    Logical.renderLogicalError x

  StripedAppendColumnMismatch x y ->
    "Cannot append columns with different schemas:" <>
    "\n  first = " <>
    "\n    " <> Text.pack (show x) <>
    "\n  second = " <>
    "\n    " <> Text.pack (show y)

  StripedAppendFieldMismatch x y ->
    "Cannot append fields with different schemas:" <>
    "\n  first = " <>
    "\n    " <> Text.pack (show x) <>
    "\n  second = " <>
    "\n    " <> Text.pack (show y)

  StripedConcatEmptyVector ->
    "Internal error, tried to concat empty vector"

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

length :: Column -> Int
length = \case
  Unit n ->
    n
  Bool xs ->
    Storable.length xs
  Int xs ->
    Storable.length xs
  Double xs ->
    Storable.length xs
  Time xs ->
    Storable.length xs
  Sum xs _ _ ->
    Storable.length xs
  Option xs _ ->
    Storable.length xs
  Result xs _ ->
    Storable.length xs
  Pair xs _ ->
    length xs
  Struct fields ->
    length . fieldData $ Cons.head fields
  String xs _ ->
    Storable.length xs
  Array xs _ ->
    Storable.length xs
  Map xs _ _ ->
    Storable.length xs

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

meltedCount :: Schema -> Int
meltedCount = \case
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
    1 +! meltedCount x +! meltedCount y
  Schema.Option x ->
    1 +! meltedCount x
  Schema.Result x ->
    1 +! meltedCount x
  Schema.Pair x y ->
    meltedCount x +! meltedCount y
  Schema.Struct fields ->
    Boxed.sum $! Cons.toVector $! fmap (meltedCount . fieldData) fields
  Schema.String ->
    1
  Schema.Array x ->
    meltedCount x
  Schema.Map k v ->
    meltedCount k +! meltedCount v

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

  String ns bss -> do
    bimapT StripedArrayError pure $
      Array.fromStringSegments pool (Array.makeDescriptor ns) bss

  Array ns x -> do
    xss <- toArrays pool x
    traverse (firstT StripedArrayError . Array.fromArraySegments pool (Array.makeDescriptor ns)) xss

  Map ns k v -> do
    xss <- (<>) <$> toArrays pool k <*> toArrays pool v
    traverse (firstT StripedArrayError . Array.fromArraySegments pool (Array.makeDescriptor ns)) xss

headValue :: Storable a => Schema -> Storable.Vector a -> EitherT StripedError IO a
headValue s xs =
  if Storable.null xs then
    left $ StripedHeadEmptyColumn s
  else
    pure $! Storable.unsafeHead xs

headAny :: Storable a => Schema -> Storable.Vector a -> EitherT StripedError IO [Any64]
headAny s xs = do
  x <- headValue s xs
  hoistEither . bimap StripedAnyError pure $ Any.from x

headAnys :: Mempool -> Column -> EitherT StripedError IO [Any64]
headAnys pool = \case
  Unit _ ->
    hoistEither . bimap StripedAnyError pure $
      Any.from unit

  Bool xs ->
    headAny Schema.Bool xs

  Int xs ->
    headAny Schema.Int xs

  Double xs ->
    headAny Schema.Double xs

  Time xs ->
    headAny Schema.Time xs

  Sum isLefts x0 y0 -> do
    (\i x y -> i <> x <> y)
      <$> headAny (schema $ Sum isLefts x0 y0) isLefts
      <*> headAnys pool x0
      <*> headAnys pool y0

  Option isSomes x -> do
    (<>)
      <$> headAny (schema $ Option isSomes x) isSomes
      <*> headAnys pool x

  Result isErrors x -> do
    (<>)
      <$> headAny (schema $ Result isErrors x) isErrors
      <*> headAnys pool x

  Pair x y ->
    (<>)
      <$> headAnys pool x
      <*> headAnys pool y

  Struct fields ->
    concat <$> traverse (headAnys pool . fieldData) (toList fields)

  String ns bss -> do
    n <- headValue Schema.String ns
    fmap pure . liftIO . Any.fromString pool $ ByteString.take (fromIntegral n) bss

  Array ns x -> do
    n <- headValue (Schema.Array $ schema x) ns
    xss0 <- toArrays pool x -- FIXME this could be more efficient
    xss <- liftIO $ traverse (\array -> Array.grow pool array (fromIntegral n)) xss0
    pure $
      fmap Any.fromArray xss

  Map ns k v -> do
    n <- headValue (Schema.Map (schema k) (schema v)) ns
    xss0 <- (<>) <$> toArrays pool k <*> toArrays pool v -- FIXME this could be more efficient
    xss <- liftIO $ traverse (\array -> Array.grow pool array (fromIntegral n)) xss0
    pure $
      fmap Any.fromArray xss

take1 :: Schema -> EitherT StripedError (StateT [a] IO) a
take1 s = do
  xs0 <- get
  case xs0 of
    [] ->
      left $ StripedMeltedExhausted s
    x : xs -> do
      put xs
      pure x

takeN :: Schema -> EitherT StripedError (StateT [a] IO) [a]
takeN s = do
  xs0 <- get

  let
    !n =
      meltedCount s

  if n > List.length xs0 then
    left $ StripedMeltedExhausted s
  else do
    let
      (xs1, xs2) =
        List.splitAt (meltedCount s) xs0

    put xs2
    pure xs1

takeArrayUnit :: EitherT StripedError (StateT [Array] IO) Column
takeArrayUnit = do
  x <- take1 Schema.Unit
  liftIO . fmap (Unit . fromIntegral) $ Array.length x

takeAnyUnit :: EitherT StripedError (StateT [Any64] IO) Column
takeAnyUnit = do
  _ <- take1 Schema.Unit
  pure $ Unit 1

takeArrayBool :: EitherT StripedError (StateT [Array] IO) Column
takeArrayBool = do
  x <- take1 Schema.Bool
  hoist lift . bimapT StripedArrayError Bool $ Array.toVector x

takeAnyBool :: EitherT StripedError (StateT [Any64] IO) Column
takeAnyBool = do
  x <- take1 Schema.Bool
  hoistEither . bimap StripedAnyError (Bool . Storable.singleton) $ Any.read x

takeArrayInt :: EitherT StripedError (StateT [Array] IO) Column
takeArrayInt = do
  x <- take1 Schema.Int
  hoist lift . bimapT StripedArrayError Int $ Array.toVector x

takeAnyInt :: EitherT StripedError (StateT [Any64] IO) Column
takeAnyInt = do
  x <- take1 Schema.Int
  hoistEither . bimap StripedAnyError (Int . Storable.singleton) $ Any.read x

takeArrayDouble :: EitherT StripedError (StateT [Array] IO) Column
takeArrayDouble = do
  x <- take1 Schema.Double
  hoist lift . bimapT StripedArrayError Double $ Array.toVector x

takeAnyDouble :: EitherT StripedError (StateT [Any64] IO) Column
takeAnyDouble = do
  x <- take1 Schema.Double
  hoistEither . bimap StripedAnyError (Double . Storable.singleton) $ Any.read x

takeArrayTime :: EitherT StripedError (StateT [Array] IO) Column
takeArrayTime = do
  x <- take1 Schema.Time
  hoist lift . bimapT StripedArrayError Time $ Array.toVector x

takeAnyTime :: EitherT StripedError (StateT [Any64] IO) Column
takeAnyTime = do
  x <- take1 Schema.Time
  hoistEither . bimap StripedAnyError (Time . Storable.singleton) $ Any.read x

takeArrayBoolTag :: Schema -> EitherT StripedError (StateT [Array] IO) (Storable.Vector Bool64)
takeArrayBoolTag s = do
  x <- take1 s
  hoist lift . firstT StripedArrayError $ Array.toVector x

takeAnyBoolTag :: Schema -> EitherT StripedError (StateT [Any64] IO) (Storable.Vector Bool64)
takeAnyBoolTag s = do
  x <- take1 s
  hoistEither . bimap StripedAnyError Storable.singleton $ Any.read x

taekArrayErrorTag :: Schema -> EitherT StripedError (StateT [Array] IO) (Storable.Vector Error64)
taekArrayErrorTag s = do
  x <- take1 s
  hoist lift . firstT StripedArrayError $ Array.toVector x

takeAnyErrorTag :: Schema -> EitherT StripedError (StateT [Any64] IO) (Storable.Vector Error64)
takeAnyErrorTag s = do
  x <- take1 s
  hoistEither . bimap StripedAnyError Storable.singleton $ Any.read x

checkAllEq :: (Monad m, Eq a) => Schema -> [a] -> EitherT StripedError m ()
checkAllEq s = \case
  [] ->
    pure ()
  ns : nss ->
    if all (ns ==) nss then
      pure ()
    else
      left $ StripedMismatchedMeltedArrays s

takeArrayNested :: Mempool -> Schema -> EitherT StripedError (StateT [Array] IO) (Storable.Vector Int64, Column)
takeArrayNested pool s = do
  xss0 <- takeN s

  nxss0 <- liftIO $ traverse (Array.toArraySegments pool) xss0
  checkAllEq s $ fmap fst nxss0

  case nxss0 of
    [] ->
      left StripedNestedEmpty

    (ns, xs) : nxss -> do
      column <- hoist lift $ fromArrays pool s (xs : fmap snd nxss)
      pure (Array.takeDescriptor ns, column)

takeAnyNested :: Mempool -> Schema -> EitherT StripedError (StateT [Any64] IO) (Int64, Column)
takeAnyNested pool s = do
  xss0 <- takeN s
  case fmap Any.toArray xss0 of
    [] ->
      left StripedNestedEmpty
    xs : xss -> do
      n <- liftIO $ Array.length xs
      ns <- liftIO $ traverse Array.length xss
      checkAllEq s (n : ns)

      column <- hoist lift $ fromArrays pool s (xs : xss)
      pure (fromIntegral n, column)

takeArrayColumn :: Mempool -> Schema -> EitherT StripedError (StateT [Array] IO) Column
takeArrayColumn pool s =
  case s of
    Schema.Unit ->
      takeArrayUnit

    Schema.Bool ->
      takeArrayBool

    Schema.Int ->
      takeArrayInt

    Schema.Double ->
      takeArrayDouble

    Schema.Time ->
      takeArrayTime

    Schema.Sum x y ->
      Sum
        <$> takeArrayBoolTag (Schema.Sum x y)
        <*> takeArrayColumn pool x
        <*> takeArrayColumn pool y

    Schema.Option x ->
      Option
        <$> takeArrayBoolTag (Schema.Option x)
        <*> takeArrayColumn pool x

    Schema.Result x ->
      Result
        <$> taekArrayErrorTag (Schema.Result x)
        <*> takeArrayColumn pool x

    Schema.Pair x y ->
      Pair
        <$> takeArrayColumn pool x
        <*> takeArrayColumn pool y

    Schema.Struct fields ->
     Struct
       <$> traverse (traverse (takeArrayColumn pool)) fields

    Schema.String -> do
      x <- take1 Schema.String
      (ns, bs) <- liftIO $ Array.toStringSegments x
      pure $
        String (Array.takeDescriptor ns) bs

    Schema.Array sx -> do
      (ns, x) <- takeArrayNested pool sx
      pure $
        Array ns x

    Schema.Map sk sv -> do
      (nsk, k) <- takeArrayNested pool sk
      (nsv, v) <- takeArrayNested pool sv
      if nsk /= nsv then
        left $ StripedMismatchedMeltedArrays (Schema.Map sk sv)
      else
        pure $
          Map nsk k v

takeAnyColumn :: Mempool -> Schema -> EitherT StripedError (StateT [Any64] IO) Column
takeAnyColumn pool s =
  case s of
    Schema.Unit ->
      takeAnyUnit

    Schema.Bool ->
      takeAnyBool

    Schema.Int ->
      takeAnyInt

    Schema.Double ->
      takeAnyDouble

    Schema.Time ->
      takeAnyTime

    Schema.Sum x y ->
      Sum
        <$> takeAnyBoolTag (Schema.Sum x y)
        <*> takeAnyColumn pool x
        <*> takeAnyColumn pool y

    Schema.Option x ->
      Option
        <$> takeAnyBoolTag (Schema.Option x)
        <*> takeAnyColumn pool x

    Schema.Result x ->
      Result
        <$> takeAnyErrorTag (Schema.Result x)
        <*> takeAnyColumn pool x

    Schema.Pair x y ->
      Pair
        <$> takeAnyColumn pool x
        <*> takeAnyColumn pool y

    Schema.Struct fields ->
     Struct
       <$> traverse (traverse (takeAnyColumn pool)) fields

    Schema.String -> do
      x <- take1 Schema.String
      bs <- liftIO $ Any.toString x
      pure $
        String (Storable.singleton . fromIntegral $ ByteString.length bs) bs

    Schema.Array sx -> do
      (n, x) <- takeAnyNested pool sx
      pure $
        Array (Storable.singleton n) x

    Schema.Map sk sv -> do
      (nk, k) <- takeAnyNested pool sk
      (nv, v) <- takeAnyNested pool sv
      if nk /= nv then
        left $ StripedMismatchedMeltedArrays (Schema.Map sk sv)
      else
        pure $
          Map (Storable.singleton nk) k v

fromArrays :: Mempool -> Schema -> [Array] -> EitherT StripedError IO Column
fromArrays pool s xs0 = do
  (ecolumn, xs) <- liftIO $ runStateT (runEitherT (takeArrayColumn pool s)) xs0
  column <- hoistEither ecolumn
  case xs of
    [] ->
      pure column
    _ ->
      left $ StripedMeltedRemaining s (List.length xs)

fromAnys :: Mempool -> Schema -> [Any64] -> EitherT StripedError IO Column
fromAnys pool s xs0 = do
  (ecolumn, xs) <- liftIO $ runStateT (runEitherT (takeAnyColumn pool s)) xs0
  column <- hoistEither ecolumn
  case xs of
    [] ->
      pure column
    _ ->
      left $ StripedMeltedRemaining s (List.length xs)

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

unsafeConcat :: Cons Boxed.Vector Column -> Either StripedError Column
unsafeConcat cxs =
  let
    loop !xs =
      case Boxed.length xs of
        0 ->
          Left StripedConcatEmptyVector

        1 ->
          pure $ Boxed.unsafeIndex xs 0

        2 ->
          unsafeAppend
            (Boxed.unsafeIndex xs 0)
            (Boxed.unsafeIndex xs 1)

        n -> do
          let
            (xs0, xs1) =
              Boxed.splitAt (n `div` 2) xs

          x0 <- loop xs0
          x1 <- loop xs1

          unsafeAppend x0 x1
  in
    loop $! Cons.toVector cxs
{-# INLINABLE unsafeConcat #-}

unsafeAppend :: Column -> Column -> Either StripedError Column
unsafeAppend c0 c1 =
  case (c0, c1) of
    (Unit n0, Unit n1) ->
      pure $ Unit (n0 + n1)

    (Bool xs0, Bool xs1) ->
      pure $ Bool (xs0 <> xs1)

    (Int xs0, Int xs1) ->
      pure $ Int (xs0 <> xs1)

    (Double xs0, Double xs1) ->
      pure $ Double (xs0 <> xs1)

    (Time xs0, Time xs1) ->
      pure $ Time (xs0 <> xs1)

    (Sum tags0 xs0 ys0, Sum tags1 xs1 ys1) ->
      Sum (tags0 <> tags1) <$> unsafeAppend xs0 xs1 <*> unsafeAppend ys0 ys1

    (Option tags0 xs0, Option tags1 xs1) ->
      Option (tags0 <> tags1) <$> unsafeAppend xs0 xs1

    (Result tags0 xs0, Result tags1 xs1) ->
      Result (tags0 <> tags1) <$> unsafeAppend xs0 xs1

    (Pair xs0 ys0, Pair xs1 ys1) ->
      Pair <$> unsafeAppend xs0 xs1 <*> unsafeAppend ys0 ys1

    (Struct fs0, Struct fs1)
      | Cons.length fs0 == Cons.length fs1
      ->
        Struct <$> Cons.zipWithM unsafeAppendField fs0 fs1

    (String ns0 x0, String ns1 x1) ->
      pure $ String (ns0 <> ns1) (x0 <> x1)

    (Array ns0 x0, Array ns1 x1) ->
      Array (ns0 <> ns1) <$> unsafeAppend x0 x1

    (Map ns0 k0 v0, Map ns1 k1 v1) ->
      Map (ns0 <> ns1) <$> unsafeAppend k0 k1 <*> unsafeAppend v0 v1

    _ ->
      Left $ StripedAppendColumnMismatch (schema c0) (schema c1)
{-# INLINABLE unsafeAppend #-}

unsafeAppendField :: Field Column -> Field Column -> Either StripedError (Field Column)
unsafeAppendField f0 f1 =
  if fieldName f0 == fieldName f1 then
    (f0 $>) <$> unsafeAppend (fieldData f0) (fieldData f1)
  else
    Left $ StripedAppendFieldMismatch (fmap schema f0) (fmap schema f1)
{-# INLINABLE unsafeAppendField #-}

splitAt :: Int -> Column -> (Column, Column)
splitAt i = \case
  Unit n ->
    let
      m =
        min n (max 0 i)
    in
      (Unit m, Unit (n - m))

  Bool xs ->
    bimap Bool Bool $
      Storable.splitAt i xs

  Int xs ->
    bimap Int Int $
      Storable.splitAt i xs

  Double xs ->
    bimap Double Double $
      Storable.splitAt i xs

  Time xs ->
    bimap Time Time $
      Storable.splitAt i xs

  Sum tags xs ys ->
    biliftA3 Sum Sum
      (Storable.splitAt i tags)
      (splitAt i xs)
      (splitAt i ys)

  Option tags xs ->
    biliftA2 Option Option
      (Storable.splitAt i tags)
      (splitAt i xs)

  Result tags xs ->
    biliftA2 Result Result
      (Storable.splitAt i tags)
      (splitAt i xs)

  Pair xs ys ->
    biliftA2 Pair Pair
      (splitAt i xs)
      (splitAt i ys)

  Struct fields0 ->
    let
      fields =
        fmap (fmap (splitAt i)) fields0

      fst_fields =
        fmap (fmap fst) fields

      snd_fields =
        fmap (fmap snd) fields
    in
      (Struct fst_fields, Struct snd_fields)

  String ns bs ->
    let
      (ns0, ns1) =
        Storable.splitAt i ns

      !n0 =
        fromIntegral $ Storable.sum ns0

      (bs0, bs1) =
        ByteString.splitAt n0 bs
    in
      (String ns0 bs0, String ns1 bs1)

  Array ns c ->
    let
      (ns0, ns1) =
        Storable.splitAt i ns

      !n0 =
        fromIntegral $ Storable.sum ns0

      (c0, c1) =
        splitAt n0 c
    in
      (Array ns0 c0, Array ns1 c1)

  Map ns k v ->
    let
      (ns0, ns1) =
        Storable.splitAt i ns

      !n0 =
        fromIntegral $ Storable.sum ns0

      (k0, k1) =
        splitAt n0 k

      (v0, v1) =
        splitAt n0 v
    in
      (Map ns0 k0 v0, Map ns1 k1 v1)
{-# INLINABLE splitAt #-}
