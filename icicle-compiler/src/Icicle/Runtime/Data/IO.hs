{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Runtime.Data.IO (
    EntityHash(..)
  , EntityId(..)
  , EntityKey(..)
  , MaximumMapSize(..)

  , SnapshotKey(..)
  , SnapshotTime(..)

  , ChordKey(..)
  , ChordDescriptor(..)
  , Label(..)

  , QueryTime(..)

  , Input(..)
  , InputTime(..)
  , InputColumn(..)

  , Output(..)

  , splitInput
  , splitInputN
  , concatInput

  , splitInputColumn
  , concatInputColumn
  , appendInputColumn

  , splitOutput
  , splitOutputN
  , concatOutput

  -- * Icicle.Data.Name re-exports
  , InputId(..)
  , OutputId(..)
  ) where

import           Data.Biapplicative ((<<$>>), (<<*>>))
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable
import           Data.Word (Word32)

import           Foreign.Storable (Storable)

import           GHC.Generics (Generic)

import           Icicle.Data.Name
import           Icicle.Runtime.Data.Primitive
import           Icicle.Runtime.Data.Striped (StripedError)
import qualified Icicle.Runtime.Data.Striped as Striped

import           P

import           X.Data.Vector.Cons (Cons)
import qualified X.Data.Vector.Cons as Cons
import           X.Text.Show (gshowsPrec)


newtype EntityHash =
  EntityHash {
      unEntityHash :: Word32
    } deriving (Eq, Ord, Generic)

instance Show EntityHash where
  showsPrec =
    gshowsPrec

newtype EntityId =
  EntityId {
      unEntityId :: ByteString
    } deriving (Eq, Ord, Generic)

instance Show EntityId where
  showsPrec =
    gshowsPrec

data EntityKey =
  EntityKey {
      entityHash :: !EntityHash
    , entityId :: !EntityId
    } deriving (Eq, Ord, Generic)

instance Show EntityKey where
  showsPrec =
    gshowsPrec

newtype MaximumMapSize =
  MaximumMapSize {
      unMaximumMapSize :: Int64
    } deriving (Eq, Ord, Generic, Storable, Num)

instance Show MaximumMapSize where
  showsPrec =
    gshowsPrec

newtype QueryTime =
  QueryTime {
      unQueryTime :: Time64
    } deriving (Eq, Ord, Generic, Storable)

instance Show QueryTime where
  showsPrec =
    gshowsPrec

newtype InputTime =
  InputTime {
      unInputTime :: Time64
    } deriving (Eq, Ord, Generic, Storable)

instance Show InputTime where
  showsPrec =
    gshowsPrec

newtype SnapshotTime =
  SnapshotTime {
      unSnapshotTime :: QueryTime
    } deriving (Eq, Ord, Generic, Storable)

instance Show SnapshotTime where
  showsPrec =
    gshowsPrec

newtype ChordDescriptor =
  ChordDescriptor {
      unChordDescriptor :: EntityId -> Set Label
    }

data Label =
  Label {
      labelTime :: !QueryTime
    , labelTag :: !ByteString
    } deriving (Eq, Ord, Generic)

instance Show Label where
  showsPrec =
    gshowsPrec

newtype SnapshotKey =
  SnapshotKey {
      snapshotEntity :: EntityKey
    } deriving (Eq, Ord, Generic)

instance Show SnapshotKey where
  showsPrec =
    gshowsPrec

data ChordKey =
  ChordKey {
      chordEntity :: !EntityKey
    , chordLabel :: !ByteString
    } deriving (Eq, Ord, Generic)

instance Show ChordKey where
  showsPrec =
    gshowsPrec

data Input =
  Input {
      inputKey :: !(Boxed.Vector EntityKey)

    -- | /invariant: all ((length inputKey ==) . length . inputLength) inputColumns
    --
    , inputColumns :: !(Map InputId InputColumn)
    } deriving (Eq, Ord, Show, Generic)

data InputColumn =
  InputColumn {
      inputLength :: !(Storable.Vector Int64)

    -- | /invariant: length inputTime = sum inputLength/
    --
    , inputTime :: !(Storable.Vector InputTime)

    -- | /invariant: length inputTombstone = sum inputLength/
    --
    , inputTombstone :: !(Storable.Vector Error64)

    -- | /invariant: length inputColumn = sum inputLength/
    --
    , inputColumn :: !Striped.Column
    } deriving (Eq, Ord, Show, Generic)

data Output key =
  Output {
      outputKey :: !(Boxed.Vector key)

    -- | /invariant: all ((length outputEntityId ==) . length) outputColumns
    --
    , outputColumns :: !(Map OutputId Striped.Column)
    } deriving (Eq, Ord, Show, Generic)

mapAppend :: (Applicative m, Ord k) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
mapAppend f xs ys =
  sequenceA $
    Map.mergeWithKey (\_ x y -> Just (f x y)) (fmap pure) (fmap pure) xs ys

mapConcat :: (Monad m, Ord k) => (a -> a -> m a) -> Boxed.Vector (Map k a) -> m (Map k a)
mapConcat f xss =
  case Boxed.length xss of
    0 ->
      pure $ Map.empty

    1 ->
      pure $ xss Boxed.! 0

    2 ->
      mapAppend f (xss Boxed.! 0) (xss Boxed.! 1)

    n -> do
      let
        (xss0, xss1) =
          Boxed.splitAt (n `div` 2) xss

      xs0 <- mapConcat f xss0
      xs1 <- mapConcat f xss1

      mapAppend f xs0 xs1

splitMap :: (Int -> v -> (v, v)) -> Int -> Map k v -> (Map k v, Map k v)
splitMap split i kvs0 =
  let
    kvs =
      fmap (split i) kvs0
  in
    (fmap fst kvs, fmap snd kvs)

splitN :: (a -> Int) -> (Int -> a -> (a, a)) -> Int -> a -> Cons Boxed.Vector a
splitN len split n_splits0 input0 =
  let
    n_total =
      len input0

    n_size =
      min 1 (n_total `quot` n_splits0)

    loop input n_splits =
      if n_splits <= 1 || len input <= n_size then
        input :| []
      else
        let
          (xs, ys) =
            split n_size input
        in
          xs `NonEmpty.cons` loop ys (n_splits - 1)
  in
    Cons.fromNonEmpty $ loop input0 n_splits0

splitInputColumn :: Int -> InputColumn -> (InputColumn, InputColumn)
splitInputColumn i input =
  let
    (ns0, ns1) =
      Storable.splitAt i $ inputLength input

    !n0 =
      fromIntegral $ Storable.sum ns0
  in
    bimap (InputColumn ns0) (InputColumn ns1)
      <<$>> Storable.splitAt n0 (inputTime input)
      <<*>> Storable.splitAt n0 (inputTombstone input)
      <<*>> Striped.splitAt n0 (inputColumn input)

splitInput :: Int -> Input -> (Input, Input)
splitInput i input =
  bimap Input Input
    <<$>> Boxed.splitAt i (inputKey input)
    <<*>> splitMap splitInputColumn i (inputColumns input)

splitInputN :: Int -> Input -> Cons Boxed.Vector Input
splitInputN n_splits input =
  splitN (Boxed.length . inputKey) splitInput n_splits input

concatInputColumn :: Cons Boxed.Vector InputColumn -> Either StripedError InputColumn
concatInputColumn xss =
  InputColumn
    (Storable.concat . Cons.toList $ fmap inputLength xss)
    (Storable.concat . Cons.toList $ fmap inputTime xss)
    (Storable.concat . Cons.toList $ fmap inputTombstone xss)
    <$> Striped.unsafeConcat (fmap inputColumn xss)

appendInputColumn :: InputColumn -> InputColumn -> Either StripedError InputColumn
appendInputColumn xs ys =
  InputColumn
    (inputLength xs <> inputLength ys)
    (inputTime xs <> inputTime ys)
    (inputTombstone xs <> inputTombstone ys)
    <$> Striped.unsafeAppend (inputColumn xs) (inputColumn ys)

concatInput :: Cons Boxed.Vector Input -> Either StripedError Input
concatInput xss =
  Input
    (Boxed.concat . Cons.toList $ fmap inputKey xss)
    <$> mapConcat appendInputColumn (Cons.toVector (fmap inputColumns xss))

splitOutput :: Int -> Output key -> (Output key, Output key)
splitOutput i output =
  bimap Output Output
    <<$>> Boxed.splitAt i (outputKey output)
    <<*>> splitMap Striped.splitAt i (outputColumns output)

splitOutputN :: Int -> Output key -> Cons Boxed.Vector (Output key)
splitOutputN n_splits output =
  splitN (Boxed.length . outputKey) splitOutput n_splits output

concatOutput :: Cons Boxed.Vector (Output key) -> Either StripedError (Output key)
concatOutput xss =
  Output
    (Boxed.concat . Cons.toList $ fmap outputKey xss)
    <$> mapConcat Striped.unsafeAppend (Cons.toVector (fmap outputColumns xss))
