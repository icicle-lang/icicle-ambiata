{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Runtime.Data.IO (
    EntityId(..)
  , MaximumMapSize(..)

  , SnapshotKey(..)
  , SnapshotTime(..)

  , ChordKey(..)
  , ChordDescriptor(..)
  , Label(..)

  , Input(..)
  , InputColumn(..)

  , Output(..)
  ) where

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Foreign.Storable (Storable)

import           GHC.Generics (Generic)

import           Icicle.Data.Name
import           Icicle.Runtime.Data.Primitive
import qualified Icicle.Runtime.Data.Striped as Striped

import           P

import           X.Text.Show (gshowsPrec)


newtype EntityId =
  EntityId {
      unEntityId :: ByteString
    } deriving (Eq, Ord, Generic)

instance Show EntityId where
  showsPrec =
    gshowsPrec

newtype MaximumMapSize =
  MaximumMapSize {
      unMaximumMapSize :: Int64
    } deriving (Eq, Ord, Generic, Storable, Num)

instance Show MaximumMapSize where
  showsPrec =
    gshowsPrec

newtype SnapshotTime =
  SnapshotTime {
      unSnapshotTime :: Time64
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
      labelTime :: !Time64
    , labelTag :: !ByteString
    } deriving (Eq, Ord, Generic)

instance Show Label where
  showsPrec =
    gshowsPrec

newtype SnapshotKey =
  SnapshotKey {
      snapshotEntityId :: EntityId
    } deriving (Eq, Ord, Generic)

instance Show SnapshotKey where
  showsPrec =
    gshowsPrec

data ChordKey =
  ChordKey {
      chordEntityId :: !EntityId
    , chordLabel :: !ByteString
    } deriving (Eq, Ord, Generic)

instance Show ChordKey where
  showsPrec =
    gshowsPrec

data Input =
  Input {
      inputEntityId :: !(Boxed.Vector EntityId)

    -- | /invariant: all ((length inputEntityId ==) . length . inputLength) inputColumns
    --
    , inputColumns :: !(Map InputId InputColumn)
    } deriving (Eq, Ord, Show, Generic)

data InputColumn =
  InputColumn {
      inputLength :: !(Storable.Vector Int64)

    -- | /invariant: length inputTime = sum inputLength/
    --
    , inputTime :: !(Storable.Vector Time64)

    -- | /invariant: length inputColumn = sum inputLength/
    --
    , inputColumn :: !Striped.Column
    } deriving (Eq, Ord, Show, Generic)

data Output i =
  Output {
      outputEntityId :: !(Boxed.Vector i)

    -- | /invariant: all ((length outputEntityId ==) . length) outputColumns
    --
    , outputColumns :: !(Map OutputId Striped.Column)
    } deriving (Eq, Ord, Show, Generic)
