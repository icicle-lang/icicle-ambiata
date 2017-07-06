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

  , Input(..)
  , InputColumn(..)

  , Output(..)

  , concatInputColumn

  -- * Icicle.Data.Name re-exports
  , InputId(..)
  , OutputId(..)
  ) where

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Word (Word32)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

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
      inputEntity :: !(Boxed.Vector EntityKey)

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

data Output key =
  Output {
      outputEntity :: !(Boxed.Vector key)

    -- | /invariant: all ((length outputEntityId ==) . length) outputColumns
    --
    , outputColumns :: !(Map OutputId Striped.Column)
    } deriving (Eq, Ord, Show, Generic)

concatInputColumn :: Cons Boxed.Vector InputColumn -> Either StripedError InputColumn
concatInputColumn xss =
  InputColumn
    (Storable.concat . Cons.toList $ fmap inputLength xss)
    (Storable.concat . Cons.toList $ fmap inputTime xss)
    <$> Striped.unsafeConcat (fmap inputColumn xss)
