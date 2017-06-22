{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Icicle.Runtime.Striped (
    Table(..)
  , Column(..)
  , Field(..)
  ) where

import           Data.ByteString (ByteString)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           GHC.Generics (Generic)

import           Icicle.Runtime.Data

import           P

import           X.Text.Show (gshowsPrec)


data Table =
    String !ByteString
  | Array !Column
  | Map !Column !Column
    deriving (Eq, Ord, Show, Generic)

data Column =
    Unit !Int
  | Bool !(Storable.Vector Bool64)
  | Int !(Storable.Vector Int64)
  | Double !(Storable.Vector Double)
  | Time !(Storable.Vector Time64)

  | Sum !(Storable.Vector Bool64) !Column !Column
  | Option !(Storable.Vector Bool64) !Column
  | Result !(Storable.Vector Error64) !Column

  | Pair !(Storable.Vector Bool64) !Column !Column
  | Struct !(Boxed.Vector Field)

  | Nested !(Storable.Vector Int64) !Table
    deriving (Eq, Ord, Show, Generic)

data Field =
  Field {
      fieldName :: !Text
    , fieldColumn :: !Column
    } deriving (Eq, Ord, Generic)

instance Show Field where
  showsPrec =
    gshowsPrec
