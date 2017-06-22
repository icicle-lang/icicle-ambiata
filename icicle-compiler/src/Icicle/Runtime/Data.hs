{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Icicle.Runtime.Data (
    Bool64(..)
  , Time64(..)
  , Error64(..)
  ) where

import           Data.Word (Word64)

import           Foreign.Storable (Storable)

import           GHC.Generics (Generic)

import           P

import           X.Text.Show (gshowsPrec)


newtype Bool64 =
  Bool64 {
      unBool64 :: Word64
    } deriving (Eq, Ord, Generic, Storable)

newtype Time64 =
  Time64 {
      unTime64 :: Word64
    } deriving (Eq, Ord, Generic, Storable)

newtype Error64 =
  Error64 {
      unError64 :: Word64
    } deriving (Eq, Ord, Generic, Storable)

instance Show Bool64 where
  showsPrec =
    gshowsPrec

instance Show Time64 where
  showsPrec =
    gshowsPrec

instance Show Error64 where
  showsPrec =
    gshowsPrec
