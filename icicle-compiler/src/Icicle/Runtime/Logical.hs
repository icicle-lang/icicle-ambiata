{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Icicle.Runtime.Logical (
    Value(..)
  ) where

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Vector as Boxed

import           GHC.Generics (Generic)

import           Icicle.Runtime.Data

import           P


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
  | Struct !(Boxed.Vector Value)

  | String !ByteString
  | Array !(Boxed.Vector Value)
  | Map !(Map Value Value)
    deriving (Eq, Ord, Show, Generic)
