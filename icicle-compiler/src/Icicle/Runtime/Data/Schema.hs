{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Runtime.Data.Schema (
    Schema(..)
  ) where

import qualified Data.Vector as Boxed

import           GHC.Generics (Generic)

import           Icicle.Runtime.Data.Primitive

import           P hiding (Sum)

import           X.Data.Vector.Cons (Cons)


data Schema =
    Unit
  | Bool
  | Int
  | Double
  | Time

  | Sum !Schema !Schema
  | Option !Schema
  | Result !Schema

  | Pair !Schema !Schema
  | Struct !(Cons Boxed.Vector (Field Schema))

  | String
  | Array !Schema
  | Map !Schema !Schema
    deriving (Eq, Ord, Show, Generic)
