{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Dictionary (
    Dictionary (..)
  , Definition (..)
  , Virtual (..)
  ) where

import           Icicle.Data

import           P


data Dictionary =
  Dictionary [(Attribute, Definition)]
  deriving (Eq, Show)


data Definition =
    ConcreteDefinition Encoding
  | VirtualDefinition  Virtual
  deriving (Eq, Show)


data Virtual =
  Virtual {
      -- | Name of concrete attribute used as input
      concrete :: Attribute
    , exp      :: Expression
    } deriving (Eq, Show)


data Expression =
  Expression -- the point of this exercise --
  deriving (Eq, Show)


