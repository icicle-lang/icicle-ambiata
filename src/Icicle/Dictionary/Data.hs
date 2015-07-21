{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Dictionary.Data (
    Dictionary (..)
  , DictionaryEntry (..)
  , Definition (..)
  , Virtual (..)
  , getVirtualFeatures
  ) where

import           Icicle.Data

import qualified Icicle.Core.Program.Program as P

import           P

import           Data.Text

data Dictionary =
  Dictionary [DictionaryEntry]
  deriving (Eq, Show)

data DictionaryEntry =
  DictionaryEntry Attribute Definition
  deriving (Eq, Show)

data Definition =
    ConcreteDefinition Encoding
  | VirtualDefinition  Virtual
  deriving (Eq, Show)


data Virtual =
  Virtual {
      -- | Name of concrete attribute used as input
      concrete :: Attribute
    , program  :: P.Program Text
    } deriving (Eq, Show)


-- | Get all virtual features from dictionary
getVirtualFeatures :: Dictionary -> [(Attribute, Virtual)]
getVirtualFeatures (Dictionary fs)
 = P.concatMap getV fs
 where
  getV (DictionaryEntry a (VirtualDefinition v))
   = [(a,v)]
  getV _
   = []
