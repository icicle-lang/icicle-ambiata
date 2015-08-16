{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Icicle.Storage.Dictionary.Toml.Prisms (
    _NTValue
  , _NTable
  , _VString
  , _VInteger
  , _VFloat
  , _VBoolean
  , _VDatetime
  , _VArray
  , key
  ) where

import           Control.Applicative
import           Control.Lens

import           Data.Bool
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.Int
import           Data.Text
import           Data.Time

import qualified Text.Parsec.Pos as Pos

import           GHC.Float

import           Icicle.Storage.Dictionary.Toml.Types


_NTValue :: Prism' Node TValue
_NTValue =
  prism  NTValue $ \n -> case n of
    NTValue v -> pure v
    _ -> Left n

_NTable :: Prism' Node Table
_NTable =
  prism  NTable $ \n -> case n of
    NTable v -> pure v
    _ -> Left n

_VString :: Prism' TValue [(Char, Pos.SourcePos)]
_VString =
  prism  VString $ \n -> case n of
    VString v -> pure v
    _ -> Left n

_VInteger :: Prism' TValue Int64
_VInteger =
  prism VInteger $ \n -> case n of
    VInteger v -> pure v
    _ -> Left n

_VFloat :: Prism' TValue Double
_VFloat =
  prism VFloat $ \n -> case n of
    VFloat v -> pure v
    _ -> Left n

_VBoolean :: Prism' TValue Bool
_VBoolean =
  prism VBoolean $ \n -> case n of
    VBoolean v -> pure v
    _ -> Left n

_VDatetime :: Prism' TValue UTCTime
_VDatetime =
  prism VDatetime $ \n -> case n of
    VDatetime v -> pure v
    _ -> Left n

_VArray :: Prism' TValue [TValue]
_VArray =
  prism VArray $ \n -> case n of
    VArray v -> pure v
    _ -> Left n

key :: Text -> Traversal' Table (Node, Pos.SourcePos)
key = ix
