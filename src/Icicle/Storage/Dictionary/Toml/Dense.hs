-- | In dense PSV, each feed (e.g. demograpghics) is an attribute (e.g. "person")
--   In TOML, this is:
--
--   @
--   [feed.demographics]
--      columns = ["age", "state", "injury"]
--
--   [fact.demographics]
--      encoding = (age:"int",state:"string",injury:(location:"string",severity:"int"))
--   @
--
--   We use this information to tell how to parse each data feed.
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Storage.Dictionary.Toml.Dense
  ( PsvInputDenseDict (..)
  , DictionaryDenseError (..)
  , denseFeeds
  ) where

import Control.Lens

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map  as Map
import Data.Map  (Map)
import Data.Text (Text)

import P

import Icicle.Common.Type (ValType)
import Icicle.Data
import Icicle.Encoding
import Icicle.Dictionary.Data
import Icicle.Storage.Dictionary.Toml.Types
import Icicle.Storage.Dictionary.Toml.Prisms


data FeedTable
  = FeedTable
  { feedName   :: Text
  , feedFields :: [Text] }

data DenseStructEncoding
  = DenseStructEncoding
  { structName :: Text
  , structFields :: Map Text Encoding }

data DictionaryDenseError
  = UndefinedFeed Text
  | InvalidFeedTable Text
  deriving (Eq, Show)

newtype PsvInputDenseDict
  = PsvInputDenseDict
  { denseDict :: Map Text [(Text, ValType)] }
  deriving (Eq, Ord, Show)


denseFeeds :: Dictionary -> Table -> Either DictionaryDenseError PsvInputDenseDict
denseFeeds dict toml
  = do ffs    <- denseEncodings toml
       let ss  = concreteStructs dict
       ss'    <- orderStructs ss ffs
       feeds  <- zipWithM go (fmap feedFields ffs) ss'
       pure $ PsvInputDenseDict $ Map.fromList $ feeds
  where
    go fieldNames s
      = do fields  <- orderFields fieldNames (structFields s)
           let fs   = fmap (second sourceTypeOfEncoding) fields
           pure $ (structName s, fs)

orderStructs :: [DenseStructEncoding] -> [FeedTable] -> Either DictionaryDenseError [DenseStructEncoding]
orderStructs ss ffs
  = foldM go [] (fmap feedName ffs)
  where
    go acc f
      = case List.find ((== f) . structName) ss of
          Just s  -> pure $ acc <> [s]
          Nothing -> Left $ UndefinedFeed f

orderFields :: [Text] -> Map Text Encoding -> Either DictionaryDenseError [(Text, Encoding)]
orderFields ordering fields
  = foldM go [] ordering
  where
    go acc f
      = case Map.lookup f fields of
          Just e  -> pure $ acc <> [(f, e)]
          Nothing -> Left $ UndefinedFeed f

denseEncodings :: Table -> Either DictionaryDenseError [FeedTable]
denseEncodings toml = do
  let feeds = fromMaybe (HashMap.empty)
            $ join $ traverse ((^? _NTable) . fst) $ (toml ^? key "feed")
  foldM go [] $ HashMap.toList feeds
  where
    go acc (k, (v, _))
      = case join (fmap (sequence . fmap (^? _VString)) (v ^? _NTValue . _VArray)) of
          Just fieldNames
            -> pure $ FeedTable k (fmap (Text.pack . fmap fst) fieldNames) : acc
          _ -> Left $ InvalidFeedTable k

concreteStructs :: Dictionary -> [DenseStructEncoding]
concreteStructs dict
  = fmap mappingOfStructs $ foldr go [] $ dictionaryEntries dict
  where
    go (DictionaryEntry a (ConcreteDefinition (StructEncoding st) _)) acc
      = (a, st) : acc
    go _ acc
      = acc

    mappingOfStructs (fname, fs)
      = DenseStructEncoding (getAttribute fname)
      $ Map.fromList
      $ fmap (\(StructField _ a e) -> (getAttribute a,e)) fs
