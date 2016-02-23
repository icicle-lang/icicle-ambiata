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
  , MissingValue
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

import Icicle.Common.Type (ValType(..))
import Icicle.Data
import Icicle.Encoding
import Icicle.Dictionary.Data
import Icicle.Storage.Dictionary.Toml.Types
import Icicle.Storage.Dictionary.Toml.Prisms


type StructName   = Text
type FieldName    = Text
type MissingValue = Text

data FeedTable
  = FeedTable
  { feedName         :: StructName
  , feedFields       :: [FieldName]
  , feedMissingValue :: Maybe MissingValue
  }

data DenseStructEncoding
  = DenseStructEncoding
  { structName   :: StructName
  , structFields :: Map FieldName (StructFieldType, Encoding) }

data DictionaryDenseError
  = UndefinedFeed    StructName
  | UndefinedField   FieldName   (Map FieldName Encoding)
  | InvalidFeedTable StructName
  deriving (Eq, Show)

data PsvInputDenseDict
  = PsvInputDenseDict
  { denseDict         :: Map StructName [(FieldName, (StructFieldType, ValType))]
  , denseMissingValue :: Map StructName MissingValue }
  deriving (Eq, Ord, Show)


denseFeeds :: Dictionary -> Table -> Either DictionaryDenseError PsvInputDenseDict
denseFeeds dict toml
  = do es       <- denseEncodings toml
       let ts    = fmap (\x -> (feedName x, feedMissingValue x)) es
       let ss    = concreteStructs dict
       ss'      <- orderStructs ss es
       feeds    <- zipWithM go (fmap feedFields es) ss'
       pure $ PsvInputDenseDict (Map.fromList feeds)
                                (Map.fromList $ catMaybes $ fmap sequence $ ts)
  where
    go fieldNames s
      = do fields  <- orderFields fieldNames (structFields s)
           let fs   = fmap (second (second sourceTypeOfEncoding)) fields
           pure $ (structName s, fs)

orderStructs :: [DenseStructEncoding] -> [FeedTable] -> Either DictionaryDenseError [DenseStructEncoding]
orderStructs ss ffs
  = foldM go [] (fmap feedName ffs)
  where
    go acc f
      = case List.find ((== f) . structName) ss of
          Just s  -> pure $ acc <> [s]
          Nothing -> Left $ UndefinedFeed f

orderFields :: [Text] -> Map FieldName (StructFieldType, Encoding) -> Either DictionaryDenseError [(Text, (StructFieldType, Encoding))]
orderFields ordering fields
  = foldM go [] ordering
  where
    go acc f
      = case Map.lookup f fields of
          Just e  -> pure $ acc <> [(f, e)]
          Nothing -> Left $ UndefinedField f $ fmap snd fields

denseEncodings :: Table -> Either DictionaryDenseError [FeedTable]
denseEncodings toml = do
  let feeds = fromMaybe (HashMap.empty)
            $ join $ traverse ((^? _NTable) . fst) $ (toml ^? key "feed")
  foldM go [] $ HashMap.toList feeds
  where
    go acc (k, (v, _))
      = let x = join
              $ fmap (sequence . fmap (^? _VString))
              $ v ^? _NTable . key "columns" . _1 . _NTValue . _VArray
            y = fmap (Text.pack . fmap fst)
              $ join
              $ sequence . fmap (^? _VString)
              $ v ^? _NTable . key "missing" . _1 . _NTValue
        in case x of
             Just fieldNames
               -> pure $ FeedTable k (fmap (Text.pack . fmap fst) fieldNames) y : acc
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
      $ fmap (\(StructField t a e) -> (getAttribute a, (t,e))) fs
