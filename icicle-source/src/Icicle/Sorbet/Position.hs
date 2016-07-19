{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Icicle.Sorbet.Position (
    Positioned(..)
  , Position(..)
  ) where

import           Data.Data (Data)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           P

import           System.IO (FilePath)

import           Text.Megaparsec (ShowToken(..), Stream(..))
import           Text.Megaparsec.Pos (SourcePos(..), unsafePos)

import           X.Text.Show (gshowsPrec)


data Positioned a =
  Positioned {
      posStart :: !Position
    , posEnd :: !Position
    , posTail :: !a
    } deriving (Eq, Ord, Generic, Data, Typeable, Functor)

data Position =
  Position {
      posFile :: !FilePath
    , posLine :: !Int
    , posColumn :: !Int
    } deriving (Eq, Ord, Generic, Data, Typeable)

instance Show a => Show (Positioned a) where
  showsPrec =
    gshowsPrec

instance Show Position where
  showsPrec =
    gshowsPrec

instance ShowToken a => ShowToken (Positioned a) where
  showTokens =
    showTokens . fmap posTail

instance Ord a => Stream [Positioned a] where
  type Token [Positioned a] = Positioned a

  uncons = \case
    [] ->
      Nothing
    x : xs ->
      Just (x, xs)

  updatePos _ _ _ (Positioned start end _) =
    (toSourcePos start, toSourcePos end)

toSourcePos :: Position -> SourcePos
toSourcePos = \case
  Position file line col ->
    SourcePos file
      (unsafePos $ fromIntegral line)
      (unsafePos $ fromIntegral col)
