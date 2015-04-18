{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Simulator (
    streams
  ) where

import           Data.List

import           Icicle.Data

import           P

streams :: [AsAt Fact] -> [[AsAt Fact]]
streams =
  fmap (sortBy (compare `on` time)) . groupBy ((==) `on` partitionBy) . sortBy (compare `on` partitionBy)

partitionBy :: AsAt Fact -> (Entity, Attribute)
partitionBy f =
  (entity . fact $ f, attribute . fact $ f)
