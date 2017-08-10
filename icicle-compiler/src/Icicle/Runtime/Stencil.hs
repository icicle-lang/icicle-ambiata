{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Runtime.Stencil (
    Stencil(..)
  , EntityStencil(..)
  , zipStencil

  , Segmented(..)
  , toSegmented
  , fromSegmented

  , StencilError(..)
  , renderStencilError

  , chordStencil
  , chordEntityStencil
  , snapshotStencil
  ) where

import qualified Data.Text as Text
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Foreign.Storable (Storable)

import           Icicle.Runtime.Data

import           P

import           Zebra.X.Vector.Segment (SegmentError)
import qualified Zebra.X.Vector.Segment as Segment -- FIXME move to x-vector


-- | The stencils to use for each entity in a block.
--
newtype Stencil =
  Stencil {
      unStencil :: Boxed.Vector EntityStencil
    } deriving (Eq, Ord, Show)

-- | The number of inputs to take for each query we want to run against a single entity.
--
data EntityStencil =
  EntityStencil {
      stencilTime :: !(Storable.Vector QueryTime)
    , stencilLength :: !(Storable.Vector Int64)
    } deriving (Eq, Ord, Show)

instance Monoid EntityStencil where
  mempty =
    EntityStencil mempty mempty
  mappend (EntityStencil x a) (EntityStencil y b) =
    EntityStencil (x <> y) (a <> b)

data Segmented a =
  Segmented {
      segmentedLength :: !(Storable.Vector Int64)
    , segmentedData :: !(Storable.Vector a)
    } deriving (Eq, Ord, Show)

data StencilError =
    StencilEntityCountMismatch !Int !Int
  | StencilSegmentError !SegmentError
    deriving (Eq, Show)

renderStencilError :: StencilError -> Text
renderStencilError = \case
  StencilEntityCountMismatch qc ic ->
    "Entity count mismatch, query time count = " <> Text.pack (show qc) <> ", input time count = " <> Text.pack (show ic)
  StencilSegmentError x ->
    Segment.renderSegmentError x

toSegmented :: Storable a => Boxed.Vector (Storable.Vector a) -> Segmented a
toSegmented xss =
  Segmented
    (Storable.convert $ Boxed.map (fromIntegral . Storable.length) xss)
    (Storable.concat $ Boxed.toList xss)

fromSegmented :: Storable a => Segmented a -> Either StencilError (Boxed.Vector (Storable.Vector a))
fromSegmented x =
  first StencilSegmentError $
    Segment.reify (segmentedLength x) (segmentedData x)

zipStencil :: Stencil -> Stencil -> Stencil
zipStencil (Stencil xs) (Stencil ys) =
  Stencil $ Boxed.zipWith (<>) xs ys

checkEntityCounts :: Segmented a -> Segmented b -> Either StencilError ()
checkEntityCounts x y =
  let
    n =
      Storable.length $ segmentedLength x

    m =
      Storable.length $ segmentedLength y
  in
    if n == m then
      Right ()
    else
      Left $ StencilEntityCountMismatch n m

queryStencil :: QueryTime -> Storable.Vector InputTime -> Int64
queryStencil (QueryTime time) ts =
  fromIntegral . Storable.length $ Storable.takeWhile (< InputTime time) ts

snapshotStencil :: SnapshotTime -> Segmented InputTime -> Either StencilError Stencil
snapshotStencil (SnapshotTime time) tss0 = do
  tss <- fromSegmented tss0
  pure . Stencil $
    Boxed.map (EntityStencil (Storable.singleton time) . Storable.singleton . queryStencil time) tss

chordEntityStencil :: Storable.Vector QueryTime -> Storable.Vector InputTime -> EntityStencil
chordEntityStencil qtimes ftimes =
  EntityStencil qtimes $
    Storable.map (flip queryStencil ftimes) qtimes

chordStencil :: Segmented QueryTime -> Segmented InputTime -> Either StencilError Stencil
chordStencil qtimes0 ftimes0 = do
  checkEntityCounts qtimes0 ftimes0

  qtimes <- fromSegmented qtimes0
  ftimes <- fromSegmented ftimes0

  pure . Stencil $
    Boxed.zipWith chordEntityStencil qtimes ftimes
