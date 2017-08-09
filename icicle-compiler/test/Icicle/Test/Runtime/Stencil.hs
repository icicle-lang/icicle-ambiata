{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Stencil where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (hoist)

import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Foreign.Storable (Storable)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Runtime.Data
import           Icicle.Runtime.Stencil
import           Icicle.Test.Gen.Runtime.Data

import           P

import           System.IO (IO)


genSegment :: Storable a => Gen a -> Gen (Segmented a)
genSegment gen = do
  xss <- Gen.list (Range.linear 0 10) $ Gen.list (Range.linear 0 10) gen
  pure . toSegmented . Boxed.fromList $ fmap Storable.fromList xss

prop_snapshot_chord_compare :: Property
prop_snapshot_chord_compare =
  property $ do
    time0 <- forAll genSnapshotTime
    time1 <- forAll genSnapshotTime
    itimes <- forAll $ genSegment genInputTime

    let
      qtimes =
        toSegmented .
        Boxed.map (const $ Storable.fromList [unSnapshotTime time0, unSnapshotTime time1]) .
        Boxed.convert $
        segmentedLength itimes

    snapshot0 <- evalEither $ snapshotStencil time0 itimes
    snapshot1 <- evalEither $ snapshotStencil time1 itimes
    chord <- evalEither $ chordStencil qtimes itimes

    zipStencil snapshot0 snapshot1 === chord

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
