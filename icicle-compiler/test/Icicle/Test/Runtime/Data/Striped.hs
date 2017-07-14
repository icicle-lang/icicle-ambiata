{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Data.Striped where

import           Anemone.Foreign.Mempool (Mempool)
import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT, allocate)

import qualified Data.Vector as Boxed

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Icicle.Runtime.Data.Striped as Striped
import           Icicle.Test.Gen.Runtime.Data

import           P

import           System.IO (IO)


newMempool :: MonadResource m => m Mempool
newMempool =
  snd <$> allocate Mempool.create Mempool.free

prop_roundtrip_split :: Property
prop_roundtrip_split =
  property $ do
    schema <- forAll genSchema
    column0 <- forAll $ genColumn schema
    column1 <- forAll $ genColumn schema

    column <- evalEither $ Striped.unsafeAppend column0 column1

    let
      (scolumn0, scolumn1) =
        Striped.splitAt (Striped.length column0) column

    column0 === scolumn0
    column1 === scolumn1

prop_roundtrip_striped_logical :: Property
prop_roundtrip_striped_logical = do
  property $ do
    schema <- forAll genSchema
    values0 <- forAll $ Gen.list (Range.linear 0 100) (genValue schema)

    column <- evalEither $ Striped.fromLogical schema (Boxed.fromList values0)
    values <- evalEither $ Striped.toLogical column

    values0 === Boxed.toList values

prop_roundtrip_striped_arrays :: Property
prop_roundtrip_striped_arrays =
  property $ do
    schema <- forAll genSchema
    column0 <- forAll (genColumn schema)

    test . runResourceT $ do
      pool <- newMempool

      arrays <- evalExceptT . hoist liftIO $ Striped.toArrays pool column0
      column <- evalExceptT . hoist liftIO $ Striped.fromArrays pool schema arrays

      column0 === column

prop_roundtrip_striped_anys :: Property
prop_roundtrip_striped_anys =
  property $ do
    schema <- forAll genSchema
    (_, column0) <- forAll (genSingleton schema)

    test . runResourceT $ do
      pool <- newMempool

      anys <- evalExceptT . hoist liftIO $ Striped.headAnys pool column0
      column <- evalExceptT . hoist liftIO $ Striped.fromAnys pool schema anys

      column0 === column

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
