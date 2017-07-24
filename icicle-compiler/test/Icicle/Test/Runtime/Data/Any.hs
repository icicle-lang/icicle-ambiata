{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Data.Any where

import           Anemone.Foreign.Mempool (Mempool)
import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT, allocate)

import qualified Data.ByteString as ByteString

import           Foreign.Storable (Storable)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Icicle.Runtime.Data.Any as Any

import           P hiding (any)

import           System.IO (IO)


newMempool :: MonadResource m => m Mempool
newMempool =
  snd <$> allocate Mempool.create Mempool.free

checkFromRead :: (MonadTest m, Storable a, Show a, Eq a) => a -> m ()
checkFromRead x0 = do
  any <- evalEither $ Any.from x0
  x1 <- evalEither $ Any.read any
  x0 === x1

prop_any_int :: Property
prop_any_int =
  withTests 1000 . property $ do
    x <- forAll $ Gen.int64 Range.linearBounded
    checkFromRead x

prop_any_double :: Property
prop_any_double =
  withTests 1000 . property $ do
    x <- forAll $ Gen.double (Range.linearFracFrom 0 (-1e308) 1e308)
    checkFromRead x

prop_any_string :: Property
prop_any_string =
  withTests 1000 . property $ do
    x0 <- forAll . fmap (ByteString.filter (/= 0)) $ Gen.bytes (Range.linear 0 20)

    test . runResourceT $ do
      pool <- newMempool
      any <- liftIO $ Any.fromString pool x0
      x <- liftIO $ Any.toString any
      x0 === x

prop_null_string :: Property
prop_null_string =
  withTests 1 . property $ do
    x <- liftIO $ Any.toString Any.nullString
    ByteString.empty === x

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
