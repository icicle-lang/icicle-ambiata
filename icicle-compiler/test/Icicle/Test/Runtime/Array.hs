{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Array where

import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Monad.Catch (bracket)

import qualified Data.List as List

import           Disorder.Jack
import           Disorder.Core.IO (testIO)

import           Icicle.Runtime.Array (ArrayLength(..))
import qualified Icicle.Runtime.Array as Array
import           Icicle.Test.Arbitrary.Run

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)


runEitherIO :: Show x => EitherT x IO a -> IO a
runEitherIO io = do
  e <- runEitherT io
  case e of
    Left err ->
      fail (show err)
    Right x ->
      pure x

prop_capacity :: Property
prop_capacity =
  gamble (choose (2, 62) :: Jack Int64) $ \ n ->
     2 ^ n === Array.calculateCapacity (ArrayLength (2 ^ n))

prop_roundtrip_writes :: Property
prop_roundtrip_writes =
  gamble (listOf (bounded :: Jack Int64)) $ \xs0 ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> do
    array <- Array.new pool (fromIntegral (length xs0))

    for_ (List.zip [0..] xs0) $ \(i, x) ->
      runEitherIO $ Array.write array i x

    xs <- runEitherIO $ Array.toList array

    pure $
      xs0 === xs

prop_roundtrip_writes_grow :: Property
prop_roundtrip_writes_grow =
  gamble (listOf (bounded :: Jack Int64)) $ \xs0 ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> do
    array00 <- Array.new pool 0

    array <-
      flip (flip foldM array00) (List.zip [0..] xs0) $ \array0 (i, x) -> do
        array <- Array.grow pool array0 (fromIntegral i + 1)
        runEitherIO $ Array.write array i x
        pure array

    xs <- runEitherIO $ Array.toList array

    pure $
      xs0 === xs

prop_roundtrip_reads :: Property
prop_roundtrip_reads =
  gamble (listOf (bounded :: Jack Int64)) $ \xs0 ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> do
    array <- runEitherIO $ Array.fromList pool xs0

    xs <-
      for [0..length xs0 - 1] $ \i ->
        runEitherIO $ Array.read array (fromIntegral i)

    pure $
      xs0 === xs

prop_roundtrip_lists :: Property
prop_roundtrip_lists =
  gamble (listOf (bounded :: Jack Int64)) $ \xs0 ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> do
    array <- runEitherIO $ Array.fromList pool xs0
    xs <- runEitherIO $ Array.toList array

    pure $
      xs0 === xs

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore checkArgs
