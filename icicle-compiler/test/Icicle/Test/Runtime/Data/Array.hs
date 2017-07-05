{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Data.Array where

import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Monad.Catch (bracket)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import qualified Data.Vector.Storable as Storable

import           Foreign.C.String (peekCString)

import           Disorder.Core.IO (testIO)
import           Disorder.Corpus (muppets)
import           Disorder.Jack

import           Icicle.Runtime.Data.Array (ArrayLength(..))
import qualified Icicle.Runtime.Data.Array as Array
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

prop_roundtrip_string_segments0 :: Property
prop_roundtrip_string_segments0 =
  gamble (listOf (elements muppets)) $ \bss0 ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> do
    let
      bs0 =
        ByteString.concat bss0

      ns0 =
        Storable.fromList .
        fmap fromIntegral $
        fmap ByteString.length bss0

    array <- runEitherIO $ Array.fromStringSegments pool ns0 bs0
    (ns, bs) <- Array.toStringSegments array

    pure $
      conjoin [
          ns0 === ns
        , bs0 === bs
        ]

prop_roundtrip_string_segments1 :: Property
prop_roundtrip_string_segments1 =
  gamble (listOf (elements muppets)) $ \bss0 ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> do
    let
      bs =
        ByteString.concat bss0

      ns =
        Storable.fromList .
        fmap fromIntegral $
        fmap ByteString.length bss0

    array <- runEitherIO $ Array.fromStringSegments pool ns bs
    ptrs <- runEitherIO $ Array.toList array

    bss <- fmap Char8.pack <$> traverse peekCString ptrs

    pure $
      bss0 === bss

prop_roundtrip_array_segments0 :: Property
prop_roundtrip_array_segments0 =
  gamble (listOf (listOf (bounded :: Jack Int64))) $ \xss0 ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> do
    xs0 <- runEitherIO . Array.fromList pool $ concat xss0

    let
      ns0 =
        Storable.fromList .
        fmap fromIntegral $
        fmap length xss0

    array <- runEitherIO $ Array.fromArraySegments pool ns0 xs0
    (ns, xs1) <- Array.toArraySegments pool array
    xs <- runEitherIO $ Array.toList xs1

    pure $
      conjoin [
          ns0 === ns
        , concat xss0 === xs
        ]

prop_roundtrip_array_segments1 :: Property
prop_roundtrip_array_segments1 =
  gamble (listOf (listOf (bounded :: Jack Int64))) $ \xss0 ->
  testIO . bracket Mempool.create Mempool.free $ \pool -> do
    xs <- runEitherIO . Array.fromList pool $ concat xss0

    let
      ns =
        Storable.fromList .
        fmap fromIntegral $
        fmap length xss0

    array <- runEitherIO $ Array.fromArraySegments pool ns xs
    arrays <- runEitherIO $ Array.toList array
    xss <- runEitherIO $ traverse Array.toList arrays

    pure $
      xss0 === xss

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore checkArgs
