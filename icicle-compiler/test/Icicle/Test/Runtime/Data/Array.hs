{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Data.Array where

import           Anemone.Foreign.Mempool (Mempool)
import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.Reader.Class (MonadReader(..))
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT, allocate)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as Storable

import           Disorder.Corpus (muppets)

import           Foreign.C.String (peekCString)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Runtime.Data.Array (Array, ArrayLength(..), ArrayIndex(..))
import qualified Icicle.Runtime.Data.Array as Array

import           P

import           System.IO (IO)


newMempool :: MonadResource m => m Mempool
newMempool =
  snd <$> allocate Mempool.create Mempool.free

prop_capacity :: Property
prop_capacity =
  property $ do
    n <- forAll $ Gen.int (Range.constant 2 62)
    2 ^ n === Array.calculateCapacity (ArrayLength (2 ^ n))

prop_roundtrip_writes :: Property
prop_roundtrip_writes =
  property $ do
    xs0 <- forAll . Gen.list (Range.linear 0 100) $ Gen.int64 Range.linearBounded
    test . runResourceT $ do
      pool <- newMempool
      array <- liftIO $ Array.new pool (fromIntegral (List.length xs0))

      for_ (List.zip [0..] xs0) $ \(i, x) ->
        evalExceptT . hoist liftIO $ Array.write array i x

      xs <- evalExceptT . hoist liftIO $ Array.toList array

      xs0 === xs

prop_roundtrip_writes_grow :: Property
prop_roundtrip_writes_grow =
  property $ do
    xs0 <- forAll . Gen.list (Range.linear 0 100) $ Gen.int64 Range.linearBounded
    test . runResourceT $ do
      pool <- newMempool
      array00 <- liftIO $ Array.new pool 0

      array <-
        flip (flip foldM array00) (List.zip [0..] xs0) $ \array0 (i, x) -> do
          array <- liftIO $ Array.grow pool array0 (fromIntegral i + 1)
          evalExceptT . hoist liftIO $ Array.write array i x
          pure array

      xs <- evalExceptT . hoist liftIO $ Array.toList array

      xs0 === xs

prop_roundtrip_reads :: Property
prop_roundtrip_reads =
  property $ do
    xs0 <- forAll . Gen.list (Range.linear 0 100) $ Gen.int64 Range.linearBounded
    test . runResourceT $ do
      pool <- newMempool
      array <- evalExceptT . hoist liftIO $ Array.fromList pool xs0

      xs <-
        for [0..List.length xs0 - 1] $ \i ->
          evalExceptT . hoist liftIO $ Array.read array (fromIntegral i)

      xs0 === xs

prop_roundtrip_lists :: Property
prop_roundtrip_lists =
  property $ do
    xs0 <- forAll . Gen.list (Range.linear 0 100) $ Gen.int64 Range.linearBounded
    test . runResourceT $ do
      pool <- newMempool

      array <- evalExceptT . hoist liftIO $ Array.fromList pool xs0
      xs <- evalExceptT . hoist liftIO $ Array.toList array

      xs0 === xs

prop_roundtrip_string_segments0 :: Property
prop_roundtrip_string_segments0 =
  property $ do
    bss0 <- forAll $ Gen.list (Range.linear 0 100) (Gen.element muppets)
    test . runResourceT $ do
      pool <- newMempool

      let
        bs0 =
          ByteString.concat bss0

        ns0 =
          Storable.fromList .
          fmap fromIntegral $
          fmap ByteString.length bss0

      array <- evalExceptT . hoist liftIO $ Array.fromStringSegments pool ns0 bs0
      (ns, bs) <- liftIO $ Array.toStringSegments array

      ns0 === ns
      bs0 === bs

prop_roundtrip_string_segments1 :: Property
prop_roundtrip_string_segments1 =
  property $ do
    bss0 <- forAll $ Gen.list (Range.linear 0 100) (Gen.element muppets)
    test . runResourceT $ do
      pool <- newMempool

      let
        bs =
          ByteString.concat bss0

        ns =
          Storable.fromList .
          fmap fromIntegral $
          fmap ByteString.length bss0

      array <- evalExceptT . hoist liftIO $ Array.fromStringSegments pool ns bs
      ptrs <- evalExceptT . hoist liftIO $ Array.toList array

      bss <- fmap Char8.pack <$> liftIO (traverse peekCString ptrs)

      bss0 === bss

prop_roundtrip_array_segments0 :: Property
prop_roundtrip_array_segments0 =
  property $ do
    xss0 <- forAll $
      Gen.list (Range.linear 0 100) $
      Gen.list (Range.linear 0 10) $
      Gen.int64 Range.linearBounded

    test . runResourceT $ do
      pool <- newMempool
      xs0 <- evalExceptT . hoist liftIO . Array.fromList pool $ concat xss0

      let
        ns0 =
          Storable.fromList .
          fmap fromIntegral $
          fmap length xss0

      array <- evalExceptT . hoist liftIO $ Array.fromArraySegments pool ns0 xs0
      (ns, xs1) <- liftIO $ Array.toArraySegments pool array
      xs <- evalExceptT . hoist liftIO $ Array.toList xs1

      ns0 === ns
      concat xss0 === xs

prop_roundtrip_array_segments1 :: Property
prop_roundtrip_array_segments1 =
  property $ do
    xss0 <- forAll $
      Gen.list (Range.linear 0 100) $
      Gen.list (Range.linear 0 10) $
      Gen.int64 Range.linearBounded

    test . runResourceT $ do
      pool <- newMempool
      xs <- evalExceptT . hoist liftIO . Array.fromList pool $ concat xss0

      let
        ns =
          Storable.fromList .
          fmap fromIntegral $
          fmap length xss0

      array <- evalExceptT . hoist liftIO $ Array.fromArraySegments pool ns xs
      arrays <- evalExceptT . hoist liftIO $ Array.toList array
      xss <- evalExceptT . hoist liftIO $ traverse Array.toList arrays

      xss0 === xss

------------------------------------------------------------------------
-- State

data State v =
  State {
      stateArrays :: Map (Var Array v) [Int64]
    }

initialState :: State v
initialState =
  State Map.empty

------------------------------------------------------------------------
-- NewArray

data NewArray (v :: * -> *) =
  NewArray ArrayLength
  deriving (Eq, Show)

instance HTraversable NewArray where
  htraverse _ (NewArray x) =
    pure (NewArray x)

newArray :: (MonadGen n, MonadReader Mempool m, MonadIO m) => Command n m State
newArray =
  let
    gen _s =
      Just $
        NewArray . ArrayLength <$> Gen.int64 (Range.linear 0 100)

    execute (NewArray len) = do
      pool <- ask
      liftIO $ Array.new pool len
  in
    Command gen execute [
        Update $ \s (NewArray len) o ->
          s {
            stateArrays =
              Map.insert o (List.replicate (fromIntegral len) 0) (stateArrays s)
          }
      ]

------------------------------------------------------------------------
-- ReadArray

data ReadArray v =
  ReadArray ArrayIndex (Var Array v)
  deriving (Eq, Show)

instance HTraversable ReadArray where
  htraverse f (ReadArray ix xs) =
    ReadArray ix <$> htraverse f xs

readList :: ArrayIndex -> Maybe [a] -> Maybe a
readList ix = \case
  Nothing ->
    Nothing
  Just xs ->
    if fromIntegral ix < List.length xs then
      Just (xs List.!! fromIntegral ix)
    else
      Nothing

readArray :: (MonadGen n, MonadTest m, MonadIO m) => Command n m State
readArray =
  let
    gen s =
      case Map.toList $ stateArrays s of
        [] ->
          Nothing
        xs ->
          Just $ do
            (array, vs) <- Gen.element xs
            ReadArray
              <$> (ArrayIndex <$> Gen.int64 (Range.linear 0 . fromIntegral $ List.length vs))
              <*> pure array

    execute (ReadArray ix (Var (Concrete array))) = do
      evalExceptT . hoist liftIO $ Array.read array ix
  in
    Command gen execute [
        Require $ \s (ReadArray ix array) ->
          Just (fromIntegral ix) < fmap List.length (Map.lookup array $ stateArrays s)

      , Ensure $ \_before after (ReadArray ix array) o ->
          readList ix (Map.lookup array $ stateArrays after) === Just o
      ]

------------------------------------------------------------------------
-- WriteArray

data WriteArray v =
  WriteArray ArrayIndex Int64 (Var Array v)
  deriving (Eq, Show)

instance HTraversable WriteArray where
  htraverse f (WriteArray ix x array) =
    WriteArray ix x <$> htraverse f array

writeList :: ArrayIndex -> a -> [a] -> [a]
writeList ix0 x xs =
  let
    ix =
      fromIntegral ix0

    (xs1, xs2) =
      List.splitAt ix xs
  in
    if ix < 0 || ix >= List.length xs then
      xs
    else
      xs1 <> [x] <> List.drop 1 xs2

writeArray :: (MonadGen n, MonadTest m, MonadIO m) => Command n m State
writeArray =
  let
    gen s =
      case Map.toList $ stateArrays s of
        [] ->
          Nothing
        xs ->
          Just $ do
            (array, vs) <- Gen.element xs
            WriteArray
              <$> (ArrayIndex <$> Gen.int64 (Range.linear 0 . fromIntegral $ List.length vs))
              <*> Gen.int64 Range.linearBounded
              <*> pure array

    execute (WriteArray ix x (Var (Concrete array))) =
      evalExceptT . hoist liftIO $ Array.write array ix x
  in
    Command gen execute [
        Require $ \s (WriteArray ix _ array) ->
          Just (fromIntegral ix) < fmap List.length (Map.lookup array $ stateArrays s)

      , Update $ \s (WriteArray ix x array) _o ->
          s {
            stateArrays =
              Map.update (Just . writeList ix x) array (stateArrays s)
          }

      , Ensure $ \_before after (WriteArray ix x array) _o ->
          readList ix (Map.lookup array $ stateArrays after) === Just x
      ]

------------------------------------------------------------------------
-- Grow

data GrowArray v =
  GrowArray ArrayLength (Var Array v)
  deriving (Eq, Show)

instance HTraversable GrowArray where
  htraverse f (GrowArray len array) =
    GrowArray len <$> htraverse f array

growList :: ArrayLength -> [Int64] -> [Int64]
growList len xs =
  let
    diff =
      max 0 (fromIntegral len - length xs)
  in
    List.take (fromIntegral len) $
      xs <> List.replicate diff 0

growArray :: (MonadGen n, MonadReader Mempool m, MonadIO m) => Command n m State
growArray =
  let
    gen s =
      case Map.toList $ stateArrays s of
        [] ->
          Nothing
        xs ->
          Just $ do
            (array, _vs) <- Gen.element xs
            GrowArray
              <$> (ArrayLength <$> Gen.int64 (Range.linear 0 100))
              <*> pure array

    execute (GrowArray len (Var (Concrete array))) = do
      pool <- ask
      liftIO $ Array.grow pool array len
  in
    Command gen execute [
        Update $ \s (GrowArray len array0) array ->
          s {
            stateArrays =
              case Map.lookup array0 (stateArrays s) of
                Nothing ->
                  stateArrays s
                Just xs ->
                  Map.insert array (growList len xs) $
                  Map.delete array0 (stateArrays s)
          }

      , Ensure $ \before after (GrowArray len array0) array -> do
          let
            ref =
              Var (Concrete array)

          old <- evalEither . maybeToRight () $ Map.lookup array0 (stateArrays before)
          new <- evalEither . maybeToRight () $ Map.lookup ref (stateArrays after)

          fromIntegral len === List.length new

          assert $
            old `List.isPrefixOf` new ||
            new `List.isPrefixOf` old
      ]

------------------------------------------------------------------------

prop_array_state_machine :: Property
prop_array_state_machine =
  withTests 1000 . property $ do
    actions <- forAll $
      Gen.sequential (Range.linear 1 100) initialState [
          newArray
        , readArray
        , writeArray
        , growArray
        ]

    test . runResourceT $ do
      pool <- newMempool
      executeSequential initialState actions `runReaderT` pool

------------------------------------------------------------------------

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
