{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Test.Runtime.Corpus where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Catch (MonadCatch, handleIf)
import           Control.Monad.Morph (hoist)

import           Data.IORef (IORef)
import           Data.String (IsString(..))
import qualified Data.IORef as IORef
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Boxed

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Common.Base
import           Icicle.Common.Eval
import           Icicle.Compiler.Sea
import           Icicle.Data.Fact
import           Icicle.Data.Name
import           Icicle.Data.Time
import           Icicle.Runtime.Data
import           Icicle.Runtime.Evaluator
import           Icicle.Test.Arbitrary
import           Icicle.Test.Arbitrary.Corpus
import           Icicle.Test.Arbitrary.NanEq (hedgehogNanEq)

import           P

import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import qualified Test.QuickCheck.Exception as QuickCheck


splitOutputs :: [Entity] -> Map OutputId [BaseValue] -> Map Entity [(OutputId, BaseValue)]
splitOutputs entities outputs =
  let
    flatten (oid, evs) =
      fmap (\(k, v) -> (k, [(oid, v)])) evs
  in
    Map.fromListWith (<>) . concatMap flatten . Map.toList $ fmap (List.zip entities) outputs

cacheRef :: IORef (Map CorpusId Runtime)
cacheRef =
  unsafePerformIO $ IORef.newIORef Map.empty
{-# NOINLINE cacheRef #-}

compileRuntime :: (MonadTest m, MonadIO m) => CorpusId -> WellTyped -> m Runtime
compileRuntime cid wt = do
  kvs <- liftIO $ IORef.readIORef cacheRef

  case Map.lookup cid kvs of
    Nothing -> do
      let
        acontext =
          AvalancheContext "Icicle.Test.Runtime.Corpus.compileRuntime" $
            Map.singleton [inputid|test:input|] (wtAvalancheFlat wt :| [])

      options0 <- getCompilerOptions

      let
        options = [
            "-DICICLE_ASSERT=1"
          , "-DICICLE_ASSERT_MAXIMUM_ARRAY_COUNT=1000000"
          , "-DICICLE_NOINLINE=1"
          , "-O0"
          ] <>
          options0

      ccontext <- evalEither $ compileAvalanche acontext
      runtime <- evalExceptT $ compileSeaWith options SkipJetskiCache ccontext

      liftIO . IORef.writeIORef cacheRef $ Map.insert cid runtime kvs

      pure runtime

    Just runtime -> do
      pure runtime

evalWellTypedRuntime :: (MonadTest m, MonadIO m) => Time -> MaximumMapSize -> CorpusId -> WellTyped -> m (Map Entity [(OutputId, BaseValue)])
evalWellTypedRuntime stime0 maxMapSize cid wt = do
  runtime <- compileRuntime cid wt

  let
    stime =
      SnapshotTime . QueryTime . Time64 $ packedOfTime stime0

  input <- evalEither $ fromInputs [inputid|test:input|] (wtInputType wt) (wtInputs wt)
  output0 <- evalExceptT . hoist liftIO $ snapshotBlock runtime maxMapSize stime input

  let
    entities =
      Boxed.toList . fmap (Entity . Text.decodeUtf8 . unEntityId . entityId . snapshotEntity) $ outputKey output0

  output1 <- evalEither $ traverse fromOutputs (outputColumns output0)

  pure $ splitOutputs entities output1

forceTombstone :: BaseValue -> BaseValue
forceTombstone = \case
  VLeft (VError _) ->
    VLeft (VError ExceptTombstone)
  x ->
    x

handleDiscard :: MonadCatch m => PropertyT m a -> PropertyT m a
handleDiscard =
  handleIf QuickCheck.isDiscard (const discard)

timeEnd :: Time
timeEnd =
  unsafeTimeOfYMD 3000 1 1

test_runtime_corpus :: CorpusId -> (InputName, OutputId, Text) -> Property
test_runtime_corpus cid query =
  property . handleDiscard $ do
    wt <- forAll . Gen.quickcheck $ genWellTypedFromSource query
    maxMapSize <- forAll $ Gen.int (Range.linear 1 100)
    stime <- forAll $ Gen.element (timeEnd : fmap atTime (concat . Map.elems $ wtInputs wt))

    let
      core_results0 =
        evalWellTyped (EvalContext stime maxMapSize) wt

    c_results0 <- evalWellTypedRuntime stime (MaximumMapSize $ fromIntegral maxMapSize) cid wt

    let
      core_results =
        fmap (fmap (second forceTombstone)) core_results0

      c_results =
        fmap (fmap (second forceTombstone)) c_results0

    core_results `hedgehogNanEq` c_results

mkPropertyName :: CorpusId -> Text -> PropertyName
mkPropertyName (CorpusId cid) source =
  case Text.lines source of
    [] ->
      fromString $ "#" <> show cid
    [x] ->
      fromString $ "#" <> show cid <> " (" <> Text.unpack (Text.strip x) <> ")"
    x : _ ->
      fromString $ "#" <> show cid <> " (" <> Text.unpack (Text.strip x) <> "...)"

return []
tests :: IO Bool
tests = do
  IORef.writeIORef cacheRef Map.empty
  checkParallel . Group "Icicle.Test.Runtime.Corpus" $
    with (List.zip [0..] corpusQueries) $ \(cid, query@(_, _, source)) ->
      (mkPropertyName cid source, test_runtime_corpus cid query)
