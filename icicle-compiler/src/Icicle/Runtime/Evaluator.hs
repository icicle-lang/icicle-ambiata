{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Runtime.Evaluator (
    AvalancheContext(..)
  , SeaContext(..)
  , UseJetskiCache(..)

  , Runtime(..)
  , ClusterInfo(..)
  , KernelIO(..)
  , runtimeOutputSchema

  , compileAvalanche
  , compileSea

  , snapshotBlock
  , snapshotCluster

  , RuntimeError(..)
  , renderRuntimeError
  ) where

import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Exception.Lifted (bracket)
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr, plusPtr)
import           Foreign.Storable (Storable(..))

import           GHC.Generics (Generic)

import qualified Icicle.Avalanche.Prim.Flat as Avalanche
import qualified Icicle.Avalanche.Program as Avalanche
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Type (ValType(..))
import           Icicle.Data.Name
import           Icicle.Internal.Pretty (Pretty)
import qualified Icicle.Internal.Pretty as Pretty
import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Array as Array
import           Icicle.Runtime.Data.Schema (SchemaError)
import qualified Icicle.Runtime.Data.Schema as Schema
import           Icicle.Runtime.Data.Striped (StripedError)
import qualified Icicle.Runtime.Data.Striped as Striped
import           Icicle.Sea.Data
import qualified Icicle.Sea.Eval.Base as Sea
import qualified Icicle.Sea.FromAvalanche.State as Sea
import           Icicle.Sea.Header
import qualified Icicle.Sea.IO.Offset as Offset

import qualified Jetski as Jetski

import           P hiding (Any)

import           System.IO (IO)

import           Text.Show.Pretty (ppShow)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left)
import qualified X.Data.Vector.Cons as Cons
import           X.Text.Show (gshowsPrec)

import           Zebra.X.Vector.Segment (SegmentError)
import qualified Zebra.X.Vector.Segment as Segment -- FIXME move to x-vector

data RuntimeError =
    RuntimeSeaError !Sea.SeaError
  | RuntimeJetskiError !Jetski.JetskiError
  | RuntimeHeaderError !HeaderDecodeError
  | RuntimeSchemaError !SchemaError
  | RuntimeStripedError !StripedError
  | RuntimeSegmentError !SegmentError
  | RuntimeUnexpectedInputType !ValType
  | RuntimeClusterHadNoOutputs !ClusterId
  | RuntimeExpectedStructOutput !Schema
  | RuntimeInvalidOutputId !Text
  | RuntimeInputCountMismatch !Int !Int
  | RuntimeInputSchemaMismatch !Schema !Schema
  | RuntimeInputClusterMismatch !(Set InputId) !(Set InputId)
    deriving (Eq, Show)

renderRuntimeError :: RuntimeError -> Text
renderRuntimeError = \case
  RuntimeSeaError x ->
    Text.pack (show (Pretty.pretty x))

  RuntimeJetskiError (Jetski.CompilerError options _src ccout) ->
    "Failed to compile C code using options: " <> Text.pack (show options) <>
    "\n" <>
    "\n" <> ccout

  RuntimeJetskiError x ->
    Text.pack (show x)

  RuntimeHeaderError x ->
    renderHeaderDecodeError x

  RuntimeSchemaError x ->
    Schema.renderSchemaError x

  RuntimeStripedError x ->
    Striped.renderStripedError x

  RuntimeSegmentError x ->
    Segment.renderSegmentError x

  RuntimeUnexpectedInputType x ->
    "Expected (Sum Error x, Time) but found: " <> Text.pack (show x)

  RuntimeClusterHadNoOutputs x ->
    "Found a cluster with no outputs: " <> renderClusterId x

  RuntimeExpectedStructOutput x ->
    "Expected to find a struct containing the outputs, but found: " <> Text.pack (show x)

  RuntimeInvalidOutputId x ->
    "Failed to parse output-id: " <> x

  RuntimeInputCountMismatch expected actual ->
    "Expected <" <> Text.pack (show expected) <> "> inputs, but found <" <> Text.pack (show actual) <> ">"

  RuntimeInputSchemaMismatch expected actual ->
    "Input schema mismatch." <>
    "\n" <>
    "\n  expected =" <>
    "\n" <> Text.pack (indent4 (ppShow expected)) <>
    "\n" <>
    "\n  actual =" <>
    "\n" <> Text.pack (indent4 (ppShow actual))

  RuntimeInputClusterMismatch missingInputs missingClusters ->
    "Input mismatch." <>
    "\n" <>
    "\n  query clusters without a matching input =" <>
    "\n" <> Text.pack (indent4 (ppShow missingInputs)) <>
    "\n" <>
    "\n  inputs without a matching query cluster =" <>
    "\n" <> Text.pack (indent4 (ppShow missingClusters))

indent4 :: String -> String
indent4 =
  List.unlines . fmap ("    " <>) . List.lines

data AvalancheContext a n =
  AvalancheContext {
      avalancheFingerprint :: !Fingerprint
    , avalanchePrograms :: !(Map InputId (NonEmpty (Avalanche.Program (Annot a) n Avalanche.Prim)))
    } deriving (Eq, Show)

data SeaContext =
  SeaContext {
      seaCode :: !Text
    } deriving (Eq, Show)

data Runtime =
  Runtime {
      runtimeLibrary :: !Jetski.Library
    , runtimeClusters :: !(Map InputId (Cluster ClusterInfo KernelIO))
    }

instance Show Runtime where
 showsPrec p (Runtime _ clusters) =
   showParen (p > 10) $
     showString "Runtime " .
     showsPrec 11 clusters

data ClusterInfo =
  ClusterInfo {
      clusterStateSize :: !Int64
    , clusterInputSchema :: !Schema
    , clusterInputCount :: !Int
    , clusterOutputSchema :: !Schema
    , clusterOutputOffset :: !OutputOffset
    } deriving (Eq, Show)

data KernelIO =
  KernelIO {
      kernelIO :: forall void. Ptr void -> IO ()
    , kernelCount :: !OutputCount
    }

instance Show KernelIO where
 showsPrec p (KernelIO _ len) =
   showParen (p > 10) $
     showString "KernelIO " .
     showsPrec 11 len

newtype OutputOffset =
  OutputOffset {
      _unOutputOffset :: Int
    } deriving (Eq, Generic, Num)

instance Show OutputOffset where
  showsPrec =
    gshowsPrec

newtype OutputCount =
  OutputCount {
      _unOutputCount :: Int
    } deriving (Eq, Generic, Num)

instance Show OutputCount where
  showsPrec =
    gshowsPrec

resolveKernelIO :: MonadIO m => Jetski.Library -> Kernel a -> EitherT RuntimeError m (Kernel KernelIO)
resolveKernelIO library kernel = do
  compute <- firstT RuntimeJetskiError $
    Jetski.function library (Sea.nameOfKernel kernel) Jetski.retVoid

  let
    outputCount =
      fromIntegral . length $
        concatMap (typeMelted . snd) (kernelOutputs kernel)

  let
    kio =
      KernelIO (\ptr -> compute [Jetski.argPtr ptr]) outputCount

  pure $
    kernel {
        kernelAnnotation =
          kio
      }

resolveKernelSchemas :: Kernel k -> Either RuntimeError [(OutputId, Schema)]
resolveKernelSchemas =
  first RuntimeSchemaError .
  traverse (traverse (Schema.fromValType . typeLogical)) .
  kernelOutputs

resolveClusterSchemas :: Cluster c k -> Either RuntimeError [(OutputId, Schema)]
resolveClusterSchemas =
  fmap concat . traverse resolveKernelSchemas . clusterKernels

fromOutputField :: OutputId -> Schema -> Field Schema
fromOutputField oid schema =
  Field (renderOutputId oid) schema

resolveClusterOutputSchema :: Cluster c k -> Either RuntimeError Schema
resolveClusterOutputSchema c = do
  xs0 <- resolveClusterSchemas c
  case xs0 of
    [] ->
      Left $ RuntimeClusterHadNoOutputs (clusterId c)
    x : xs ->
      pure .
        Schema.Struct .
        fmap (uncurry fromOutputField) $
        Cons.fromNonEmpty (x :| xs)

resolveClusterInputSchema :: ValType -> Either RuntimeError Schema
resolveClusterInputSchema = \case
  PairT (SumT ErrorT x) TimeT ->
    first RuntimeSchemaError $ Schema.fromValType x
  x ->
    Left $ RuntimeUnexpectedInputType x

resolveClusterKernelIO :: MonadIO m => Jetski.Library -> Cluster c k -> EitherT RuntimeError m (Cluster ClusterInfo KernelIO)
resolveClusterKernelIO library cluster = do
  kernels <- traverse (resolveKernelIO library) (clusterKernels cluster)

  sizeOfState <- firstT RuntimeJetskiError $
    Jetski.function library (Sea.nameOfClusterStateSize cluster) Jetski.retInt64

  size <- liftIO $ sizeOfState []

  ischema <- hoistEither . resolveClusterInputSchema $ clusterInputType cluster
  oschema <- hoistEither $ resolveClusterOutputSchema cluster

  let
    icount =
      length (clusterInputVars cluster)

    ooffset =
      fromIntegral . Offset.programOutputStart . length $ clusterInputVars cluster

    info =
      ClusterInfo size ischema icount oschema ooffset

  pure $
    cluster {
        clusterKernels =
          kernels

      , clusterAnnotation =
          info
      }

compileAvalanche :: (Show a, Show n, Pretty n, Eq n) => AvalancheContext a n -> Either RuntimeError SeaContext
compileAvalanche context =
  let
    fingerprint =
      avalancheFingerprint context

    programs =
      Map.toList $ avalanchePrograms context
  in
    fmap SeaContext . first RuntimeSeaError $
      Sea.codeOfPrograms fingerprint Sea.NoInput programs

fromUseJetskiCache :: UseJetskiCache -> Jetski.CacheLibrary
fromUseJetskiCache = \case
  SkipJetskiCache ->
    Jetski.NoCacheLibrary
  UseJetskiCache ->
    Jetski.CacheLibrary

compileSea :: MonadIO m => UseJetskiCache -> SeaContext -> EitherT RuntimeError m Runtime
compileSea cache context = do
  options <- Sea.getCompilerOptions
  (header, code) <- hoistEither . first RuntimeHeaderError . parseHeader $ seaCode context

  library <-
    firstT RuntimeJetskiError $
      Jetski.compileLibrary (fromUseJetskiCache cache) options code

  clusters0 <- traverse (resolveClusterKernelIO library) (headerClusters header)

  let
    clusters =
      Map.fromList $ fmap (\x -> (clusterInputId x, x)) clusters0

  pure $
    Runtime library clusters

pokeWordOff :: (MonadIO m, Storable a) => Ptr x -> Int -> a -> m ()
pokeWordOff ptr off x =
  liftIO $! pokeByteOff ptr (off * 8) x

peekOutputs :: MonadIO m => Ptr x -> OutputOffset -> OutputCount -> m [Any64]
peekOutputs ptr (OutputOffset off) (OutputCount len) = do
  fp <- liftIO $! newForeignPtr_ (ptr `plusPtr` (off * 8))

  let
    !xs =
      Storable.toList $! Storable.unsafeFromForeignPtr0 fp len

  length xs `seq` pure xs

mkOutputColumns :: Column -> Either RuntimeError (Map OutputId Column)
mkOutputColumns = \case
  Striped.Struct fields ->
    fmap Map.fromList . for (Cons.toList fields) $ \(Field k v) ->
      (,)
        <$> maybeToRight (RuntimeInvalidOutputId k) (parseOutputId k)
        <*> pure v
  x ->
    Left $ RuntimeExpectedStructOutput (Striped.schema x)

mkOutputSchemas :: Schema -> Either RuntimeError (Map OutputId Schema)
mkOutputSchemas = \case
  Schema.Struct fields ->
    fmap Map.fromList . for (Cons.toList fields) $ \(Field k v) ->
      (,)
        <$> maybeToRight (RuntimeInvalidOutputId k) (parseOutputId k)
        <*> pure v
  x ->
    Left $ RuntimeExpectedStructOutput x

runtimeOutputSchema :: Runtime -> Either RuntimeError (Map OutputId Schema)
runtimeOutputSchema =
  fmap Map.unions .
  traverse (mkOutputSchemas . clusterOutputSchema . clusterAnnotation) .
  Map.elems .
  runtimeClusters

resolveNewCount :: SnapshotTime -> Storable.Vector Int64 -> Storable.Vector Time64 -> Either RuntimeError (Storable.Vector Int64)
resolveNewCount (SnapshotTime stime) ns ts = do
  tss <- first RuntimeSegmentError $ Segment.reify ns ts
  pure . Storable.convert $
    Boxed.map (fromIntegral . Storable.length . Storable.takeWhile (< stime)) tss

snapshotCluster ::
     Cluster ClusterInfo KernelIO
  -> MaximumMapSize
  -> SnapshotTime
  -> InputColumn
  -> EitherT RuntimeError IO (Map OutputId Column)
snapshotCluster cluster maxMapSize stime input =
  let
    size =
      clusterStateSize $ clusterAnnotation cluster

    expectedInputSchema =
      clusterInputSchema $ clusterAnnotation cluster

    inputData =
      Striped.Pair
        (Striped.Result (inputTombstone input) (inputColumn input))
        (Striped.Time (inputTime input))

    inputSchema =
      Striped.schema $ inputColumn input

    inputCount =
      clusterInputCount $ clusterAnnotation cluster

    outputSchema =
      clusterOutputSchema $ clusterAnnotation cluster

    outputOffset =
      clusterOutputOffset $ clusterAnnotation cluster

    outputCount =
      Foldable.sum . fmap (kernelCount . kernelAnnotation) $ clusterKernels cluster
  in
    if inputSchema /= expectedInputSchema then
      left $ RuntimeInputSchemaMismatch expectedInputSchema inputSchema
    else
      bracket (liftIO Mempool.create) (liftIO . Mempool.free) $ \pool -> do
        arrays <- bimapT RuntimeStripedError Boxed.fromList $
          Striped.toArrays pool inputData

        let
          !n_arrays =
            Boxed.length arrays

        when (inputCount /= n_arrays) $
          left $ RuntimeInputCountMismatch inputCount n_arrays

        ncounts <- hoistEither $ resolveNewCount stime (inputLength input) (inputTime input)

        let
          offsets =
            Storable.prescanl' (\off n -> off + fromIntegral n * 8) 0 ncounts

          -- This loops runs once per entity
          computeEntity offset ncount = do
            pState <- liftIO $ Mempool.callocBytes pool (fromIntegral size) 1
            pokeWordOff pState Offset.programMempool pool
            pokeWordOff pState Offset.programMaxMapSize maxMapSize
            pokeWordOff pState Offset.programInputQueryTime stime
            pokeWordOff pState Offset.programInputNewCount (ncount :: Int64)

            flip Boxed.imapM_ arrays $ \ix array ->
              let
                !ptr =
                  Array.unsafeElementPtr array `plusPtr` offset
              in
                pokeWordOff pState (Offset.programInputError + ix) ptr

            liftIO . for_ (clusterKernels cluster) $ \kernel ->
              kernelIO (kernelAnnotation kernel) pState

            outputs <- peekOutputs pState outputOffset outputCount
            firstT RuntimeStripedError $ Striped.fromAnys pool outputSchema outputs

        columns <- Boxed.zipWithM computeEntity (Boxed.convert offsets) (Boxed.convert ncounts)

        case Cons.fromVector columns of
          Nothing ->
            left $ RuntimeClusterHadNoOutputs (clusterId cluster)
          Just xss0 -> do
            xss <- hoistEither . first RuntimeStripedError $ Striped.unsafeConcat xss0
            hoistEither $ mkOutputColumns xss

snapshotBlock :: Runtime -> MaximumMapSize -> SnapshotTime -> Input -> EitherT RuntimeError IO (Output SnapshotKey)
snapshotBlock runtime maxsize stime input =
  let
    eids =
      inputKey input

    keys =
      fmap SnapshotKey eids

    clusters =
      runtimeClusters runtime

    inputs =
      inputColumns input

    both =
      Map.intersectionWith (,) clusters inputs

    missingInputs =
      clusters `Map.difference` inputs

    missingClusters =
      inputs `Map.difference` clusters
  in
    if Map.size both /= Map.size inputs then
      left $ RuntimeInputClusterMismatch (Map.keysSet missingInputs) (Map.keysSet missingClusters)
    else do
      fmap (Output keys . Map.unions) . for (Map.elems both) $ \(cluster, column) ->
        snapshotCluster cluster maxsize stime column

------------------------------------------------------------------------
-- Skeleton for generated cluster state
--
-- typedef struct {
--     /* runtime */
--     anemone_mempool_t *mempool;
--     iint_t             max_map_size;
--
--     /* input */
--     itime_t   input_query_time;
--     iint_t    input_count;
--     ierror_t  *input_tombstone;
--     iany_t    *input_melted0;
--     iany_t    *input_melted1;
--     iany_t    *input_melted2;
--     ..
--     iany_t    *input_meltedN;
--     itime_t   *input_time;
--
--     /* kernel 0:0 outputs */
--     iany_t     output0_melted0;
--     iany_t     output0_melted1;
--     ..
--     iany_t     output0_meltedN;
--
--     /* kernel 0:1 outputs */
--     iany_t     output1_melted0;
--     iany_t     output1_melted1;
--     ..
--     iany_t     output1_meltedN;
--
--     /* kernel 0:0 flags */
--     ibool_t has_flags_start_0_0;
--     ibool_t has_0_0_melted0;
--     ibool_t has_0_0_melted1;
--     ..
--     ibool_t has_0_0_meltedN;
--     ibool_t has_flags_end_0_0;
--
--     /* kernel 0:1 flags */
--     ibool_t has_flags_start_0_1;
--     ibool_t has_0_1_melted0;
--     ibool_t has_0_1_melted1;
--     ..
--     ibool_t has_0_1_meltedN;
--     ibool_t has_flags_end_0_1;
--
--     /* kernel 0:0 resumables */
--     ibuf_1_any_t res_0_0_melted0;
--     ibuf_1_any_t res_0_0_melted1;
--     ..
--     ibuf_1_any_t res_0_0_meltedN;
--
--     /* kernel 0:1 resumables */
--     ibuf_1_any_t res_0_1_melted0;
--     ibuf_1_any_t res_0_1_melted1;
--     ..
--     ibuf_1_any_t res_0_1_meltedN;
-- } cluster_0_t;
--
