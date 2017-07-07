{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Runtime.Evaluator (
    AvalancheContext(..)
  , SeaContext(..)

  , Runtime(..)
  , KernelIO(..)
  , RuntimeError(..)

  , compileAvalanche
  , compileSea
  , snapshot

  -- * Internal
  , clusterSnapshot
  ) where

import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Exception.Lifted (bracket)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(..), evalStateT, get, put)

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr, plusPtr)
import           Foreign.Storable (Storable(..))

import           GHC.Generics (Generic)

import qualified Icicle.Avalanche.Prim.Flat as Avalanche
import qualified Icicle.Avalanche.Program as Avalanche
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base ()
import           Icicle.Data.Name
import           Icicle.Internal.Pretty (Pretty)
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

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left)
import qualified X.Data.Vector.Cons as Cons
import           X.Text.Show (gshowsPrec)


data RuntimeError =
    RuntimeSeaError !Sea.SeaError
  | RuntimeJetskiError !Jetski.JetskiError
  | RuntimeHeaderError !HeaderDecodeError
  | RuntimeSchemaError !SchemaError
  | RuntimeStripedError !StripedError
  | RuntimeInputCountMismatch !Int !Int
  | RuntimeInputSchemaMismatch !Schema !Schema
  | RuntimeClusterHadNoOutputs !ClusterId
  | RuntimeExpectedStructOutput !Schema
  | RuntimeInvalidOutputId !Text
  | RuntimeInputClusterMismatch !(Set InputId) !(Set InputId)
    deriving (Eq, Show)

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
    , clusterOutputSchema :: !Schema
    } deriving (Eq, Show)

data KernelIO =
  KernelIO {
      kernelIO :: forall void. Ptr void -> IO ()
    , kernelOffset :: !OutputOffset
    , kernelCount :: !OutputCount
    }

instance Show KernelIO where
 showsPrec p (KernelIO _ off len) =
   showParen (p > 10) $
     showString "KernelIO " .
     showsPrec 11 off .
     showChar ' ' .
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
--     /*** Kernel 0:0 ***/
--
--     /* outputs */
--     iany_t     output_melted0;
--     iany_t     output_melted1;
--     ..
--     iany_t     output_meltedN;
--
--     /* resumables: values */
--     ibuf_1_any_t res_0_0_melted0;
--     ibuf_1_any_t res_0_0_melted1;
--     ..
--     ibuf_1_any_t res_0_0_meltedN;
--
--     /* resumables: has flags */
--     ibool_t has_flags_start_0_0;
--     ibool_t has_0_0_melted0;
--     ibool_t has_0_0_melted1;
--     ..
--     ibool_t has_0_0_meltedN;
--     ibool_t has_flags_end_0_0;
--
--     /*** Kernel 0:1 ***/
--
--     ..
-- }

resolveKernelIO :: MonadIO m => Jetski.Library -> Kernel a -> StateT OutputOffset (EitherT RuntimeError m) (Kernel KernelIO)
resolveKernelIO library kernel = do
  compute <- lift . firstT RuntimeJetskiError $
    Jetski.function library (Sea.nameOfKernel kernel) Jetski.retVoid

  let
    outputCount =
      length $
        concatMap (typeMelted . snd) (kernelOutputs kernel)

    resumeableCount =
      length $
        kernelResumables kernel

    sentinelCount = -- has_flags_start / has_flags_end
      2

  offset <- get
  put $
    offset +
    fromIntegral outputCount +
    fromIntegral resumeableCount +
    sentinelCount

  let
    kio =
      KernelIO (\ptr -> compute [Jetski.argPtr ptr]) offset (fromIntegral outputCount)

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

resolveClusterSchema :: Cluster c k -> Either RuntimeError Schema
resolveClusterSchema c = do
  xs0 <- resolveClusterSchemas c
  case xs0 of
    [] ->
      Left $ RuntimeClusterHadNoOutputs (clusterId c)
    x : xs ->
      pure .
        Schema.Struct .
        fmap (uncurry fromOutputField) $
        Cons.fromNonEmpty (x :| xs)

resolveClusterKernelIO :: MonadIO m => Jetski.Library -> Cluster c k -> EitherT RuntimeError m (Cluster ClusterInfo KernelIO)
resolveClusterKernelIO library cluster = do
  let
    outputStart =
      fromIntegral . Offset.programOutputStart . length $
        clusterInputVars cluster

  kernels <- flip evalStateT outputStart $ traverse (resolveKernelIO library) (clusterKernels cluster)

  sizeOfState <- firstT RuntimeJetskiError $
    Jetski.function library (Sea.nameOfClusterStateSize cluster) Jetski.retInt64

  size <- liftIO $ sizeOfState []

  ischema <- hoistEither . first RuntimeSchemaError . Schema.fromValType $ clusterInputType cluster
  oschema <- hoistEither $ resolveClusterSchema cluster

  let
    info =
      ClusterInfo size ischema oschema

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
      Sea.codeOfPrograms fingerprint Sea.NoInput [] programs

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

clusterSnapshot ::
     Cluster ClusterInfo KernelIO
  -> MaximumMapSize
  -> SnapshotTime
  -> InputColumn
  -> EitherT RuntimeError IO (Map OutputId Column)
clusterSnapshot cluster maxMapSize stime input =
  let
    size =
      clusterStateSize $ clusterAnnotation cluster

    expectedInputSchema =
      clusterInputSchema $ clusterAnnotation cluster

    outputSchema =
      clusterOutputSchema $ clusterAnnotation cluster

    inputData =
      Striped.Pair
        (inputColumn input)
        (Striped.Time (inputTime input))

    inputSchema =
      Striped.schema inputData

    !clusterInputCount =
      length (clusterInputVars cluster)
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

        when (clusterInputCount /= n_arrays) $
          left $ RuntimeInputCountMismatch clusterInputCount n_arrays

        let
          loop (!offset0, dl) !ncount = do
            pState <- liftIO $ Mempool.callocBytes pool (fromIntegral size) 1
            pokeWordOff pState Offset.programMempool pool
            pokeWordOff pState Offset.programMaxMapSize maxMapSize
            pokeWordOff pState Offset.programInputQueryTime stime
            pokeWordOff pState Offset.programInputNewCount (ncount :: Int64)

            flip Boxed.imapM_ arrays $ \ix array ->
              let
                !ptr =
                  Array.unsafeElementPtr array `plusPtr` offset0
              in
                pokeWordOff pState (Offset.programInputError + ix) ptr

            let
              !offset =
                offset0 + fromIntegral ncount * 8

            outputs <-
              liftIO . fmap concat . for (clusterKernels cluster) $ \kernel -> do
                let
                  kio =
                    kernelAnnotation kernel

                kernelIO kio pState
                peekOutputs pState (kernelOffset kio) (kernelCount kio)

            column <- firstT RuntimeStripedError $ Striped.fromAnys pool outputSchema outputs

            pure (offset, dl . (column :))

        (_, dl) <- Storable.foldM loop (0, id) (inputLength input)

        case Cons.fromList $ dl [] of
          Nothing ->
            left $ RuntimeClusterHadNoOutputs (clusterId cluster)
          Just xss0 -> do
            xss <- hoistEither . first RuntimeStripedError $ Striped.unsafeConcat xss0
            hoistEither $ mkOutputColumns xss

snapshot :: Runtime -> MaximumMapSize -> SnapshotTime -> Input -> EitherT RuntimeError IO (Output SnapshotKey)
snapshot runtime maxsize stime input =
  let
    eids =
      inputEntity input

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
    else
      fmap (Output keys . Map.unions) . for (Map.elems both) $ \(cluster, column) ->
        clusterSnapshot cluster maxsize stime column
