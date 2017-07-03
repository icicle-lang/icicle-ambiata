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
  ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map

import           Foreign.Ptr (Ptr)

import qualified Icicle.Avalanche.Prim.Flat as Avalanche
import qualified Icicle.Avalanche.Program as Avalanche
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base ()
import           Icicle.Data.Name
import           Icicle.Internal.Pretty (Pretty)
import           Icicle.Sea.Data
import qualified Icicle.Sea.Eval.Base as Sea
import qualified Icicle.Sea.FromAvalanche.State as Sea
import           Icicle.Sea.Header

import qualified Jetski as Jetski

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)


data RuntimeError =
    RuntimeSeaError !Sea.SeaError
  | RuntimeJetskiError !Jetski.JetskiError
  | RuntimeHeaderError !HeaderDecodeError
    deriving (Eq, Show)

data AvalancheContext a n =
  AvalancheContext {
      avalancheFingerprint :: !Fingerprint
    , avalanchePrograms :: !(Map InputId (NonEmpty (Avalanche.Program (Annot a) n Avalanche.Prim)))
    }

data SeaContext =
  SeaContext {
      seaCode :: !Text
    }

data Runtime =
  Runtime {
      runtimeLibrary :: !Jetski.Library
    , runtimeClusters :: !(Map InputId Cluster)
    , runtimeKernels :: !(Map KernelId KernelIO)
    }

data KernelIO =
  KernelIO {
      kernelIO :: forall state. Ptr state -> IO ()
    }

compileAvalanche :: (Show a, Show n, Pretty n, Eq n) => AvalancheContext a n -> EitherT RuntimeError IO SeaContext
compileAvalanche context =
  let
    fingerprint =
      icicleFingerprint context

    programs =
      Map.toList $ iciclePrograms context
  in
    fmap SeaContext . hoistEither . first RuntimeSeaError $
      Sea.codeOfPrograms fingerprint Sea.NoInput [] programs

findKernel :: Jetski.Library -> KernelId -> EitherT RuntimeError IO KernelIO
findKernel library kid = do
  compute <- firstT RuntimeJetskiError $ Jetski.function library (Sea.nameOfKernel' kid) Jetski.retVoid
  pure $ KernelIO $ \ptr ->
    compute [Jetski.argPtr ptr]

findClusterKernels :: Jetski.Library -> Cluster -> EitherT RuntimeError IO (Map KernelId KernelIO)
findClusterKernels library =
  let
    fromKid kid =
      (kid, ) <$> findKernel library kid
  in
    fmap Map.fromList . traverse fromKid . fmap kernelId . toList . clusterKernels

compileSea :: SeaContext -> EitherT RuntimeError IO Runtime
compileSea context = do
  options <- Sea.getCompilerOptions
  (header, code) <- hoistEither . first RuntimeHeaderError . parseHeader $ seaCode context

  library <-
    firstT RuntimeJetskiError $
      Jetski.compileLibrary Jetski.CacheLibrary options code

  let
    clusters0 =
      headerClusters header

    clusters =
      Map.fromList $ fmap (\x -> (clusterInputId x, x)) clusters0

  kernels <- Map.unions <$> traverse (findClusterKernels library) clusters0

  pure $
    Runtime library clusters kernels
