{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Data (
    Cluster(..)
  , ClusterId(..)
  , clusterOutputs
  , renderClusterId
  , prettyClusterId

  , Kernel(..)
  , KernelId(..)
  , KernelIndex(..)
  , renderKernelId
  , prettyKernelId
  , renderKernelIndex
  , prettyKernelIndex

  , MeltedType(..)
  , UseJetskiCache(..)
  ) where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import           Icicle.Common.Type
import           Icicle.Data.Name
import           Icicle.Internal.Pretty
import           Icicle.Sea.Name

import           P

import           X.Text.Show (gshowsPrec)


data Cluster c k =
  Cluster {
      clusterId :: !ClusterId
    , clusterInputId :: !InputId
    , clusterInputType :: !ValType
    , clusterInputVars :: ![(SeaName, ValType)]
    , clusterTimeVar :: !SeaName
    , clusterKernels :: !(NonEmpty (Kernel k))
    , clusterAnnotation :: !c
    } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data Kernel k =
  Kernel {
      kernelId :: !KernelId
    , kernelOutputs :: ![(OutputId, MeltedType)]
    , kernelAnnotation :: !k
    } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

newtype ClusterId =
  ClusterId {
      unClusterId :: Int
    } deriving (Eq, Ord, Generic, Num, Enum, Real, Integral)

newtype KernelIndex =
  KernelIndex {
      unKernelIndex :: Int
    } deriving (Eq, Ord, Generic, Num, Enum, Real, Integral)

data KernelId =
  KernelId {
      kernelCluster :: !ClusterId
    , kernelIndex :: !KernelIndex
    } deriving (Eq, Ord, Generic)

data MeltedType =
  MeltedType {
      typeLogical :: !ValType
    , typeMelted :: ![ValType]
    } deriving (Eq, Ord, Show, Generic)

data UseJetskiCache =
    SkipJetskiCache
  | UseJetskiCache
    deriving (Eq, Ord, Show)

instance Show ClusterId where
  showsPrec =
    gshowsPrec

instance Show KernelIndex where
  showsPrec =
    gshowsPrec

instance Show KernelId where
  showsPrec =
    gshowsPrec

clusterOutputs :: Cluster c k -> Map OutputId MeltedType
clusterOutputs =
  Map.fromList . concatMap kernelOutputs . NonEmpty.toList . clusterKernels

renderClusterId :: ClusterId -> Text
renderClusterId =
  Text.pack . show . unClusterId

prettyClusterId :: ClusterId -> Doc
prettyClusterId =
  text . show . unClusterId

renderKernelIndex :: KernelIndex -> Text
renderKernelIndex =
  Text.pack . show . unKernelIndex

prettyKernelIndex :: KernelIndex -> Doc
prettyKernelIndex =
  text . show . unKernelIndex

renderKernelId :: KernelId -> Text
renderKernelId (KernelId cid kix) =
  renderClusterId cid <> ":" <> renderKernelIndex kix

prettyKernelId :: KernelId -> Doc
prettyKernelId =
  text . Text.unpack . renderKernelId
