{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Data (
    Cluster(..)
  , ClusterId(..)
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
  ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import           Icicle.Common.Type
import           Icicle.Data.Name
import           Icicle.Internal.Pretty
import           Icicle.Sea.Name

import           P

import           X.Text.Show (gshowsPrec)


data Cluster =
  Cluster {
      clusterId :: !ClusterId
    , clusterInputId :: !InputId
    , clusterInputType :: !ValType
    , clusterInputVars :: ![(SeaName, ValType)]
    , clusterTimeVar :: !SeaName
    , clusterKernels :: !(NonEmpty Kernel)
    , clusterOutputs :: !(Map OutputId MeltedType)
    } deriving (Eq, Ord, Show, Generic)

data Kernel =
  Kernel {
      kernelId :: !KernelId
    , kernelResumables :: ![(SeaName, ValType)]
    , kernelOutputs :: ![(OutputId, MeltedType)]
    } deriving (Eq, Ord, Show, Generic)

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

instance Show ClusterId where
  showsPrec =
    gshowsPrec

instance Show KernelIndex where
  showsPrec =
    gshowsPrec

instance Show KernelId where
  showsPrec =
    gshowsPrec

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
  renderClusterId cid <> "/" <> renderKernelIndex kix

prettyKernelId :: KernelId -> Doc
prettyKernelId =
  text . Text.unpack . renderKernelId
