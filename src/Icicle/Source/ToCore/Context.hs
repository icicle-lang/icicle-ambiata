{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Context (
    Features (..)
  , FeatureContext

  , envOfFeatureContext
  ) where

import                  Icicle.Source.Type
import                  Icicle.Common.Base
import qualified        Icicle.Core as C

import                  P
import qualified        Data.Map as Map


data Features n
 = Features
 { featuresConcretes :: Map.Map (Name n) (Type n, FeatureContext n)
 , featuresFunctions :: Map.Map (Name n) (FunctionType n)
 }

type FeatureContext n
 = Map.Map (Name n) (Type n, C.Exp n -> C.Exp n)

envOfFeatureContext :: FeatureContext n -> Map.Map (Name n) (Type n)
envOfFeatureContext ff
 = Map.map (\(t,_) -> Temporality TemporalityElement $ Possibility PossibilityDefinitely t)
 $ ff

