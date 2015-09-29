{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Context (
    Features (..)
  , FeatureContext

  , envOfFeatureContext
  , envOfFeatureNow
  ) where

import                  Icicle.Source.Type
import                  Icicle.Common.Base
import qualified        Icicle.Core as C

import                  P
import qualified        Data.Map as Map


data Features a n
 = Features
 { featuresConcretes :: Map.Map (Name n) (Type n, FeatureContext a n)
 , featuresFunctions :: Map.Map (Name n) (FunctionType n)
 , featureNow        :: Maybe (Name n)
 }

type FeatureContext a n
 = Map.Map (Name n) (Type n, C.Exp a n -> C.Exp a n)

envOfFeatureContext :: FeatureContext a n -> Map.Map (Name n) (Type n)
envOfFeatureContext ff
 = Map.map (\(t,_) -> Temporality TemporalityElement $ Possibility PossibilityDefinitely t)
 $ ff

envOfFeatureNow :: Ord n => Maybe (Name n) -> Map.Map (Name n) (Type n)
envOfFeatureNow
 = Map.fromList
 . maybeToList
 . fmap
   (\n -> (n, Temporality TemporalityAggregate $ Possibility PossibilityDefinitely DateTimeT))
