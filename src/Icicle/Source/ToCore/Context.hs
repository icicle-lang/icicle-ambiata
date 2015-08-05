{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Context (
    Features
  , FeatureContext

  , envOfFeatureContext
  ) where

import                  Icicle.Source.Type
import qualified        Icicle.Core as C

import                  P
import qualified        Data.Map as Map


type Features n
 = Map.Map n (BaseType n, FeatureContext n)

type FeatureContext n
 = Map.Map n (BaseType n, C.Exp n -> C.Exp n)

envOfFeatureContext :: FeatureContext n -> Map.Map n (UniverseType n)
envOfFeatureContext ff
 = Map.map (\(t,_) -> UniverseType (Universe Elem Definitely) t)
 $ ff

