{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Context (
    Features (..)
  , FeatureContext
  , FeatureVariable (..)

  , envOfFeatureContext
  , envOfFeatureNow
  , typeOfFeatureVariable
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
 = Map.Map (Name n) (FeatureVariable a n)

data FeatureVariable a n
 = FeatureVariable
 { featureVariableType :: Type n
 , featureVariableExp  :: C.Exp a n -> C.Exp a n
 , featureVariablePossibly :: Bool
 }

envOfFeatureContext :: FeatureContext a n -> Map.Map (Name n) (Type n)
envOfFeatureContext ff
 = Map.map typeOfFeatureVariable ff

typeOfFeatureVariable :: FeatureVariable a n -> Type n
typeOfFeatureVariable fv
 = Temporality TemporalityElement
 $ Possibility (if featureVariablePossibly fv then PossibilityPossibly else PossibilityDefinitely)
 $ featureVariableType fv

envOfFeatureNow :: Ord n => Maybe (Name n) -> Map.Map (Name n) (Type n)
envOfFeatureNow
 = Map.fromList
 . maybeToList
 . fmap
   (\n -> (n, Temporality TemporalityAggregate $ Possibility PossibilityDefinitely TimeT))
