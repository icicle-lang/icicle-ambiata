{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Context (
    Features (..)
  , FeatureConcrete (..)
  , FeatureContext (..)
  , FeatureVariable (..)

  , envOfFeatureContext
  , envOfFeatureNow
  , typeOfFeatureVariable
  ) where

import                  Icicle.Source.Type
import                  Icicle.Source.Checker.Base
import                  Icicle.Common.Base
import qualified        Icicle.Core as C

import                  Icicle.Data

import                  P
import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)


data Features a n
 = Features
 { featuresConcretes :: Map.Map (Name n) (FeatureConcrete a n)
 , featuresFunctions :: Map.Map (Name n) (FunctionType n)
 , featureNow        :: Maybe (Name n)
 }

data FeatureConcrete a n
 = FeatureConcrete
 { featureConcreteType    :: Type n
 , featureConcreteMode    :: FactMode
 , featureConcreteContext :: FeatureContext a n
 }

data FeatureContext a n
 = FeatureContext
 { featureContextVariables :: Map.Map (Name n) (FeatureVariable a n)
 , featureContextFactTime  :: Name n
 }

data FeatureVariable a n
 = FeatureVariable
 { featureVariableType :: Type n
 , featureVariableExp  :: C.Exp a n -> C.Exp a n
 , featureVariablePossibly :: Bool
 }

envOfFeatureContext :: FeatureContext a n -> Map.Map (Name n) (Type n)
envOfFeatureContext ff
 = Map.map typeOfFeatureVariable
 $ featureContextVariables ff

typeOfFeatureVariable :: FeatureVariable a n -> Type n
typeOfFeatureVariable fv
 = Temporality TemporalityElement
 $ Possibility (if featureVariablePossibly fv then PossibilityPossibly else PossibilityDefinitely)
 $ featureVariableType fv

envOfFeatureNow :: (Hashable n, Eq n) => CheckOptions -> Maybe (Name n) -> Map.Map (Name n) (Type n)
envOfFeatureNow opts
 = Map.fromList
 . maybeToList
 . fmap
   (\n -> (n, Temporality tmp $ Possibility PossibilityDefinitely TimeT))
 where
   tmp
    | checkOptionNowPure opts = TemporalityPure
    | otherwise               = TemporalityAggregate
