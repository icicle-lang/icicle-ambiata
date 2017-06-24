{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Context (
    Features (..)
  , FeatureConcrete (..)
  , FeatureContext (..)
  , FeatureVariable (..)
  , FeatureKey (..)

  , envOfFeatureContext
  , envOfFeatureNow
  , typeOfFeatureVariable
  ) where

import                  Icicle.Common.Base

import qualified        Icicle.Core as C

import                  Icicle.Data.Name

import                  Icicle.Source.Type
import                  Icicle.Source.Checker.Base

import                  P

import                  Data.Map (Map)
import qualified        Data.Map as Map


data Features a n k
 = Features
 { featuresConcretes :: Map InputId (FeatureConcrete a n k)
 , featuresFunctions :: Map   (Name n) (FunctionType n)
 , featureNow        :: Maybe (Name n)
 }

data FeatureConcrete a n k
 = FeatureConcrete
 { featureConcreteKey     :: k
 , featureConcreteType    :: Type n
 , featureConcreteContext :: FeatureContext a n
 }

data FeatureContext a n
 = FeatureContext
 { featureContextVariables :: Map (Name n) (FeatureVariable a n)
 , featureContextFactTime  :: Name n
 }

data FeatureVariable a n
 = FeatureVariable
 { featureVariableType     :: Type n
 , featureVariableExp      :: C.Exp a n -> C.Exp a n
 , featureVariablePossibly :: Bool
 }

newtype FeatureKey a n = FeatureKey {
   featureKey :: Maybe (C.Exp a n)
 }

envOfFeatureContext :: FeatureContext a n -> Map (Name n) (Type n)
envOfFeatureContext ff
 = Map.map typeOfFeatureVariable
 $ featureContextVariables ff

typeOfFeatureVariable :: FeatureVariable a n -> Type n
typeOfFeatureVariable fv
 = Temporality TemporalityElement
 $ Possibility (if featureVariablePossibly fv then PossibilityPossibly else PossibilityDefinitely)
 $ featureVariableType fv

envOfFeatureNow :: Eq n => CheckOptions -> Maybe (Name n) -> Map (Name n) (Type n)
envOfFeatureNow opts
 = Map.fromList
 . maybeToList
 . fmap
   (\n -> (n, Temporality tmp $ Possibility PossibilityDefinitely TimeT))
 where
   tmp
    | checkOptionNowPure opts = TemporalityPure
    | otherwise               = TemporalityAggregate
