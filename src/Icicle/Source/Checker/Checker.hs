-- | Typecheck a Query, returning the annotated query and return type
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Checker (
    checkQT
  , checkQ
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Checker.Constraint
import                  Icicle.Source.Checker.Invariants
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.Query
import                  Icicle.Source.Type

import qualified        Icicle.Common.Fresh     as Fresh

import                  P

import                  Control.Monad.Trans.Either


import qualified        Data.Map as Map



type Result r a n = EitherT (CheckError a n) (Fresh.Fresh n) (r, Type n)


-- | Check a top-level Query, returning the query with type annotations and casts inserted.
checkQT :: Ord n
        => Features () n
        -> QueryTop a n
        -> Result (QueryTop (Annot a n) n) a n
checkQT features qt
 = case Map.lookup (feature qt) (featuresConcretes features) of
    Just (_,f)
     -> do  let env = fmap function0 $ envOfFeatureContext f
            let env' = env `Map.union` featuresFunctions features
            (q,t) <- checkQ (emptyCheckEnv { checkEnvironment = env' }) (query qt)
            return (qt { query = q }, t)

    Nothing
     -> hoistEither
      $ errorSuggestions (ErrorNoSuchFeature (annotOfQuery $ query qt) (feature qt))
                         [suggestionForFeatures]

 where
  suggestionForFeatures
   = AvailableFeatures (feature qt)
   $ fmap (\(k,(t,_)) -> (k, t))
   $ Map.toList
   $ featuresConcretes features


checkQ  :: Ord        n
        => CheckEnv a n
        -> Query    a n
        -> Result (Query (Annot a n) n) a n
checkQ ctx q
 = do q'  <- defaults <$> constraintsQ (checkEnvironment ctx) q

      let t = annResult $ annotOfQuery q'
      case getTemporalityOrPure t of
       TemporalityAggregate -> return ()
       _ -> hoistEither
          $ errorSuggestions (ErrorReturnNotAggregate (annotOfQuery $ q) t)
                             [Suggest "The return must be an aggregate, otherwise the result could be quite large"]

      hoistEither $ invariantQ ctx q

      return (q', t)

