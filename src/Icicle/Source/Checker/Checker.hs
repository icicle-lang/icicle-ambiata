-- | Typecheck a Query, returning the annotated query and return type
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Checker (
    checkQT
  , checkQ
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import qualified        Icicle.Source.Checker.Constraint as Constr
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
        => Features n
        -> QueryTop a n
        -> Result (QueryTop (a, Type n) n) a n
checkQT features qt
 = case Map.lookup (feature qt) features of
    Just (_,f)
     -> do  (q,t) <- checkQ (emptyCheckEnv { checkEnvironment = fmap function0 $ envOfFeatureContext f }) (query qt)
            return (qt { query = q }, t)

    Nothing
     -> hoistEither
      $ errorSuggestions (ErrorNoSuchFeature (feature qt))
                         [suggestionForFeatures]

 where
  suggestionForFeatures
   = AvailableFeatures
   $ fmap (\(k,(t,_)) -> (k, t))
   $ Map.toList features


checkQ  :: Ord        n
        => CheckEnv a n
        -> Query    a n
        -> Result (Query (a, Type n) n) a n
checkQ ctx q
 = do q'  <- Constr.checkQ (checkEnvironment ctx) q
      let q'a = reannotQ annotDiscardConstraints
              $ Constr.defaults q'

      let t = snd $ annotOfQuery q'a
      case getTemporalityOrPure t of
       TemporalityAggregate -> return ()
       _ -> hoistEither
          $ errorSuggestions (ErrorReturnNotAggregate (annotOfQuery $ q) t)
                             [Suggest "The return must be an aggregate, otherwise the result could be quite large"]

      hoistEither $ invariantQ ctx q

      return (q'a, snd $ annotOfQuery q'a)

