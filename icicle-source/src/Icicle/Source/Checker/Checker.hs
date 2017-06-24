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
import                  Icicle.Source.Checker.Resumable
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.Query
import                  Icicle.Source.Type
import                  Icicle.Source.Lexer.Token

import                  Icicle.Data.Name

import                  Icicle.Dictionary.Data

import qualified        Icicle.Common.Fresh     as Fresh

import                  P

import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)

import                  X.Control.Monad.Trans.Either



type Result r a n = EitherT (CheckError a n) (Fresh.Fresh n) (r, Type n)


-- | Check a top-level Query, returning the query with type annotations and casts inserted.
checkQT :: (Hashable n, Eq n)
        => CheckOptions
        -> Features () n (InputKey ann Variable)
        -> QueryTop a n
        -> Result (QueryTop (Annot a n) n) a n
checkQT opts features qt
  = case lookupInputId (queryInput qt) (featuresConcretes features) of
    Just (FeatureConcrete _ _ f)
     -> do  let env = Map.unions
                      [ fmap function0 (envOfFeatureContext f)
                      , featuresFunctions features
                      , fmap function0 (envOfFeatureNow opts (featureNow features)) ]
            (q,t) <- checkQ opts (emptyCheckEnv { checkEnvironment = env }) (query qt)
            return (qt { query = q }, t)

    Nothing
     -> hoistEither
      $ errorSuggestions (ErrorNoSuchInput (annotOfQuery $ query qt) (queryInput qt))
                         [suggestionForFeatures]

 where
  suggestionForFeatures
   = AvailableFeatures (queryInput qt)
   $ fmap (\(k, FeatureConcrete _ t _) -> (k, t))
   $ Map.toList
   $ featuresConcretes features


checkQ  :: (Hashable n, Eq n)
        => CheckOptions
        -> CheckEnv a n
        -> Query    a n
        -> Result (Query (Annot a n) n) a n
checkQ opts ctx q
 = do q'  <- defaults <$> constraintsQ (checkEnvironment ctx) q

      let t = annResult $ annotOfQuery q'
      case getTemporalityOrPure t of
       TemporalityAggregate
         -> return ()
       TemporalityPure
         -> return ()
       _
         -> hoistEither
          $ errorSuggestions
              (ErrorReturnNotAggregate (annotOfQuery $ q) t)
              [Suggest "The return must be an aggregate, otherwise the result could be quite large"]

      hoistEither $ invariantQ ctx q

      when (checkOptionRequireResumable opts) $
        hoistEither $ checkResumableQ ctx q

      return (q', t)

