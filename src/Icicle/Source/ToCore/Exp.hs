-- | Convert Element Expressions to Core
--
-- Worker functions, eg the "value * 2" in "sum (value * 2)".
-- Aggregates are dealt with elsewhere.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Exp (
    convertExp
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Common.Exp           as CE
import                  Icicle.Common.Base


import                  P

import                  Data.List (zip)

import qualified        Data.Map as Map



-- | Convert an element-level expression.
-- These are worker functions for folds, filters and so on.
convertExp
        :: Ord n
        => FeatureContext n
        -> Exp (a,UniverseType) n
        -> ConvertM a n (C.Exp n)
convertExp fs x
 | Var _ v <- x
 , Just (_, x') <- Map.lookup v fs
 = (x' . CE.XVar) <$> convertInputName

 -- Primitive application: convert arguments, then convert primitive
 | Just (p, (ann,retty), args) <- takePrimApps x
 = do   args'   <- mapM (convertExp fs) args
        let tys  = fmap (snd . annotOfExp) args
        convertPrim p ann retty (args' `zip` tys)

 -- A real nested query should not appear here.
 -- However, if it has no contexts, it's really just a nested expression.
 | Nested _ (Query [] x') <- x
 = convertExp fs x'

 | otherwise
 = case x of
    -- Variable must be bound as a precomputation
    Var _ n
     -> return $ CE.XVar $ Name n
    Nested (ann,_) q
     -> convertError
      $ ConvertErrorExpNestedQueryNotAllowedHere ann q
    App (ann,_) _ _
     -> convertError
      $ ConvertErrorExpApplicationOfNonPrimitive ann x
    Prim (ann,retty) p
     -> convertPrim p ann retty []


