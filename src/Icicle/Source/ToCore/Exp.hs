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
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Common.Exp           as CE

import                  P

import                  Data.List (zip)

import qualified        Data.Map as Map



-- | Convert an element-level expression.
-- These are worker functions for folds, filters and so on.
convertExp
        :: Ord n
        => Exp (a,UniverseType) n
        -> ConvertM a n (C.Exp n)
convertExp x
 = case x of
    Var (ann,_) n
     -> do  fs <- convertFeatures
            case Map.lookup n fs of
             Just (_, x')
              -> (x' . CE.XVar) <$> convertInputName
             -- Variable must be bound as a precomputation
             Nothing
              -> CE.XVar <$> convertFreshenLookup ann n


    Nested (ann,_) q
     -- A real nested query should not appear here.
     -- However, if it has no contexts, it's really just a nested expression.
     | (Query [] x') <- q
     -> convertExp x'

     | otherwise
     -> convertError
      $ ConvertErrorExpNestedQueryNotAllowedHere ann q


    App (ann,_) _ _
     -- Primitive application: convert arguments, then convert primitive
     | Just (p, _, args) <- takePrimApps x
     -> do  args'   <- mapM convertExp args
            let tys  = fmap (snd . annotOfExp) args
            convertPrim p ann (args' `zip` tys)

     | otherwise
     -> convertError
      $ ConvertErrorExpApplicationOfNonPrimitive ann x


    Prim (ann,_) p
     -> convertPrim p ann []


