-- | Convert Element Expressions to Core
--
-- Worker functions, eg the "value * 2" in "sum (value * 2)".
-- Aggregates are dealt with elsewhere.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Exp (
    convertExp
  , convertExpQ
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
        => Exp (a,UniverseType n) n
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


    Nested _ q
     -> convertExpQ q


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



convertExpQ
        :: Ord n
        => Query (a,UniverseType n) n
        -> ConvertM a n (C.Exp n)
convertExpQ q
 = case contexts q of
    []
     -> convertExp $ final q
    (Let _ b d:cs)
     -> do  d' <- convertExp d
            -- NB: because it's non-recursive let, the freshen must be done after the definition
            b' <- convertFreshenAdd b
            x' <- convertExpQ $ Query cs $ final q
            return $ CE.XLet b' d' x'
    _
     -> convertError
      $ ConvertErrorExpNestedQueryNotAllowedHere (fst $ annotOfQuery q) q

