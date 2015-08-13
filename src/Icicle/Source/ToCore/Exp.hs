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
        => Exp (Annot a n) n
        -> ConvertM a n (C.Exp n)
convertExp x
 = case x of
    Var ann n
     -> do  fs <- convertFeatures
            case Map.lookup n fs of
             Just (_, x')
              -> (x' . CE.XVar) <$> convertInputName
             -- Variable must be bound as a precomputation
             Nothing
              -> CE.XVar <$> convertFreshenLookup (annAnnot ann) n


    Nested _ q
     -> convertExpQ q


    App ann _ _
     -- Primitive application: convert arguments, then convert primitive
     | Just (p, Annot { annResult = resT }, args) <- takePrimApps x
     -> do  args'   <- mapM convertExp args
            let tys  = fmap (annResult . annotOfExp) args
            convertPrim p (annAnnot ann) resT (args' `zip` tys)

     | otherwise
     -> convertError
      $ ConvertErrorExpApplicationOfNonPrimitive (annAnnot ann) x


    Prim ann p
     -> convertPrim p (annAnnot ann) (annResult ann) []



convertExpQ
        :: Ord n
        => Query (Annot a n) n
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
      $ ConvertErrorExpNestedQueryNotAllowedHere (annAnnot $ annotOfQuery q) q

