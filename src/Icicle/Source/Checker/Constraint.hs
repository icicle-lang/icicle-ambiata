-- | Generate type constraints
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Constraint (
    generateQ
  ) where

import Icicle.Source.Checker.Error

import Icicle.Source.Query
import Icicle.Source.Type

import Icicle.Common.Base
import Icicle.Common.Fresh

import P

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.RWS

import qualified        Data.Map as Map

type Env n = Map.Map n (FunctionType n)

type Gen a n t = EitherT (CheckError a n) (FreshT n (RWS (Env n) [(a, Constraint n)] ())) t
type Gen' a n q = q a n -> Gen a n (q (a, UniverseType n) n)

require :: a -> Constraint n -> Gen a n ()
require a c
 = lift $ lift $ tell [(a,c)]

freshVar :: Gen a n (Name n)
freshVar = lift $ fresh


generateQ :: Gen' a n Query
generateQ (Query [] x)
 = Query [] <$> generateX x

generateQ (Query (c:cs) x)
 = do q' <- generateQ (Query cs x)
      let t = snd $ annotOfExp q'
      c' <- generateC t c
      return $ Query (c' : contexts q') (final q')


generateC :: UniverseType n -> Gen a n Context
generateC t c
 = case c of
    Windowed _ from to
     -> do  requireAgg
            return $ Windowed a' from to
    Latest _ i
     -> do  requireAgg
            return $ Latest a' i
    GroupBy _ x
     -> do  x' <- generateX x
            requireAgg
            let tkey = snd $ annotOfExp x'
            let tval = baseType t
            let uni  = Universe (Group tkey) (universePossibility $ universe t)
            let t' = UniverseType uni tval
            return $ GroupBy (a,t') x'

    Distinct _ x
     -> do  x' <- generateX x
            requireAgg
            return $ Distinct a' x'

    Filter _ x
     -> do  x' <- generateX x
            requireAgg
            require a $ ConstraintBaseType (baseType $ snd $ annotOfExp x') ConstraintEq BoolT
            return $ Filter a' x'

    LetFold _ f
     -> do  i <- generateX $ foldInit f
            insertBind (foldBind f) (snd $ annotOfExp i)
            w <- generateX $ foldWork f
            case foldType f of
             FoldTypeFoldl1
              -> require a $ ConstraintTemporality (snd $ annotOfExp i) ConstraintEq Element
             FoldTypeFoldl
              -> require a $ ConstraintTemporality (snd $ annotOfExp i) ConstraintEq Pure

            require a $ ConstraintTemporality (snd $ annotOfExp w) ConstraintEq Element

            -- XXX: insert these before checking the rest
            insertBind (foldBind f) (UniverseType AggU .. $ snd $ annotOfExp w)
            return $ LetFold a' (f { foldInit = i, foldWork = w })

    Let _ n x
     -> do  x' <- generateX x
            insertBind n (snd $ annotOfExp x')
            return $ Let a' n x'

 where
  a  = annotOfContext c
  a' = (a, t)
  requireAgg
    = require a
    $ ConstraintTemporality (universeTemporality $ universe s) ConstraintEq AggU

generateX :: Gen a n Exp
generateX x
 = case x of
    Var a n
     -> do f <- lookup n
           when (functionArgs /= []) $
             error - unapplied function
    Nested a q
     -> do q' <- generateQ q
           return $ Nested (a, snd $ annotOfExp q') q

    App a p q
     | takePrimApps
     ...
     ->

    Prim a p
     -> do  unapplied prim

