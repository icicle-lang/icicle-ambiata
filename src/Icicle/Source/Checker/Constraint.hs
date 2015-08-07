-- | Generate type constraints
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Constraint (
    generateQ
  ) where

import                  Icicle.Source.Checker.Error

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import                  Icicle.Common.Base
import qualified        Icicle.Common.Fresh     as Fresh

import                  P

import                  Control.Monad.Trans.Class
import                  Control.Monad.Trans.Either
import qualified        Control.Monad.Trans.RWS as RWS

import qualified        Data.Map                as Map


type Env n = Map.Map n (FunctionType n)

type Gen a n t = EitherT
                    (CheckError a n)
                    (Fresh.FreshT n
                    (RWS.RWS () [(a, Constraint n)] (Env n)))
                    t

type Gen' a n q = q a n -> Gen a n (q (a, Type n) n)


require :: a -> Constraint n -> Gen a n ()
require a c
 = lift $ lift $ RWS.tell [(a,c)]

fresh :: Gen a n (Name n)
fresh
 = lift $ Fresh.fresh


freshenFunction :: a -> FunctionType n -> Gen a n ([Type n], Type n, FunctionType n)
freshenFunction ann f
 = do   freshen <- Map.fromList <$> mapM mkSubst (functionForalls f)

        mapM_ (require ann . substC freshen) (functionConstraints f)

        let sub = substT freshen
        return ( fmap sub $ functionArguments f
               ,      sub $ functionReturn    f
               ,                              f)

 where
  mkSubst n
   = ((,) n . TypeVar) <$> fresh


lookup :: a -> Name n -> Gen a n ([Type n], Type n, FunctionType n)
lookup ann n
 = do   env <- RWS.get
        case Map.lookup n env of
         Just t
          -> freshenFunction ann t
         Nothing
          -> left
           $ errorSuggestions (ErrorNoSuchVariable ann n)
                              [AvailableBindings $ Map.toList env]

bind :: Name n -> Type n -> Gen a n ()
bind n t
 = do   env <- RWS.get
        put $ Map.insert n $ function0 t


generateQ :: Gen' a n Query
generateQ (Query [] x)
 = Query [] <$> generateX x

generateQ (Query (c:cs) x)
 = do q' <- generateQ (Query cs x)
      let t = snd $ annotOfExp q'
      c' <- generateC t c
      return $ Query (c' : contexts q') (final q')


generateC :: Type n -> Gen' a n Context
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
            let t'  = canonT $ Temporality TemporalityAggregate $ Group tkey t
            return $ GroupBy (a,t') x'

    Distinct _ x
     -> do  x' <- generateX x
            requireAgg
            return $ Distinct a' x'

    Filter _ x
     -> do  x' <- generateX x
            requireAgg
            require a $ CEquals (snd $ annotOfExp x') BoolT
            return $ Filter a' x'

    LetFold _ f
     -> do  i <- generateX $ foldInit f
            bind (foldBind f) (snd $ annotOfExp i)
            w <- generateX $ foldWork f
            n <- fresh
            case foldType f of
             FoldTypeFoldl1
              -> requireTemporality (snd $ annotOfExp i) TemporalityElement
             FoldTypeFoldl
              -> requireTemporality (snd $ annotOfExp i) TemporalityPure

            requireTemporality (snd $ annotOfExp w) TemporalityElement

            -- XXX: insert these before checking the rest
            bind (foldBind f) (canonT $ Temporality TemporalityAggregate $ snd $ annotOfExp w)
            return $ LetFold a' (f { foldInit = i, foldWork = w })

    Let _ n x
     -> do  x' <- generateX x
            bind n (snd $ annotOfExp x')
            return $ Let a' n x'

 where
  a  = annotOfContext c
  a' = (a, t)

  requireTemporality ty tmp
   = do n <- fresh
        require a $ CEquals ty (Temporality tmp $ TypeVar n)
  requireAgg
   = requireTemporality t TemporalityAggregate

generateX :: Gen' a n Exp
generateX x
 = case x of
    Var a n
     -> do (args, res, fErr) <- lookup a n
           when (args /= [])
             $ left $ errorNoSuggestions (ErrorUnappliedFunction a x f)
           return (Var (a,res) n)

    Nested a q
     -> do q' <- generateQ q
           return $ Nested (a, snd $ annotOfExp q') q

    App a _ _
     | Just (p, args) <- takePrimApps x
     -> do stuff
     | (Var a' n, args) <- takeApps x
     -> do (args,res, fErr) <- lookup a' n
           bob dobbs
           dob bodds

    Prim a p
     -> do  unapplied prim

