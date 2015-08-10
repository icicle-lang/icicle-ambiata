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

import                  Data.List (zip)
import qualified        Data.Map                as Map


type Env n = Map.Map n (FunctionType n)

type Gen a n t = EitherT
                    (CheckError a n)
                    (Fresh.FreshT n
                    (RWS.RWS () [(a, Constraint n)] (Env n)))
                    t

require :: a -> Constraint n -> Gen a n ()
require a c
 = lift $ lift $ RWS.tell [(a,c)]

fresh :: Gen a n (Name n)
fresh
 = lift $ Fresh.fresh


freshenFunction :: Ord n => a -> FunctionType n -> Gen a n ([Type n], Type n, FunctionType n)
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


lookup :: Ord n => a -> n -> Gen a n ([Type n], Type n, FunctionType n)
lookup ann n
 = do   env <- lift $ lift $ RWS.get
        case Map.lookup n env of
         Just t
          -> freshenFunction ann t
         Nothing
          -> hoistEither
           $ errorSuggestions (ErrorNoSuchVariable ann n)
                              [AvailableBindings $ Map.toList env]

bind :: Ord n => n -> Type n -> Gen a n ()
bind n t
 = do   env <- lift $ lift $ RWS.get
        lift $ lift $ RWS.put $ Map.insert n (function0 t) env


generateQ :: Ord n => Query a n -> Gen a n (Query (a, Type n) n)
generateQ (Query [] x)
 = Query [] <$> generateX x

generateQ (Query (c:cs) x)
 = do q' <- generateQ (Query cs x)
      let t = snd $ annotOfQuery q'
      c' <- generateC t c
      return $ Query (c' : contexts q') (final q')


generateC :: Ord n => Type n -> Context a n -> Gen a n (Context (a, Type n) n)
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
            let t'  = canonT $ Temporality TemporalityAggregate $ GroupT tkey t
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


generateX :: Ord n => Exp a n -> Gen a n (Exp (a, Type n) n)
generateX x
 = case x of
    Var a n
     -> do (argsT, resT, fErr) <- lookup a n
           when (not $ null argsT)
             $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])
           return (Var (a,resT) n)

    Nested a q
     -> do q' <- generateQ q
           return $ Nested (a, snd $ annotOfQuery q') q'

    App a _ _
     -> let (f, args)   = takeApps x
            look        | Prim _ p <- f
                        = primLookup a p
                        | Var _ n  <- f
                        = lookup a n
                        | otherwise
                        = hoistEither $ errorNoSuggestions (ErrorApplicationNotFunction a x)
        in do   (argsT, resT, fErr) <- look

                args'   <- mapM generateX args
                let argsT' = fmap (snd.annotOfExp) args'

                when (length argsT /= length args)
                 $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr argsT')

                resT'   <- foldM (appType a) resT (argsT `zip` argsT')

                let f' = reannotX (\anno -> (anno,resT')) f
                return $ foldl mkApp f' args'

    Prim a p
     -> do (argsT, resT, fErr) <- primLookup a p
           when (not $ null argsT)
             $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])
           return (Prim (a,resT) p)


appType :: a -> Type n -> (Type n, Type n) -> Gen a n (Type n)
appType ann resT (expT,actT)
 = do let (tmpE,posE,datE) = decomposeT expT
      let (tmpA,posA,datA) = decomposeT actT
      let (tmpR,posR,datR) = decomposeT resT

      require ann (CEquals datE datA)

      tmpR' <- checkTemp (purely tmpE) (purely tmpA) (purely tmpR)
      posR' <- checkPoss (definitely posE) (definitely posA) (definitely posR)

      return $ recomposeT (tmpR', posR', datR)
 where
  checkTemp = check' TemporalityPure
  checkPoss = check' PossibilityDefinitely

  check' pureMode modE modA modR
   | Nothing <- modA
   = return modR
   | Just _  <- modA
   , Nothing <- modE
   , Nothing <- modR
   = return modA
   | Just a' <- modA
   , Nothing <- modE
   , Just r' <- modR
   = do require ann $ CEquals a' r'
        return modR
   | otherwise
   = do require ann $ CEquals (maybe pureMode id modE) (maybe pureMode id modA)
        return modR


  purely (Just TemporalityPure) = Nothing
  purely tmp = tmp

  definitely (Just PossibilityDefinitely) = Nothing
  definitely pos = pos


primLookup :: Ord n => a -> Prim -> Gen a n ([Type n], Type n, FunctionType n)
primLookup ann p
 = do ft <- primLookup' p
      freshenFunction ann ft

primLookup' :: Prim -> Gen a n (FunctionType n)
primLookup' p
 = case p of
    Op (ArithUnary _)
     -> fNum $ \at -> ([at], at)
    Op (ArithBinary _)
     -> fNum $ \at -> ([at, at], at)
    Op (ArithDouble Div)
     -> f0 [DoubleT, DoubleT] DoubleT

    Op (Relation _)
     -> f1 $ \a at -> FunctionType [a] [] [at, at] BoolT

    Op  TupleComma
     -> do a <- fresh
           b <- fresh
           let at = TypeVar a
           let bt = TypeVar b
           return $ FunctionType [a,b] [] [at, bt] (PairT at bt)

    Lit (LitInt _)
     -> fNum $ \at -> ([], at)
    Lit (LitDouble _)
     -> f0 [] DoubleT
    Lit (LitString _)
     -> f0 [] StringT

    Fun Log
     -> f0 [DoubleT] DoubleT
    Fun Exp
     -> f0 [DoubleT] DoubleT
    Fun ToDouble
     -> fNum $ \at -> ([at], DoubleT)
    Fun ToInt
     -> fNum $ \at -> ([at], IntT)

 where

  f0 argsT resT
   = return $ FunctionType [] [] argsT resT

  fNum f
   = f1 (\a at -> uncurry (FunctionType [a] [CIsNum at]) (f at))

  f1 f
   = do n <- fresh
        return $ f n (TypeVar n)
