-- | Generate type constraints
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Constraint (
    checkQ
  , generateQ
  , generateX
  , defaults
  ) where

import                  Icicle.Source.Checker.Base
import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Checker.Prim

import                  Icicle.Source.Query
import                  Icicle.Source.Type

import qualified        Icicle.Common.Fresh     as Fresh

import                  P

import                  Control.Monad.Trans.Class
import                  Control.Monad.Trans.Either
import qualified        Control.Monad.Trans.State as State

import                  Data.List (zip)
import qualified        Data.Map                as Map



defaults :: Ord n
         => Query'C a n
         -> Query'C a n
defaults q
 = let cm = Map.fromList
          $ concatMap (defaultOfConstraint . snd)
          $ annConstraints
          $ annotOfQuery q
   in substTQ cm q
 where
  defaultOfConstraint (CIsNum t)
   | TypeVar n <- t
   = [(n, IntT)]
   | otherwise
   = []
  defaultOfConstraint (CEquals _ _)
   = []



checkQ  :: Ord n
        => Map.Map n (FunctionType n)
        -> Query a n
        -> EitherT (CheckError a n) (Fresh.Fresh n) (Query'C a n)
checkQ env q
 = evalGen (generateQ q) (CheckState env [])


generateQ :: Ord n => Query a n -> Gen a n (Query'C a n)
generateQ (Query [] x)
 = Query [] <$> generateX x

generateQ qq@(Query (c:_) _)
 = discharge (annAnnot.annotOfQuery) substTQ
 $ case c of
    Windowed _ from to
     -> do  (q',t') <- rest
            requireAgg t'
            with q' t' $ \a' -> Windowed a' from to
    Latest _ i
     -> do  (q',t') <- rest
            -- requireAgg t'
            with q' t' $ \a' -> Latest a' i
    GroupBy _ x
     -> do  x' <- generateX x
            (q',tval) <- rest
            requireAgg tval
            let tkey = annResult $ annotOfExp x'
            let t'  = canonT $ Temporality TemporalityAggregate $ GroupT tkey tval
            with q' t' $ \a' -> GroupBy a' x'

    Distinct _ x
     -> do  x' <- generateX x
            (q',t') <- rest
            requireAgg t'
            with q' t' $ \a' -> Distinct a' x'

    Filter _ x
     -> do  x' <- generateX x
            (q',t') <- rest
            requireAgg t'
            -- TODO allow pure, allow possibly
            require a $ CEquals (annResult $ annotOfExp x') (Temporality TemporalityElement BoolT)
            with q' t' $ \a' -> Filter a' x'

    LetFold _ f
     -> do  i <- generateX $ foldInit f
            bind (foldBind f) (annResult $ annotOfExp i)
            w <- generateX $ foldWork f
            case foldType f of
             FoldTypeFoldl1
              -> requireTemporality (annResult $ annotOfExp i) TemporalityElement
             FoldTypeFoldl
              -> requireTemporality (annResult $ annotOfExp i) TemporalityPure

            -- TODO: this should allow possibly too
            requireTemporality (annResult $ annotOfExp w) TemporalityElement

            let (_,_,it) = decomposeT $ annResult $ annotOfExp i
            let (_,_,wt) = decomposeT $ annResult $ annotOfExp w
            require a $ CEquals it wt

            bind (foldBind f) (canonT $ Temporality TemporalityAggregate $ annResult $ annotOfExp w)
            (q',t') <- rest
            with q' t' $ \a' -> LetFold a' (f { foldInit = i, foldWork = w })

    Let _ n x
     -> do  x' <- generateX x
            bind n (annResult $ annotOfExp x')
            (q',t') <- rest
            with q' t' $ \a' -> Let a' n x'

 where
  a  = annotOfContext c

  rest
   = do q' <- generateQ (qq { contexts = drop 1 $ contexts qq })
        return (q', annResult $ annotOfQuery q')

  with q' t' c'
   = do cs <- constraints <$> (lift $ lift State.get)
        let a' = Annot a t' cs
        return q' { contexts = c' a' : contexts q' }

  requireTemporality ty tmp
   | TemporalityPure <- getTemporalityOrPure ty
   = return ()
   | otherwise
   = do n <- fresh
        require a $ CEquals ty (Temporality tmp $ TypeVar n)
  requireAgg t
   = requireTemporality t TemporalityAggregate



generateX :: Ord n => Exp a n -> Gen a n (Exp'C a n)
generateX x
 = discharge (annAnnot.annotOfExp) substTX
 $ case x of
    Var a n
     -> do (argsT, resT, fErr) <- lookup a n
           when (not $ null argsT)
             $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])
           annotate resT $ \a' -> Var a' n

    Nested _ q
     -> do q' <- generateQ q
           annotate (annResult $ annotOfQuery q') $ \a' -> Nested a' q'

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
                let argsT' = fmap (annResult.annotOfExp) args'

                when (length argsT /= length args)
                 $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr argsT')

                resT'   <- foldM (appType a) resT (argsT `zip` argsT')

                f' <- annotate resT' $ \a' -> reannotX (const a') f
                return $ foldl mkApp f' args'

    Prim a p
     -> do (argsT, resT, fErr) <- primLookup a p
           when (not $ null argsT)
             $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])
           annotate resT $ \a' -> Prim a' p

 where
  annotate t' f
   = do cs <- constraints <$> (lift $ lift State.get)
        let a' = Annot (annotOfExp x) t' cs
        return (f a')



appType :: a -> Type n -> (Type n, Type n) -> Gen a n (Type n)
appType ann resT (expT,actT)
 = do let (tmpE,posE,datE) = decomposeT $ canonT expT
      let (tmpA,posA,datA) = decomposeT $ canonT actT
      let (tmpR,posR,datR) = decomposeT $ canonT resT

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


