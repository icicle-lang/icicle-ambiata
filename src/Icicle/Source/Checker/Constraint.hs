-- | Generate type constraints
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Constraint (
    constraintsQ
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

import                  Data.List (zip,unzip)
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
  defaultOfConstraint (CReturnOfLetTemporalities _ _ _)
   = []
  defaultOfConstraint (CReturnOfLatest _ _ _)
   = []



constraintsQ
        :: Ord n
        => Map.Map n (FunctionType n)
        -> Query a n
        -> EitherT (CheckError a n) (Fresh.Fresh n) (Query'C a n)
constraintsQ env q
 = evalGen (fst <$> generateQ q) (CheckState env [])


generateQ :: Ord n => Query a n -> Gen a n (Query'C a n, SubstT n)
generateQ (Query [] x)
 = do   (x',s) <- generateX x
        return (Query [] x', s)

-- | Note:
-- something is very wrong with using State for constraints.
-- The problem is that you can build up constraints at a higher level, then
-- they will be discharged and thrown away deeper in the recursion.
-- However, the substitution from discharging them does not apply on the higher level!
--
-- So for now any constraints must only be added *after* all recursions are done.
generateQ qq@(Query (c:_) _)
 = discharge (annAnnot.annotOfQuery) substTQ
 $ case c of
    Windowed _ from to
     -> do  (q',sq,t') <- rest
            requireAgg t'
            let t'' = canonT $ Temporality TemporalityAggregate t'
            with q' sq t'' $ \a' -> Windowed a' from to
    Latest _ i
     -> do  (q',sq,tq) <- rest
            retDat <- TypeVar <$> fresh
            let (tmpq,_,datq) = decomposeT tq
            require a $ CReturnOfLatest retDat (fromMaybe TemporalityPure tmpq) datq
            let t' = canonT $ Temporality TemporalityAggregate retDat
            with q' sq t' $ \a' -> Latest a' i
    GroupBy _ x
     -> do  (x',sx) <- generateX x
            (q',sq,tval) <- rest
            requireTemporality (annResult $ annotOfExp x') TemporalityElement
            requireAgg tval
            let tkey = annResult $ annotOfExp x'
            let t'  = canonT $ Temporality TemporalityAggregate $ GroupT tkey tval
            with q' (compose sx sq) t' $ \a' -> GroupBy a' x'

    Distinct _ x
     -> do  (x',sx) <- generateX x
            (q',sq,t') <- rest
            requireTemporality (annResult $ annotOfExp x') TemporalityElement
            requireAgg t'
            let t'' = canonT $ Temporality TemporalityAggregate t'
            with q' (compose sx sq) t'' $ \a' -> Distinct a' x'

    Filter _ x
     -> do  (x',sx) <- generateX x
            (q',sq,t') <- rest
            -- TODO allow possibly
            requireTemporality (annResult $ annotOfExp x') TemporalityElement
            requireData        (annResult $ annotOfExp x') BoolT

            requireAgg t'
            let t'' = canonT $ Temporality TemporalityAggregate t'
            with q' (compose sx sq) t'' $ \a' -> Filter a' x'

    LetFold _ f
     -> do  (i,si) <- generateX $ foldInit f
            (w,sw) <- withBind (foldBind f) (annResult $ annotOfExp i)
                    $ generateX $ foldWork f

            (q',sq,t') <- withBind (foldBind f) (canonT $ Temporality TemporalityAggregate $ annResult $ annotOfExp w) rest

            case foldType f of
             FoldTypeFoldl1
              -> requireTemporality (annResult $ annotOfExp i) TemporalityElement
             FoldTypeFoldl
              -> requireTemporality (annResult $ annotOfExp i) TemporalityPure

            requireAgg t'
            -- TODO: this should allow possibly too
            requireTemporality (annResult $ annotOfExp w) TemporalityElement

            let (_,_,it) = decomposeT $ annResult $ annotOfExp i
            let (_,_,wt) = decomposeT $ annResult $ annotOfExp w

            require a $ CEquals it wt
            let t'' = canonT $ Temporality TemporalityAggregate t'
            let s'  = si `compose` sw `compose` sq
            with q' s' t'' $ \a' -> LetFold a' (f { foldInit = i, foldWork = w })

    Let _ n x
     -> do  (x',sx) <- generateX x
            (q',sq,tq) <- withBind n (annResult $ annotOfExp x') rest

            retTmp <- TypeVar <$> fresh
            let tmpx = getTemporalityOrPure $ annResult $ annotOfExp x'
            let tmpq = getTemporalityOrPure $ tq
            require a $ CReturnOfLetTemporalities retTmp tmpx tmpq
            let t' = canonT $ Temporality retTmp tq

            with q' (compose sx sq) t' $ \a' -> Let a' n x'

 where
  a  = annotOfContext c

  rest
   = do (q',s') <- generateQ (qq { contexts = drop 1 $ contexts qq })
        return (q', s', annResult $ annotOfQuery q')

  with q' s' t' c'
   = do cs <- stateConstraints <$> (lift $ lift State.get)
        let a' = Annot a t' cs
        return (q' { contexts = c' a' : contexts q' }, s')

  requireTemporality ty tmp
   | TemporalityPure <- getTemporalityOrPure ty
   = return ()
   | otherwise
   = do n <- fresh
        require a $ CEquals ty (Temporality tmp $ TypeVar n)
  requireAgg t
   = requireTemporality t TemporalityAggregate

  requireData t1 t2
   = let (_,_,d1) = decomposeT t1
         (_,_,d2) = decomposeT t2
     in  require a $ CEquals d1 d2


generateX :: Ord n => Exp a n -> Gen a n (Exp'C a n, SubstT n)
generateX x
 = discharge (annAnnot.annotOfExp) substTX
 $ case x of
    Var a n
     -> do (argsT, resT, fErr) <- lookup a n
           when (not $ null argsT)
             $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])
           annotate Map.empty resT $ \a' -> Var a' n

    Nested _ q
     -> do (q',sq) <- generateQ q
           annotate sq (annResult $ annotOfQuery q') $ \a' -> Nested a' q'

    App a _ _
     -> let (f, args)   = takeApps x
            look        | Prim _ p <- f
                        = primLookup a p
                        | Var _ n  <- f
                        = lookup a n
                        | otherwise
                        = hoistEither $ errorNoSuggestions (ErrorApplicationNotFunction a x)
        in do   (argsT, resT, fErr) <- look

                (args',subs') <- unzip <$> mapM generateX args
                let argsT' = fmap (annResult.annotOfExp) args'

                when (length argsT /= length args)
                 $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr argsT')

                resT'   <- foldM (appType a) resT (argsT `zip` argsT')

                let s' = foldl compose Map.empty subs'
                (f',_) <- annotate s' resT' $ \a' -> reannotX (const a') f
                return (foldl mkApp f' args', s')

    Prim a p
     -> do (argsT, resT, fErr) <- primLookup a p
           when (not $ null argsT)
             $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])
           annotate Map.empty resT $ \a' -> Prim a' p

 where
  annotate s' t' f
   = do cs <- stateConstraints <$> (lift $ lift State.get)
        let a' = Annot (annotOfExp x) t' cs
        return (f a', s')



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


