{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Checker (
    checkQT
  , checkQ
  , checkX
  ) where

import                  Icicle.Source.Checker.Error
import                  Icicle.Source.Query
import                  Icicle.Source.Type

import qualified        Icicle.Common.Type as T

import                  P

import qualified        Data.Map as Map


type FeatureMap n = Map.Map n (Map.Map n BaseType)
type Env        n = Map.Map n UniverseType
type Result r a n = Either (CheckError a n) (r, UniverseType)


checkQT :: Ord n
        => FeatureMap n
        -> QueryTop a n
        -> Result (QueryTop (a,UniverseType) n) a n
checkQT features qt
 = case Map.lookup (feature qt) features of
    Just f
     -> do  (q,t) <- checkQ (Map.map (UniverseType $ Universe Elem Definitely) f) (query qt)
            return (qt { query = q }, t)
    Nothing
     -> Left $ ErrorNoSuchFeature (feature qt)


checkQ  :: Ord      n
        => Env      n
        -> Query  a n
        -> Result (Query (a,UniverseType) n) a n
checkQ ctx q
 = case contexts q of
    []
     -> do  (x,t) <- checkX ctx (final q)
            requireAggOrGroup (annotOfExp $ final q) t
            return (Query [] x, t)
    (c:cs)
     -> let q' = q { contexts = cs }
            tq = checkQ ctx q'

        in  case c of
             Windowed ann lo hi
              -- TODO: check that range is valid
              -> do (q'',t) <- tq
                    let c' = Windowed (ann, t) lo hi
                    return (wrap c' q'', t)

             Latest ann num
              -> do (q'',t) <- tq
                    let tA = wrapAsAgg t
                    let c' = Latest (ann, tA) num
                    return (wrap c' q'', tA)

             GroupBy ann e
              -> do (e',te) <- checkX ctx e
                    -- Check that the thing we're grouping by is enum-ish
                    expIsEnum ann c te
                    -- And contains no aggregates
                    expIsElem ann c te

                    (q'',t') <- tq
                    -- The group contents must be an aggregate.
                    -- No nested groups
                    when (not $ isAgg $ universe t')
                     $ Left $ ErrorReturnNotAggregate ann q t'

                    let t'' = t' { universe = (Universe (Group $ baseType te) Definitely) }
                    let c' = GroupBy (ann, t'') e'
                    return (wrap c' q'', t'')

             Distinct ann e
              -> do (e',te) <- checkX ctx e
                    expIsEnum ann c te
                    expIsElem ann c te
                    (q'', t') <- tq
                    let c' = Distinct (ann, t') e'
                    return (wrap c' q'', t')

             Filter   ann e
              -> do (e', te) <- checkX ctx e
                    expIsBool ann c te
                    expIsElem ann c te
                    (q'', t') <- tq
                    let t'' = t' { universe = castPossibilityWith (universe t') (universe te) }
                    let c' = Filter (ann, t'') e'
                    return (wrap c' q'', t'')

             LetFold ann f
              -> do (init',ti) <- checkX ctx  $ foldInit f
                    expIsElem ann c ti
                    let ctx' = Map.insert (foldBind f) ti ctx
                    (work',tw) <- checkX ctx' $ foldWork f

                    when (ti /= tw)
                      $ Left $ ErrorFoldTypeMismatch ann ti tw

                    expIsElem ann c tw

                    let ctx'' = Map.insert (foldBind f)
                             (UniverseType (Universe AggU Possibly) $ baseType ti) ctx
                    (q'',t') <- checkQ ctx'' q'

                    let c'  = LetFold (ann,t') (f { foldInit = init', foldWork = work' })
                    return (wrap c' q'', t')


             Let ann n e
              -> do (e',te) <- checkX ctx e
                    let ctx' = Map.insert n te ctx
                    (q'',t') <- checkQ ctx' q'
                    let c'   = Let (ann,t') n e'
                    return (wrap c' q'', t')

 where
  expIsBool ann c te
   | T.BoolT <- baseType te
   = return ()
   | otherwise
   = Left $ ErrorContextExpNotBool ann c te

  expIsEnum ann c te
   = when (not $ isEnum $ baseType te)
         $ Left $ ErrorContextExpNotEnum ann c te

  expIsElem ann c te
   = when (not $ isPureOrElem $ universe te)
         $ Left $ ErrorContextExpNotElem ann c te

  requireAggOrGroup ann t
   = when (isPureOrElem $ universe t)
         $ Left $ ErrorReturnNotAggregate ann q t

  wrapAsAgg t
   | isPureOrElem $ universe t
   = UniverseType (universe t) { universeTemporality = AggU }
   $ T.ArrayT $ baseType t
   | otherwise
   = t

  wrap cc qq
   = qq { contexts = cc : contexts qq }


checkX  :: Ord      n
        => Env      n
        -> Exp    a n
        -> Result (Exp (a,UniverseType) n) a n
checkX ctx x
 | Just (prim, ann, args) <- takePrimApps x
 = do xts <- mapM (checkX ctx) args
      let xs = fmap fst xts
      let ts = fmap snd xts
      (_,t') <- checkP x prim ts
      -- Here we are annotating the primitive with its result type
      -- instead of the actual function type.
      let x' = foldl mkApp (Prim (ann,t') prim) xs
      return (x', t')

 | otherwise
 = case x of
    Var ann n
     -> maybe (Left $ ErrorNoSuchVariable ann n) (\t -> return (Var (ann,t) n, t))
              (Map.lookup n ctx)
    Nested ann q
     -> do (q',t') <- checkQ ctx q
           return (Nested (ann,t') q', t')

    Prim ann p
     -> do (_,t') <- checkP x p []
           return (Prim (ann,t') p, t')

    App a _ _
     -> Left $ ErrorApplicationOfNonPrim a x


checkP  :: Ord      n
        => Exp    a n
        -> Prim
        -> [UniverseType]
        -> Result () a n
checkP x p args
 = case p of
    Op o
     | Negate <- o
     -> unary
     | Div <- o
     -> binary Possibly o
     | otherwise
     -> binary Definitely o

    Agg a
     | Count <- a
     , [] <- args
     -> return ((), UniverseType (Universe AggU Definitely) T.IntT)
     | SumA <- a
     , [t] <- args
     , isPureOrElem $ universe t
     , baseType t == T.IntT
     -> return ((), UniverseType (aggu $ universe t) T.IntT)
     | Newest <- a
     , [t] <- args
     , isPureOrElem $ universe t
     -> return ((), UniverseType (possibly $ universe t) (baseType t))
     | Oldest <- a
     , [t] <- args
     , isPureOrElem $ universe t
     -> return ((), UniverseType (possibly $ universe t) (baseType t))

     | otherwise
     -> err

    Lit (LitInt _)
     | [] <- args
     -> return ((), UniverseType (Universe Pure Definitely) T.IntT)
     | otherwise
     -> err
 where
  err = Left $ ErrorPrimBadArgs (annotOfExp x) x args

  aggu u = u { universeTemporality = AggU }

  unary
   | [t] <- args
   , baseType t == T.IntT
   , not $ isGroup $ universe t
   = return ((), t)
   | otherwise
   = err

  binary poss o
   | [a, b] <- args
   , baseType a == T.IntT
   , baseType b == T.IntT
   , Just u <- maxOf (universe a) (universe b)
   , poss'  <- maxOfPossibility (universePossibility u) poss
   , not $ isGroup u
   = return ((), UniverseType (u { universePossibility = poss'}) $ returnType o)
   | otherwise
   = err

  returnType o
   = case o of
     Gt -> T.BoolT
     Ge -> T.BoolT
     Lt -> T.BoolT
     Le -> T.BoolT
     Eq -> T.BoolT
     Ne -> T.BoolT
     _  -> T.IntT
