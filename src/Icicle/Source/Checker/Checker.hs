{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Checker (
    checkQT
  , checkQ
  , checkX
  ) where

import                  Icicle.Source.Checker.Error
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.Query
import                  Icicle.Source.Type

import qualified        Icicle.Common.Type as T

import                  P

import qualified        Data.Map as Map


type Env        n = Map.Map n UniverseType
type Result r a n = Either (CheckError a n) (r, UniverseType)


checkQT :: Ord n
        => Features n
        -> QueryTop a n
        -> Result (QueryTop (a,UniverseType) n) a n
checkQT features qt
 = case Map.lookup (feature qt) features of
    Just (_,f)
     -> do  (q,t) <- checkQ (envOfFeatureContext f) (query qt)
            return (qt { query = q }, t)
    Nothing
     -> errorSuggestions (ErrorNoSuchFeature (feature qt))
                         [suggestionForFeatures]

 where
  suggestionForFeatures
   = AvailableFeatures
   $ fmap (\(k,(t,_)) -> (k, t))
   $ Map.toList features


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
                    -- The group contents must be an aggregate or pure.
                    -- No nested groups
                    let groupError x
                         = errorSuggestions (ErrorReturnNotAggregate ann q t')
                                            [Suggest "Group must return an aggregate", Suggest x]

                    case universeTemporality $ universe t' of
                     AggU -> return ()
                     Pure -> return ()
                     Elem    -> groupError "Elements are not allowed as this could create very structures"
                     Group _ -> groupError "Nested groups are not supported"

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
                    expFilterIsBool ann c te
                    expIsElem ann c te
                    (q'', t') <- tq
                    let t'' = t' { universe = castPossibilityWith (universe t') (universe te) }
                    let c' = Filter (ann, t'') e'
                    return (wrap c' q'', t'')

             LetFold ann f
              -> do -- Any mention of the binding in the zero case is an error.
                    -- We need to explicitly remove it in case there was something
                    -- already defined with the same name
                    let ctxRemove = Map.delete (foldBind f) ctx

                    (init',ti) <- checkX ctxRemove  $ foldInit f

                    let foldError x
                         | FoldTypeFoldl  <- foldType f
                         = errorSuggestions (ErrorUniverseMismatch ann ti $ (universe ti) { universeTemporality = Pure })
                                            [Suggest "Fold initialisers must be pure", Suggest x]
                         | otherwise
                         = errorSuggestions (ErrorUniverseMismatch ann ti $ (universe ti) { universeTemporality = Elem })
                                            [Suggest "Fold initialisers must be pure or element", Suggest x]

                    case (foldType f, universeTemporality $ universe ti) of
                     (_, Pure)              -> return ()
                     (FoldTypeFoldl1, Elem) -> return ()
                     (FoldTypeFoldl, Elem)  -> foldError "You cannot refer to an element; perhaps you meant to use fold1"
                     (_, _)                 -> foldError "The initialiser cannot refer to an aggregate or group, as this would require multiple passes"

                    let ctx' = Map.insert (foldBind f) (UniverseType (Universe Pure Definitely) $ baseType ti) ctx
                    (work',tw) <- checkX ctx' $ foldWork f

                    when (baseType ti /= baseType tw)
                      $ errorNoSuggestions $ ErrorFoldTypeMismatch ann ti tw

                    expIsElem ann c tw

                    let possibility
                              | FoldTypeFoldl1 <- foldType f
                              = Possibly
                              | Possibly <- universePossibility $ universe ti
                              = Possibly
                              | Possibly <- universePossibility $ universe tw
                              = Possibly
                              | otherwise
                              = Definitely

                    let ctx'' = Map.insert (foldBind f)
                             (UniverseType (Universe AggU possibility) $ baseType ti) ctx
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
  expFilterIsBool ann c te
   | T.BoolT <- baseType te
   = return ()
   | otherwise
   = errorSuggestions (ErrorContextExpNotBool ann c te)
                      [Suggest "The predicate for a filter must be a boolean"]

  expIsEnum ann c te
   = when (not $ isEnum $ baseType te)
         $ errorSuggestions (ErrorContextExpNotEnum ann c te)
                            [Suggest "Group-by and distinct-by must be bounded; otherwise we'd run out of memory"]

  expIsElem ann c te
   = when (not $ isPureOrElem $ universe te)
         $ errorSuggestions (ErrorContextExpNotElem ann c te)
                            [Suggest "This expression cannot refer to aggregates or groups as it would require multiple passes"]

  requireAggOrGroup ann t
   = when (isPureOrElem $ universe t)
         $ errorSuggestions (ErrorReturnNotAggregate ann q t)
                            [Suggest "The return must be an aggregate, otherwise the result could be quite large"]

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
     -> maybe (errorSuggestions (ErrorNoSuchVariable ann n)
                                [AvailableBindings $ Map.toList ctx])
              (\t -> return (Var (ann,t) n, t))
              (Map.lookup n ctx)
    Nested ann q
     -> do (q',t') <- checkQ ctx q
           return (Nested (ann,t') q', t')

    Prim ann p
     -> do (_,t') <- checkP x p []
           return (Prim (ann,t') p, t')

    -- We can give slightly better error messages if we descend first
    App a p q
     -> do  _ <- checkX ctx p
            _ <- checkX ctx q
            errorNoSuggestions $ ErrorApplicationOfNonPrim a x


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
     -> return ((), UniverseType (Universe AggU Possibly) (baseType t))
     | Oldest <- a
     , [t] <- args
     , isPureOrElem $ universe t
     -> return ((), UniverseType (Universe AggU Possibly) (baseType t))

     | otherwise
     -> err

    Lit (LitInt _)
     | [] <- args
     -> return ((), UniverseType (Universe Pure Definitely) T.IntT)
     | otherwise
     -> err
 where
  err = errorNoSuggestions $ ErrorPrimBadArgs (annotOfExp x) x args

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
