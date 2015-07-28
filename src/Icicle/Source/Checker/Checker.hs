-- | Typecheck a Query, returning the annotated query and return type
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Checker.Checker (
    checkQT
  , checkQ
  , checkX
  , CheckEnv(..)
  , emptyEnv
  ) where

import                  Icicle.Source.Checker.Error
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.Query
import                  Icicle.Source.Type

import qualified        Icicle.Common.Type as T

import                  P

import qualified        Data.Map as Map

import                  Data.List (zip, repeat)


-- | Type checking environment.
-- Keep track of all the things we need to know
data CheckEnv n
 = CheckEnv
 -- | Mapping from variable names to whole types
 { env        :: Map.Map n UniverseType
 -- | The top-level of a query must return an Aggregate, so note whether we're currently at top
 , isTopLevel :: Bool
 -- | We can't have windows or other group-like things inside groups.
 -- This is actually treating all of latests, distincts and groups as "group-like"
 -- because they all require compilation to a single fold.
 , isInGroup  :: Bool
 -- | Don't allow any sort of contexts: lets, filters, windows, groups, etc
 -- These are not allowed inside "worker expressions", eg inside a filter's predicate
 -- or a group's by/key.
 , allowContexts :: Bool
 }

-- | Initial environment at top-level, not inside a group, and allowing contexts
emptyEnv :: CheckEnv n
emptyEnv
 = CheckEnv Map.empty True False True

type Result r a n = Either (CheckError a n) (r, UniverseType)


-- | Check a top-level Query, returning the query with type annotations and casts inserted.
checkQT :: Ord n
        => Features n
        -> QueryTop a n
        -> Result (QueryTop (a,UniverseType) n) a n
checkQT features qt
 = case Map.lookup (feature qt) features of
    Just (_,f)
     -> do  (q,t) <- checkQ (emptyEnv { env = envOfFeatureContext f }) (query qt)
            return (qt { query = q }, t)
    Nothing
     -> errorSuggestions (ErrorNoSuchFeature (feature qt))
                         [suggestionForFeatures]

 where
  suggestionForFeatures
   = AvailableFeatures
   $ fmap (\(k,(t,_)) -> (k, t))
   $ Map.toList features



-- | Check a Query with contexts
checkQ  :: Ord      n
        => CheckEnv n
        -> Query  a n
        -> Result (Query (a,UniverseType) n) a n
checkQ ctx_top q
 = do (x, t) <- go
      -- If it's top-level, make sure it returns aggregate or group
      when (isTopLevel ctx_top)
       $ requireAggOrGroup (annotOfQuery q) t
      return (x, t)
 where
  -- If we were at top-level, any lower levels are not top
  ctx = ctx_top { isTopLevel = False }
  go
   = case contexts q of
        -- No contexts, so it's just an expression
        []
         -> do  (x,t) <- checkX ctx (final q)
                return (Query [] x, t)

        -- Pull the first context off and look at it
        (c:cs)
         | allowContexts ctx == False
         -> errorSuggestions (ErrorContextNotAllowedHere (annotOfQuery q) c)
                             [Suggest "Contexts are not allowed inside worker functions of sums etc"]
         | otherwise
         -> let q' = q { contexts = cs }

            in  case c of
                 Windowed ann lo hi
                  -- TODO: check that range is valid
                  -> do (q'',t) <- checkQ ctx q'
                        notAllowedInGroupBy ann c
                        requireAggOrGroup ann t
                        let c' = Windowed (ann, t) lo hi
                        return (wrap c' q'', t)

                 Latest ann num
                  -> do (q'',t') <- checkQ (ctx { isInGroup = True }) q'
                        notAllowedInGroupBy ann c
                        -- If the rest is an element, wrap it in an array
                        -- eg (latest 3 ~> value) is an array of 3 ints
                        let tA = wrapAsAgg t'
                        let c' = Latest (ann, tA) num
                        return (wrap c' q'', tA)

                 GroupBy ann e
                  -> do (e',te) <- checkX (ctx { allowContexts = False}) e
                        notAllowedInGroupBy ann c
                        -- Check that the thing we're grouping by is enum-ish
                        expIsEnum ann c te
                        -- And contains no aggregates
                        expIsElem ann c te

                        (q'',t') <- checkQ (ctx { isInGroup = True }) q'
                        -- The group contents must be an aggregate or pure.
                        -- No nested groups
                        let groupError x
                             = errorSuggestions (ErrorReturnNotAggregate ann q t')
                                                [Suggest "Group must return an aggregate", Suggest x]

                        case universeTemporality $ universe t' of
                         AggU -> return ()
                         Pure -> return ()
                         Elem    -> groupError "Elements are not allowed as this could create very large structures"
                         Group _ -> groupError "Nested groups are not supported"

                        let poss = maxOfPossibility
                                    (universePossibility $ universe te)
                                    (universePossibility $ universe t')
                        let t'' = t' { universe = (Universe (Group $ baseType te) poss) }
                        let c' = GroupBy (ann, t'') e'
                        return (wrap c' q'', t'')

                 Distinct ann e
                  -> do (e',te) <- checkX (ctx { allowContexts = False}) e
                        notAllowedInGroupBy ann c
                        expIsEnum ann c te
                        expIsElem ann c te
                        (q'',t') <- checkQ (ctx { isInGroup = True }) q'
                        requireAggOrGroup ann t'
                        let c' = Distinct (ann, t') e'
                        return (wrap c' q'', t')

                 Filter   ann e
                  -> do (e', te) <- checkX (ctx { allowContexts = False}) e
                        expFilterIsBool ann c te
                        expIsElem ann c te
                        (q'', t') <- checkQ ctx q'
                        requireAggOrGroup ann t'
                        let t'' = t' { universe = castPossibilityWith (universe t') (universe te) }
                        let c' = Filter (ann, t'') e'
                        return (wrap c' q'', t'')

                 LetFold ann f
                  -> do -- Any mention of the binding in the zero case is an error.
                        -- We need to explicitly remove it in case there was something
                        -- already defined with the same name
                        let envRemove = Map.delete (foldBind f) $ env ctx

                        (init',ti) <- checkX (ctx { allowContexts = False, env = envRemove }) $ foldInit f

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

                        let env' ft = Map.insert (foldBind f)
                                 (UniverseType (Universe Pure Definitely) ft)
                                 $ env ctx
                        let checkW ft = checkX (ctx { env = env' ft, allowContexts = False }) $ foldWork f
                        (work',tw) <- checkW (baseType ti)

                        -- Check if we need to cast one of the sides from Int to Double
                        (init'', work'',tret)
                          <- case (baseType ti, baseType tw) of
                              (T.IntT, T.DoubleT)
                               -- If the zero is an Int and the worker is a Double,
                               -- we need to convert the zero to a Double.
                               -- However, the already computed work' will include a cast from
                               -- the zero to a Double, so we need to compute a new worker
                               -- without the cast.
                               -> do    (work'',_) <- checkW T.DoubleT
                                        return (mkCastDouble init', work'', T.DoubleT)

                              -- If the zero is a double and the worker returns an Int,
                              -- we need to cast the worker.
                              -- However, this actually seems unlikely.
                              (T.DoubleT, T.IntT)
                               -> return (init', mkCastDouble work', T.DoubleT)
                              (a, b)
                               | a == b
                               -> return (init', work', a)
                               | otherwise
                               -> errorNoSuggestions $ ErrorFoldTypeMismatch ann ti tw

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

                        let env'' = Map.insert (foldBind f)
                                  (UniverseType (Universe AggU possibility) $ tret)
                                  $ env ctx
                        (q'',t') <- checkQ (ctx { env = env'' }) q'
                        requireAggOrGroup ann t'

                        let c'  = LetFold (ann,t') (f { foldInit = init'', foldWork = work'' })
                        return (wrap c' q'', t')


                 Let ann n e
                  -> do (e',te) <- checkX ctx e
                        let ctx' = ctx { env = Map.insert n te $ env ctx }
                        (q'',t') <- checkQ ctx' q'

                        let c'   = Let (ann,t') n e'
                        when (not $ letAllowedUniverses (universeTemporality $ universe te) (universeTemporality $ universe t'))
                         $ errorSuggestions (ErrorLetTypeMismatch ann te (te {universe = universe t'}))
                                            [Suggest "The type for this let implies that the definition cannot be used!"]

                        return (wrap c' q'', t')

  expFilterIsBool ann c te
   | T.BoolT <- baseType te
   = return ()
   | otherwise
   = errorSuggestions (ErrorContextExpNotBool ann c te)
                      [Suggest "The predicate for a filter must be a boolean"]

  expIsEnum ann c te
  -- TODO: this check is disabled as strings should be allowed
   = when (False && (not $ isEnum $ baseType te))
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

  notAllowedInGroupBy ann c
   = when (isInGroup ctx)
         $ errorSuggestions (ErrorContextNotAllowedHere ann c)
                            [Suggest "Windows, latests, distincts and groups are not allowed inside groups"]

  wrapAsAgg t
   | isPureOrElem $ universe t
   = UniverseType (universe t) { universeTemporality = AggU }
   $ T.ArrayT $ baseType t
   | otherwise
   = t

  wrap cc qq
   = qq { contexts = cc : contexts qq }

  letAllowedUniverses l1 l2
   = case (l1,l2) of
      (Pure,    _)      -> True
      (Elem,    Pure)   -> False
      (Elem,    _)      -> True
      (AggU,    Pure)   -> False
      (AggU,    Elem)   -> False
      (AggU,    _)      -> True
      (Group _, Pure)   -> False
      (Group _, Elem)   -> False
      (Group _, _)      -> True


checkX  :: Ord      n
        => CheckEnv      n
        -> Exp    a n
        -> Result (Exp (a,UniverseType) n) a n
checkX ctx x
 | Just (prim, ann, args) <- takePrimApps x
 = do let ctx' | Agg _ <- prim
               = ctx { allowContexts = False }
               | otherwise
               = ctx
      xts <- mapM (checkX ctx') args
      let xs = fmap fst xts
      let ts = fmap snd xts
      (cs,t') <- checkP x prim ts
      -- Here we are annotating the primitive with its result type
      -- instead of the actual function type.
      --
      -- Perform any casts necessary
      let mkCastApp f (xx,False) = mkApp f xx
          mkCastApp f (xx,True)  = mkApp f (mkCastDouble xx)
      let x' = foldl mkCastApp (Prim (ann,t') prim) (xs `zip` (cs <> repeat False))
      return (x', t')

 | otherwise
 = case x of
    Var ann n
     -> maybe (errorSuggestions (ErrorNoSuchVariable ann n)
                                [AvailableBindings $ Map.toList $ env ctx])
              (\t -> return (Var (ann,t) n, t))
              (Map.lookup n $ env ctx)
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


-- | Check a primitive against the types of its arguments.
-- The expression is just for error reporting.
--
-- This returns the result type as well as a list of which arguments need casting to Double.
-- If the list of casts is empty, assume that no arguments require casts.
checkP  :: Ord      n
        => Exp    a n
        -> Prim
        -> [UniverseType]
        -> Result [Bool] a n
checkP x p args
 = case p of
    Op o
     -> typeOp o

    Agg a
     | Count <- a
     , [] <- args
     -> return ([], UniverseType (Universe AggU Definitely) T.IntT)
     | SumA <- a
     , [t] <- args
     , isPureOrElem $ universe t
     , baseType t == T.IntT || baseType t == T.DoubleT
     -> return ([], UniverseType (aggu $ universe t) (baseType t))
     | Newest <- a
     , [t] <- args
     , isPureOrElem $ universe t
     -> return ([], UniverseType (Universe AggU Possibly) (baseType t))
     | Oldest <- a
     , [t] <- args
     , isPureOrElem $ universe t
     -> return ([], UniverseType (Universe AggU Possibly) (baseType t))

     | otherwise
     -> err

    Lit (LitInt _)
     | [] <- args
     -> return ([], UniverseType (Universe Pure Definitely) T.IntT)
     | otherwise
     -> err

    Fun Log
     | [t] <- args
     -> do  (_,ds) <- castToDoublesForce [t]
            return (ds, t { baseType = T.DoubleT } )
     | otherwise -> err

    Fun Exp
     | [t] <- args
     -> do  (_,ds) <- castToDoublesForce [t]
            return (ds, t { baseType = T.DoubleT } )
     | otherwise -> err

    Fun ToDouble
     | [t] <- args
     , baseType t == T.IntT
     -> return ([], t { baseType = T.DoubleT } )
     | otherwise -> err

    Fun ToInt
     | [t] <- args
     , baseType t == T.DoubleT
     -> return ([], t { baseType = T.IntT } )
     | otherwise -> err

 where
  err = errorNoSuggestions $ ErrorPrimBadArgs (annotOfExp x) x args

  castToDoublesForce
   = castToDoubles True
  castToDoubles force ts
   | not force && all (\t -> baseType t == T.IntT) ts
   = return (T.IntT, [])
   | all (\t -> baseType t == T.IntT || baseType t == T.DoubleT) ts
   = return (T.DoubleT, fmap ((==T.IntT) . baseType) ts)
   | otherwise
   = notnumber

  aggu u = u { universeTemporality = AggU }

  notnumber
   = errorNoSuggestions $ ErrorPrimNotANumber (annotOfExp x) x args

  checknumber t
   | Just _ <- T.arithTypeOfValType t
   = return ()
   | otherwise
   = notnumber

  typeOp o
   = case o of
      ArithUnary _
       | [t] <- args
       , not $ isGroup $ universe t
       -> do    checknumber $ baseType t
                return ([], t)
       | otherwise
       -> err

      ArithBinary _
       | [a, b] <- args
       , Just u <- maxOf (universe a) (universe b)
       , not $ isGroup u
       -> do    (t, cs) <- castToDoubles False [a,b]
                return (cs, UniverseType u t)
       | otherwise
       -> err

      ArithDouble Div
       | [a,b] <- args
       , Just u <- maxOf (universe a) (universe b)
       , not $ isGroup u
       -> do    (_, cs) <- castToDoublesForce [a,b]
                return (cs, UniverseType u T.DoubleT)
       | otherwise
       -> err

      Relation _
       | [a,b] <- args
       , baseType a == baseType b
       , Just u <- maxOf (universe a) (universe b)
       , not $ isGroup u
       -> return ([], UniverseType u T.BoolT)

       | [a,b] <- args
       , Just u <- maxOf (universe a) (universe b)
       , not $ isGroup u
       -> do    (_, cs) <- castToDoubles False [a,b]
                return (cs, UniverseType u T.BoolT)

       | otherwise
       -> err

      TupleComma
       | [a,b] <- args
       , a' <- unwrapGroup a
       , b' <- unwrapGroup b
       , Just u <- maxOf (universe a') (universe b')
       -> return ([], UniverseType u $ T.PairT (baseType a') (baseType b'))
       | otherwise
       -> err

mkCastDouble :: Exp (a, UniverseType) n -> Exp (a, UniverseType) n
mkCastDouble xx
 = let (a,t) = annotOfExp xx
       t'    = t { baseType = T.DoubleT }
   in  Prim (a,t') (Fun ToDouble) `mkApp` xx

