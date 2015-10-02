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

import                  Icicle.Common.Base
import qualified        Icicle.Common.Fresh     as Fresh

import                  P

import                  Control.Monad.Trans.Class
import                  Control.Monad.Trans.Either
import qualified        Control.Monad.Trans.State as State

import                  Data.List (zip,unzip)
import qualified        Data.Map                as Map



-- | Defaulting any polymorphic Nums to Ints.
-- For example, if the query has
-- > feature salary ~> 1
-- this actually has type "forall a. Num a => a"
-- and we could safely use any number type.
--
-- We should really also default anything else to unit.
-- However, because defaulting only applies to the query, where the input stream must be concrete,
-- there should be no type variables left at the end except Nums.
defaults :: Ord n
         => Query'C a n
         -> Query'C a n
defaults q
 -- Find all Num constraints and map them to Int
 = let cm = Map.fromList
          $ concatMap (defaultOfConstraint . snd)
          $ annConstraints
          $ annotOfQuery q
   -- Just substitute the type vars in
   in substTQ cm q
 where
  defaultOfConstraint (CIsNum t)
   -- It must be a type variable - if it isn't it either already has a concrete
   -- Num type such as Int, or it is a type error
   | TypeVar n <- t
   = [(n, IntT)]
   | otherwise
   = []
  -- Everything else should really be known by this stage.
  -- These shouldn't actually occur.
  defaultOfConstraint (CEquals _ _)
   = []
  defaultOfConstraint (CReturnOfLetTemporalities _ _ _)
   = []
  defaultOfConstraint (CReturnOfLatest _ _ _)
   = []
  defaultOfConstraint (CPossibilityJoin _ _ _)
   = []
  defaultOfConstraint (CTemporalityJoin _ _ _)
   = []
  defaultOfConstraint (CExtractTemporality _ _ _)
   = []



-- | Generate constraints for an entire query.
-- We take the map of types of all imported functions.
constraintsQ
        :: Ord n
        => Map.Map (Name n) (FunctionType n)
        -> Query a n
        -> EitherT (CheckError a n) (Fresh.Fresh n) (Query'C a n)
constraintsQ env q
 -- Silly monad transformer gymnastics
 = evalGen (fst <$> generateQ q) (CheckState env [])


-- | Generate constraints for top-level query.
-- The Gen monad stack has a fresh name supply, as well as the state of
-- constraints and environment mapping from name to type.
--
-- The constraints flow upwards, bottom-up, but are also discharged and simplified at every level.
-- The substitutions from discharged constraints also flow upwards.
--
-- The environment mapping flows downwards, with new names only available in lexically nested subexpressions and so on.
--
-- Shoehorning these two differently flowing contexts into a state monad was a mistake, and is a constant source of bugs.
--
generateQ :: Ord n => Query a n -> Gen a n (Query'C a n, SubstT n)
-- End of query - at this stage it is just an expression with no contexts.
generateQ (Query [] x)
 = do   (x',s) <- generateX x
        return (Query [] x', s)

-- Query with a context
generateQ qq@(Query (c:_) _)
 -- Discharge any constraints on the result
 = discharge (annAnnot.annotOfQuery) substTQ
 $ case c of
    -- In the following "rules",
    --  x't stands for "temporality of x";
    --  x'p "possibility of x";
    --  x'd "data type of x"
    --  and so on.
    --
    -- >   windowed n days ~> Aggregate x'p x'd
    -- > : Aggregate x'p x'd
    Windowed _ from to
     -> do  (q',sq,t') <- rest
            -- Windowed only works for aggregates
            requireAgg t'
            let t'' = canonT $ Temporality TemporalityAggregate t'
            with q' sq t'' $ \a' -> Windowed a' from to

    -- >   latest n ~> x't x'p x'd
    -- > : Aggregate Possibly (ReturnOfLatest x't x'd)
    Latest _ i
     -> do  (q',sq,tq) <- rest
            retDat <- TypeVar <$> fresh
            let (tmpq,_,datq) = decomposeT tq

            -- Latest works for aggregates or elements.
            -- If the end is an element, such as
            --
            -- > latest 3 ~> value
            --
            -- the result type is actually an array of the last three elements.
            --
            --
            -- However, if we are in a function definition right now such as
            --
            -- > silly_function x = latest 3 ~> x
            --
            -- we do not necessarily know whether "x" is an element or an aggregate;
            -- its temporality is still just a type variable.
            --
            -- So instead of making a choice prematurely, we introduce a constraint:
            --
            -- > silly_function x = latest 3 ~> x
            -- >  : forall (x't : Temporality)
            -- >           (x'd : Data)
            -- >           (ret : Data)
            -- >    (ret = ReturnOfLatest x't x'd)
            -- > => x't x'd -> Aggregate ret
            --
            -- The definition for ReturnOfLatest is in Icicle.Source.Type.Constraints, but is something like
            -- > ReturnOfLatest Aggregate d = d
            -- > ReturnOfLatest Element   d = Array d
            --
            require a $ CReturnOfLatest retDat (fromMaybe TemporalityPure tmpq) datq
            let t'  = canonT
                    $ Temporality TemporalityAggregate
                    $ Possibility PossibilityPossibly retDat
            with q' sq t' $ \a' -> Latest a' i

    -- >   group (Element k'p k'd) ~> Aggregate v'p v'd
    -- > : Aggregate (PossibilityJoin k'p v'p) (Group k'd v'd)
    GroupBy _ x
     -> do  (x',sx) <- generateX x
            (q',sq,tval) <- rest
            let tkey = annResult $ annotOfExp x'
            requireTemporality tkey TemporalityElement
            requireAgg tval

            poss <- requirePossibilityJoin tkey tval

            let t'  = canonT
                    $ Temporality TemporalityAggregate
                    $ Possibility poss
                    $ GroupT tkey tval

            with q' (compose sx sq) t' $ \a' -> GroupBy a' x'

    -- >   group fold (k, v) = ( |- Q : Aggregate g'p (Group a'k a'v))
    -- >   ~> (k: Element a'k, v: Element a'v |- Aggregate a'p a)
    -- >    : Aggregate (PossibilityJoin g'p a'p) a
    GroupFold _ k v x
     -> do  (x',sx)    <- generateX x
            retKey     <- Temporality TemporalityElement . TypeVar <$> fresh
            retVal     <- Temporality TemporalityElement . TypeVar <$> fresh
            (q',sq,t') <- withBind k retKey
                        $ withBind v retVal rest
            let tgroup  = annResult $ annotOfExp x'
            requireAgg  t'
            requireAgg  tgroup
            requireData tgroup $ GroupT retKey retVal
            poss <- requirePossibilityJoin tgroup t'

            let t''     = canonT
                        $ Temporality TemporalityAggregate
                        $ Possibility poss t'
            with q' (compose sx sq) t'' $ \a' -> GroupFold a' k v x'

    -- >   distinct (Element k'p k'd) ~> Aggregate v'p v'd
    -- > : Aggregate (PossibilityJoin k'p v'p) v'd
    Distinct _ x
     -> do  (x',sx) <- generateX x
            (q',sq,t') <- rest
            let tkey = annResult $ annotOfExp x'
            requireTemporality tkey TemporalityElement
            requireAgg t'
            poss <- requirePossibilityJoin tkey t'

            let t'' = canonT
                    $ Temporality TemporalityAggregate
                    $ Possibility poss t'
            with q' (compose sx sq) t'' $ \a' -> Distinct a' x'

    -- >   filter (Element p'p Bool) ~> Aggregate x'p x'd
    -- > : Aggregate (PossibilityJoin p'p x'p) x'd
    Filter _ x
     -> do  (x',sx)    <- generateX x
            (q',sq,t') <- rest
            let pred    = annResult $ annotOfExp x'
            requireTemporality pred TemporalityElement
            requireData        pred BoolT
            requireAgg t'
            poss <- requirePossibilityJoin pred t'

            let t'' = canonT
                    $ Temporality TemporalityAggregate
                    $ Possibility poss t'
            with q' (compose sx sq) t'' $ \a' -> Filter a' x'

    -- >     let fold  bind = ( |- Pure z'p a'd) : ( bind : Element z'p a'd |- Element k'p a'd)
    -- >  ~> (bind : Aggregate k'p a'd |- Aggregate r'p r'd)
    -- >   :  Aggregate r'p r'd
    --
    -- >     let fold1 bind = ( |- Element z'p a'd) : ( bind : Element z'p a'd |- Element k'p a'd)
    -- >  ~> (bind : Aggregate Possibly a'd |- Aggregate r'p r'd)
    -- >   :  Aggregate r'p r'd
    LetFold _ f
     -> do  (i,si) <- generateX $ foldInit f
            (w,sw) <- withBind (foldBind f) (canonT $ Temporality TemporalityElement $ annResult $ annotOfExp i)
                    $ generateX $ foldWork f

            let bindType
                 | FoldTypeFoldl1 <- foldType f
                 = canonT
                 $ Temporality TemporalityAggregate
                 $ Possibility PossibilityPossibly
                 $ annResult $ annotOfExp w

                 | otherwise
                 = canonT
                 $ Temporality TemporalityAggregate
                 $ annResult $ annotOfExp w

            (q',sq,t') <- withBind (foldBind f) bindType rest

            case foldType f of
             FoldTypeFoldl1
              -> requireTemporality (annResult $ annotOfExp i) TemporalityElement
             FoldTypeFoldl
              -> requireTemporality (annResult $ annotOfExp i) TemporalityPure

            requireAgg t'
            requireTemporality (annResult $ annotOfExp w) TemporalityElement

            let (_,_,it) = decomposeT $ annResult $ annotOfExp i
            let (_,_,wt) = decomposeT $ annResult $ annotOfExp w

            require a $ CEquals it wt
            let t'' = canonT $ Temporality TemporalityAggregate t'
            let s'  = si `compose` sw `compose` sq
            with q' s' t'' $ \a' -> LetFold a' (f { foldInit = i, foldWork = w })

    -- Lets are not allowed if it means the binding would not be able to be used in the body.
    -- For example, trying to bind an Aggregate where the body is an Element, means that the
    -- definition cannot possibly be used: any reference to the binding would force the body to be
    -- an Aggregate itself.
    --
    -- The constraint "let't = ReturnOfLetTemporalities def't body't" is used for this.
    -- 
    -- >   let n = ( |- def't def'p def'd )
    -- >    ~> ( n : def't def'p def'd |- body't body'p body'd )
    -- > : (ReturnOfLetTemporalities def't body't) body'p body'd
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

  -- Generate constraints for the remainder of the query, and rip out the result type
  rest
   = do (q',s') <- generateQ (qq { contexts = drop 1 $ contexts qq })
        return (q', s', annResult $ annotOfQuery q')

  -- Rebuild the result with given substitution and type,
  -- using current constraints
  with q' s' t' c'
   = do cs <- stateConstraints <$> (lift $ lift State.get)
        let a' = Annot a t' cs
        return (q' { contexts = c' a' : contexts q' }, s')

  -- Helpers for adding constraints
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

  requirePossibilityJoin t1 t2
   = do poss <- TypeVar <$> fresh
        require a $ CPossibilityJoin poss (getPossibilityOrDefinitely t1) (getPossibilityOrDefinitely t2)
        return poss

-- | Generate constraints for expression
generateX :: Ord n => Exp a n -> Gen a n (Exp'C a n, SubstT n)
generateX x
 = discharge (annAnnot.annotOfExp) substTX
 $ case x of
    -- Variables are super easy!
    Var a n
     -> do (argsT, resT, fErr) <- lookup a n
           when (not $ null argsT)
             $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])
           annotate Map.empty resT $ \a' -> Var a' n

    -- Nested queries are also easy
    Nested _ q
     -> do (q',sq) <- generateQ q
           annotate sq (annResult $ annotOfQuery q') $ \a' -> Nested a' q'

    -- Applications are a bit more complicated.
    -- If your function expects an argument with a particular temporality, and you give it that temporality, it is fine; function application is as usual.
    -- (This is even if it expects Pure, and you apply a Pure)
    -- If your function expects a pure argument, and you give it a temporality such as Element, that means the argument must be
    -- implicitly unboxed to a pure computation, and then the result of application being reboxed as an Element.
    -- Reboxing can only work if the result type is pure or the desired temporality; you could not rebox an Aggregate result to an Element.
    --
    -- Look at an application of (+), for example:
    --  (+) : Pure Int -> Pure Int -> Pure Int
    -- However if one of its arguments is an Element:
    --  (x : Pure Int) + (y : Element Int) : Element Int
    -- here the y is unboxed, then the result is reboxed.
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

    -- Unapplied primitives should be relatively easy
    Prim a p
     -> do (argsT, resT, fErr) <- primLookup a p
           when (not $ null argsT)
             $ hoistEither $ errorNoSuggestions (ErrorFunctionWrongArgs a x fErr [])
           annotate Map.empty resT $ \a' -> Prim a' p

    -- Cases require:
    --
    --  1. Alternatives to have "join-able" types, i.e. no mixing of aggregates and elements.
    --  2. The return temporality is the join of the scrutinee with the alternatives.
    --  3. The type of the scrutinee is compatible with all patterns.
    --
    Case a scrut pats
     -> do (scrut', sub) <- generateX scrut

           -- Destruct the scrutinee type into the base type
           -- and the temporality (defaulting to Pure).
           let scrutT  = annResult $ annotOfExp scrut'
           scrutTy    <- TypeVar <$> fresh
           scrutTm    <- TypeVar <$> fresh
           require a $ CExtractTemporality scrutTm scrutTy scrutT

           -- Require the scrutinee and the alternatives to have compatible temporalities.
           returnType    <- TypeVar <$> fresh
           returnTemp    <- TypeVar <$> fresh
           returnTemp'   <- TypeVar <$> fresh
           require a $ CTemporalityJoin returnTemp' scrutTm returnTemp
           (pats', subs) <- unzip <$> generateP a scrutT returnType returnTemp pats

           let t' = Temporality returnTemp returnType
           annotate (Map.unions (sub : subs)) t' $ \a' -> Case a' scrut' pats'
  where
  annotate s' t' f
   = do cs <- stateConstraints <$> (lift $ lift State.get)
        let a' = Annot (annotOfExp x) t' cs
        return (f a', s')


generateP
  :: Ord n
  => a
  -> Type n                 -- ^ scrutinee type
  -> Type n                 -- ^ result base type
  -> Type n                 -- ^ result temporality
  -> [(Pattern n, Exp a n)]
  -> Gen a n [((Pattern n, Exp'C a n), SubstT n)]
generateP ann _ _ resTp []
 = do   require ann $ CEquals resTp TemporalityPure
        return []

generateP ann scrutTy resTy resTm ((pat, alt):rest)
 = do   e <- lift $ lift State.get
        t <- goPat pat

        let (_,_,datS) = decomposeT $ canonT scrutTy
        require (annotOfExp alt) $ CEquals datS t

        (alt',sub) <- generateX alt
        let altTy'  = annResult (annotOfExp alt')
        altTy      <- TypeVar <$> fresh
        altTp      <- TypeVar <$> fresh

        -- Require alternative types to have the same temporality if they
        -- do have temporalities. Otherwise defaults to TemporalityPure.
        require (annotOfExp alt) $ CExtractTemporality altTp altTy altTy'

        -- Require the alternative types without temporality to be the same.
        requireData resTy altTy

        -- Require return temporality to be compatible with alternative temporalities.
        resTp' <- TypeVar <$> fresh
        require (annotOfExp alt) $ CTemporalityJoin resTm resTp' altTp

        e'    <- lift $ lift State.get
        lift $ lift $ State.put (e' { stateEnvironment = stateEnvironment e })
        rest' <- generateP ann scrutTy resTy resTp' rest

        return (((pat,alt'), sub):rest')

 where
  requireData t1 t2
   = let (_,_,d1) = decomposeT t1
         (_,_,d2) = decomposeT t2
     in  require ann $ CEquals d1 d2

  goPat PatDefault
   = TypeVar <$> fresh
  goPat (PatVariable n)
   = do let (tmpS,posS,_) = decomposeT $ canonT scrutTy
        datV <- TypeVar <$> fresh
        bind n $ recomposeT (tmpS, posS, datV)
        return datV

  goPat (PatCon c pats)
   = case c of
      ConSome
       | [p] <- pats
       -> OptionT <$> goPat p
       | otherwise
       -> err
      ConNone
       | [] <- pats
       -> OptionT <$> (TypeVar <$> fresh)
       | otherwise
       -> err
      ConTuple
       | [a,b] <- pats
       -> PairT <$> goPat a <*> goPat b
       | otherwise
       -> err
      ConTrue
       | [] <- pats
       -> return BoolT
       | otherwise
       -> err
      ConFalse
       | [] <- pats
       -> return BoolT
       | otherwise
       -> err
      ConLeft
       | [p] <- pats
       -> SumT <$> goPat p <*> (TypeVar <$> fresh)
       | otherwise
       -> err
      ConRight
       | [p] <- pats
       -> SumT <$> (TypeVar <$> fresh) <*> goPat p
       | otherwise
       -> err
      ConError _
       | [] <- pats
       -> return ErrorT
       | otherwise
       -> err


  err = hoistEither $ errorNoSuggestions (ErrorCaseBadPattern (annotOfExp alt) pat)


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


