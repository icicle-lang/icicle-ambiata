-- | Converting Source to Core.
--
-- This is all very rough around the edges at the moment.
-- Some fundamental things are missing, like dealing with optional values:
-- the "newest v" aggregate returns a "Maybe v", but we actually want to implicitly
-- unbox that.
-- So, for example,
--
-- > let x = newest value ~> x * 5
--
-- here, "x" should be bound to a simple "Int", but the outer query should return
-- a "Maybe Int".
--
-- However, some things like filtering, grouping, latest and simple aggregates do work.
--
-- (This is why there are no property tests yet.)
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.ToCore (
    convertQueryTop
  , convertQuery
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.ToCore.Context
import                  Icicle.Source.ToCore.Exp
import                  Icicle.Source.ToCore.Fold
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Exp.Prim.Minimal as Min
import qualified        Icicle.Common.Exp.Simp.Beta    as Beta
import qualified        Icicle.Common.Type as T
import                  Icicle.Common.Fresh
import                  Icicle.Common.Base


import                  P

import                  Control.Monad.Morph
import                  Control.Monad.Trans.State.Lazy
import                  Data.List (zip, unzip)

import qualified        Data.Map as Map
import                  Data.Hashable (Hashable)


-- | Convert a top-level Query to Core.
--
-- The input query is annotated with its type information at every node.
-- This is essential for both putting type annotations on the explicitly typed core,
-- and also because the universe annotations determine where computations are done:
--
-- "Pure" computations become Pre-computations in Core,
-- since they must not use on any stream inputs;
--
-- "Elem" computations must be worker functions on filters, maps, and so on.
--
-- "AggU" or "Group" computations can be reductions on streams, or postcomputations.
--
convertQueryTop
        :: (Hashable n, Eq n)
        => Features () n
        -> QueryTop (Annot a n) n
        -> FreshT n (Either (ConvertError a n)) (C.Program () n)
convertQueryTop feats qt
 = do   inp         <- fresh
        factid      <- fresh
        facttime    <- fresh
        now         <- fresh
        (ty,fs) <- lift
                 $ maybeToRight (ConvertErrorNoSuchFeature (feature qt))
                 $ Map.lookup (feature qt) (featuresConcretes feats)

        inpTy <- case valTypeOfType ty of
                  Nothing -> lift $ Left $ ConvertErrorCannotConvertType (annAnnot $ annotOfQuery $ query qt) ty
                  Just t' -> return t'
        let inpTy'dated = T.PairT inpTy T.TimeT

        (bs,ret) <- evalStateT (do
                                       maybe (return ()) (flip convertFreshenAddAs now) $ featureNow feats
                                       convertQuery $ query qt)
                                   (ConvertState inp inpTy'dated factid facttime now fs Map.empty)
        return (programOfBinds (queryName qt) inpTy inp factid facttime now bs () ret)


-- | Convert a Query to Core
-- This takes the name of the input stream, the element types of the input,
-- and the query to transform.
-- It returns a list of program bindings, as well as the name of the binding
-- that is being "returned" in the program - essentially the last added binding.
convertQuery
        :: (Hashable n, Eq n)
        => Query (Annot a n) n
        -> ConvertM a n (CoreBinds () n, Name n)
convertQuery q
 = convertContext
 $ case contexts q of
    -- There are no queries left, so deal with simple aggregates and nested queries.
    []
     -> convertReduce (final q)

    -- Converting filters is probably the simplest conversion.
    --
    -- We create a fresh name for the "filter" binding we're creating,
    -- as well as a fresh name for the lambda variable for the predicate.
    --
    -- Then we convert the filter expression into a predicate, and wrap it in the lambda.
    --
    -- We convert the rest of the query and pass through the fresh filter binding's name,
    -- as that is what the data is operating on.
    (Filter _ e : _)
     -> do  e'      <- convertExp e
            (bs, b) <- convertQuery q'
            let bs' = filt e' (streams bs) <> bs { streams = [] }
            return (bs', b)


    -- Windowing in Core must be at the start of a chain of stream transformers.
    -- I think this is actually a shortcoming of Core, and can be relatively easily
    -- fixed there.
    -- For now, this doesn't support "filter X ~> window Y", but only "window Y ~> filter X".
    --
    -- We don't want unbounded "older than" windowing because then the window contents will
    -- continue to grow unboundedly.
    -- Instead, only allow "older than" windowing if there is also a "newer than" bound on the
    -- other side.
    -- We could support "older than" by storing reduce result of the end of window and
    -- storing all corresponding newer thans in the snapshot, so if this ends up being an issue
    -- we can address it.
    (Windowed _ newerThan olderThan : _)
     -> do  (bs, b) <- convertQuery q'
            now  <- convertDateName
            time <- convertFactTimeName
            fact <- convertFactIdName

            let e'  = CE.makeApps () (CE.xPrim $ C.PrimWindow newerThan olderThan)
                    [ CE.xVar now, CE.xVar time, CE.xVar fact ]
            let bs' = filt e' (streams bs) <> bs { streams = [] }
            return (bs', b)


    -- TODO: latest can actually have multiple 'aggregate passes',
    -- eg you should be able to do
    --
    -- > latest 5 ~> value / sum value
    --
    -- since this only requires a second pass over the small array that is
    -- already in memory.
    --
    -- However, at the moment we only support folds with single pass,
    -- and not returning the whole array.
    (Latest _ _ : _)
     -> do  -- We can cheat!
            -- Convert the entire latest query as a fold.
            res        <- convertFold q
            n'red      <- lift fresh
            n'ret      <- lift fresh

            let k'      = beta
                        ( foldKons res CE.@~ CE.xVar n'red)

            let b'red   = sfold  n'red (typeFold res) (foldZero res) k'
            let b'ret   = post n'ret $ beta (mapExtract res CE.@~ CE.xVar n'red)

            return (b'red <> b'ret, n'ret)

    -- Convert a group by into the construction of a Map.
    -- The key is the "by" of the group, while the value is the result of the
    -- aggregate over the partitioned results.
    --
    -- The result of a group by must be an aggregate, which means we have a
    -- relatively good idea of an upper bound on memory here,
    -- as long as the "by" is something like an enum, and the "value" has bounded memory.
    --
    (GroupBy (Annot { annAnnot = ann, annResult = retty }) e : _)
     -> do  n'      <- lift fresh

            -- Convert the rest of the query into a fold.
            -- We have the "k"onstructor, the "z"ero, and the e"x"tract,
            -- as well as the intermediate result type before extraction.
            -- See convertFold.
            res     <- convertFold q'

            tK      <- convertValType' $ annResult $ annotOfExp e
            let tV   = typeFold res

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertExp e

            let mapt = T.MapT tK tV

            -- For each input element, we use the group by as the key, and insert into a map.
            --
            -- If the map already has the key, we perform the "k" on the current element
            -- and the existing value - the fold accumulator.
            --
            -- If the map doesn't have the key, we insert the "k" of the current element
            -- with the zero accumulator.
            -- This is because the map doesn't start with "zero"s in it, unlike a normal fold.
            let priminsert
                    = C.PrimMapInsertOrUpdate tK tV

            let insertOrUpdate
                    = beta
                    $ CE.makeApps () (CE.xPrim $ C.PrimMap $ priminsert)
                    [ foldKons res
                    , foldKons res `CE.xApp` foldZero res
                    , e'
                    , CE.xVar n']

            let emptyMap
                    = CE.emptyMap tK tV

            -- Perform the map fold
            let r = sfold n' mapt emptyMap insertOrUpdate

            -- After all the elements have been seen, we go through the map and perform
            -- the "extract" on each value.
            -- This performs any fixups that couldn't be performed during the fold.

            -- Unwrapped results
            (tKr,tXr) <- getGroupByMapType ann retty
            let isPossibly = PossibilityPossibly == getPossibilityOrDefinitely retty
                sumt | isPossibly
                     = T.SumT T.ErrorT $ T.MapT tKr tXr
                     | otherwise
                     = T.MapT tKr tXr
                emptyResult
                     | isPossibly
                     = CE.xValue sumt $ VRight $ VMap Map.empty
                     | otherwise
                     = CE.xValue sumt $ VMap Map.empty

            nkey    <- lift fresh
            nkey'   <- lift fresh
            nval    <- lift fresh
            nval'   <- lift fresh
            nval''  <- lift fresh
            nmap    <- lift fresh
            nmap'   <- lift fresh

            nErr    <- lift fresh

            let unwrapSum' chk = unwrapSum chk sumt nErr

            let rewrapSum' = rewrapSum isPossibly sumt

            let ins = CE.xLam nmap sumt
                    $ CE.xLam nkey tK
                    $ CE.xLam nval tV
                    $ unwrapSum' isPossibly
                                 (CE.xVar nmap) nmap' sumt
                    $ unwrapSum' (isAnnotPossibly $ annotOfExp e)
                                 (CE.xVar nkey) nkey' tK
                    $ unwrapSum' (isAnnotPossibly $ annotOfQuery q')
                                 (beta (mapExtract res CE.@~ CE.xVar nval)) nval' (typeExtract res)
                    $ rewrapSum'
                    $ CE.makeApps () (CE.xPrim $ C.PrimMap $ C.PrimMapInsertOrUpdate tKr tXr)
                    [ CE.xLam nval'' tXr $ CE.xVar nval''
                    , CE.xVar nval'
                    , CE.xVar nkey'
                    , CE.xVar nmap']


            let mapResult
                    = CE.xPrim (C.PrimFold (C.PrimFoldMap tK tV) sumt)
                    CE.@~ beta ins
                    CE.@~ emptyResult
                    CE.@~ CE.xVar n'

            n''     <- lift fresh
            let p   = post n'' mapResult

            return (r <> p, n'')


    -- Convert a group fold using a Map. Very similar to Group By, with an additional
    -- postcomputation.
    --
    -- The group itself constructs the Map and the group fold perform its aggregate
    -- on the Map.
    --
    (GroupFold (Annot { annAnnot = ann }) k v e : _ )
     -> do  (tk, tv) <- getGroupFoldType ann e

            n'   <- lift fresh
            nacc <- lift fresh

            -- Convert the inner group into a stream fold that produces a map.
            (bs, nm) <- convertReduce e

            -- The key and value will be available after the fold
            k' <- convertFreshenAdd k
            v' <- convertFreshenAdd v

            -- Convert the rest of the query into a map fold.
            res      <- convertFold q'
            let tacc  = typeFold res

            -- Perform the map fold.
            let p = post n'
                  $ beta
                  ( mapExtract res CE.@~
                  ( CE.xPrim
                      (C.PrimFold (C.PrimFoldMap tk tv) tacc)
                    CE.@~ ( CE.xLam nacc tacc
                          $ CE.xLam k'   tk
                          $ CE.xLam v'   tv
                              (foldKons res CE.@~ CE.xVar nacc))
                    CE.@~ foldZero res
                    CE.@~ CE.xVar nm))

            return (bs <> p, n')

    -- Distinct is very similar to a group by, except instead of performing the fold
    -- as the elements are seen into the map,
    -- we insert/update the element in the map, then fold over essentially the last-seen
    -- for each group.
    (Distinct _ e : _)
     -> do  tkey <- convertValType' $ annResult $ annotOfExp e

            n'      <- lift fresh
            n''     <- lift fresh
            n'key   <- lift fresh
            n'ignore<- lift fresh
            n'ignore2<- lift fresh

            -- Convert the rest of the query into a fold.
            -- This is executed as a Map fold at the end, rather than
            -- as a stream fold.
            res     <- convertFold q'

            let tV'  = typeFold res

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertExp e

            let mapt    = T.MapT tkey T.UnitT
            let pairt   = T.PairT mapt tV'

            let xfst    = CE.xApp
                        $ CE.xPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst mapt tV'
            let xsnd    = CE.xApp
                        $ CE.xPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd mapt tV'

            let pair x y= (CE.xPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair mapt tV')
                        CE.@~ x CE.@~ y

            -- This is a little bit silly -
            -- this "insertOrUpdate" should really just be an insert.
            -- Just put each element in the map, potentially overwriting the last one.
            let insert_map mm kk vv
                  = CE.xPrim (C.PrimMap $ C.PrimMapInsertOrUpdate tkey T.UnitT)
                    CE.@~ (CE.xLam n'ignore T.UnitT $ vv)
                    CE.@~ vv
                    CE.@~ kk
                    CE.@~ mm

            let update_step
                        = pair
                        ( insert_map (xfst $ CE.xVar n') (CE.xVar n'key) (CE.xValue T.UnitT VUnit) )
                        ( foldKons res CE.@~ xsnd (CE.xVar n') )

            let check_if_exists
                        = CE.xPrim (C.PrimMap (C.PrimMapLookup tkey T.UnitT))
                        CE.@~ xfst (CE.xVar n')
                        CE.@~ CE.xVar n'key

            let kons    = CE.xLet n'key e'
                        ( CE.xPrim (C.PrimFold (C.PrimFoldOption T.UnitT) pairt)
                        CE.@~ CE.xLam n'ignore2 T.UnitT (CE.xVar n')
                        CE.@~ update_step
                        CE.@~ check_if_exists )

            -- Perform the map fold
            let r = sfold n' pairt (pair (CE.emptyMap tkey T.UnitT) (foldZero res)) (beta kons)

            -- Perform a fold over that map
            let p = post n'' $ beta (mapExtract res CE.@~ xsnd (CE.xVar n'))

            return (r <> p, n'')

    (Let _ b def : _)
     -> case getTemporalityOrPure $ annResult $ annotOfExp def of
         TemporalityElement
          -> do t' <- convertValType' $ annResult $ annotOfExp def
                (inpstream, inpty) <- convertInput
                let inpty' = T.PairT t' inpty

                e'      <- convertExp def

                let xfst = CE.xApp
                         $ CE.xPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t' inpty
                let xsnd = CE.xApp
                         $ CE.xPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t' inpty

                convertModifyFeaturesMap (Map.insert b (FeatureVariable (annResult $ annotOfExp def) xfst False) . Map.map (\fv -> fv { featureVariableExp = featureVariableExp fv . xsnd }))

                let pairC l r
                     = (CE.xPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair t' inpty)
                         CE.@~ l CE.@~ r
                let pair = pairC e' (CE.xVar inpstream)
                let defT ty = CE.xValue ty (T.defaultOfType ty)
                let pairD= pairC (defT t') (defT inpty)


                n'r     <- lift fresh
                let bs   = sfold n'r inpty' pairD pair
                (bs', n'') <- convertWithInput n'r inpty' $ convertQuery q'

                return (bs <> bs', n'')

         TemporalityPure
          -> do e'      <- convertExp def
                b'      <- convertFreshenAdd b
                let bs   = pre b' e'
                (bs', n') <- convertQuery q'
                return (bs <> bs', n')

         TemporalityAggregate
          -> do (bs,n')      <- convertReduce    def
                b'           <- convertFreshenAdd b
                (bs',n'')    <- convertQuery     q'
                return (bs <> post b' (CE.xVar n') <> bs', n'')

         _
          -> convertError $ ConvertErrorGroupByHasNonGroupResult (annAnnot $ annotOfExp def) (annResult $ annotOfExp def)


    -- Converting fold1s.
    (LetFold (Annot { annAnnot = ann }) Fold{ foldType = FoldTypeFoldl1 } : _)
     -> convertError $ ConvertErrorImpossibleFold1 ann

    -- In comparison, normal folds are quite easy.
    --
    -- > let fold summ ~> 0 : summ + value ~> ...
    -- >
    -- > =====>
    -- >
    -- > Stream.fold
    -- > (\a : Int. \v : Value. a + v)
    -- > 0
    -- > stream
    --
    (LetFold _ f@Fold{ foldType = FoldTypeFoldl } : _)
     -> do  -- Type helpers
            tU <- convertValType' $ annResult $ annotOfExp $ foldWork f

            z   <- convertExp (foldInit f)
            -- Current accumulator is only available in worker
            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            n'a <- convertFreshenAdd $ foldBind f
            k   <- convertExp (foldWork f)

            -- Bind the fold to the original name
            let bs = sfold n'a tU z k

            (bs', n'')      <- convertQuery q'

            return (bs <> bs', n'')

 where
  -- The remaining query after the current context is removed
  q' = q { contexts = drop 1 $ contexts q }

  -- Group bys can live in either Aggregate or Group universe.
  -- Because Core is explicitly typed, we need to pull out the key type and value type.
  getGroupByMapType ann ty
   | (_,_,t') <- decomposeT ty
   , (GroupT tk tv) <- t'
   = (,) <$> convertValType' tk <*> convertValType' tv
   | (_,_,t') <- decomposeT ty
   , (SumT ErrorT (GroupT tk tv)) <- t'
   = (,) <$> convertValType' tk <*> convertValType' tv
   | otherwise
   = convertError $ ConvertErrorGroupByHasNonGroupResult ann ty

  -- Get the key and value type of a group inside a group-fold.
  getGroupFoldType a e
   = do t <- convertValType' $ annResult $ annotOfExp e
        case t of
         T.MapT tk tv -> return (tk, tv)
         _            -> convertError $ ConvertErrorGroupFoldNotOnGroup a e

  -- Perform beta reduction, just to simplify the output a tiny bit.
  beta = Beta.betaToLets ()
       . Beta.beta Beta.isSimpleValue

  convertValType' = convertValType (annAnnot $ annotOfQuery q)



-- | Convert an Aggregate computation at the end of a query.
-- This must be an aggregate, some primitive applied to at least one aggregate expression,
-- or a nested query.
convertReduce
        :: (Hashable n, Eq n)
        => Exp (Annot a n) n
        -> ConvertM a n (CoreBinds () n, Name n)
convertReduce xx
 | Just (p, Annot { annResult = ty }, args) <- takePrimApps xx
 -- For any primitives:
 --   recurse into its arguments and get bindings for them
 --   apply those bindings to the primitive, as a postcomputation
 --
 -- If the binding is Pure however, it must not rely on any aggregates,
 -- so it might as well be a precomputation.
 = do   (bs,nms) <- unzip <$> mapM convertReduce args
        let tys  = fmap (annResult . annotOfExp) args
        let xs   = fmap  CE.xVar           nms
        x' <- convertPrim p (annAnnot $ annotOfExp xx) ty (xs `zip` tys)

        nm  <- lift fresh

        let bs'  = mconcat bs
        let b''  | TemporalityPure <- getTemporalityOrPure ty
                 = pre nm x'
                 | otherwise
                 = post nm x'

        return (bs' <> b'', nm)


 -- Convert a nested query
 -- Any lets, folds etc bound in here will go out of scope at the end.
 -- So we revert the state at the end, clearing any bindings,
 -- rolling back to the old input type, etc.
 | Nested _ q   <- xx
 = do   o <- get
        r <- convertQuery q
        put o
        return r
 -- Any variable must be a let-bound aggregate, so we can safely assume it has a binding.
 | Var (Annot { annAnnot = ann }) v      <- xx
 = (,) mempty <$> convertFreshenLookup ann v


 | Case (Annot { annAnnot = ann, annResult = retty }) scrut patalts <- xx
 = do   scrut' <- convertReduce scrut

        -- Because the alternatives can contain more folds and stuff, we need to
        -- use convertReduce on them.
        -- However, because the pattern variables must be Aggregates, we know that
        -- any pattern variables will only be mentioned in the postcomputations.
        -- Therefore we need to pull out the postcomputations into a let,
        -- and stick them inside the new case alternative.
        --
        -- All the foldy bits of each alternative must be computed, because
        -- we won't know which ones will be needed until after they are run.
        let goPatAlt (p,alt)
                  = (,) <$> convertCaseFreshenPat p <*> convertReduce alt
        patalts' <- mapM goPatAlt patalts
        let pats' = fmap                 fst  patalts'
            alts' = fmap (pullPosts () . snd) patalts'

        let bs' = fst scrut' <> mconcat (fmap fst alts')

        let sX  = CE.xVar $ snd scrut'
        let aXs = fmap snd alts'

        scrutT <- convertValType ann $ annResult $ annotOfExp scrut
        resT   <- convertValType ann $ retty

        x'     <- convertCase xx sX (pats' `zip` aXs) scrutT resT
        nm     <- lift fresh

        let b'  | TemporalityPure <- getTemporalityOrPure retty
                = pre nm x'
                | otherwise
                = post nm x'

        return (bs' <> b', nm)

 -- It's not a variable or a nested query,
 -- so it must be an application of a non-primitive
 | otherwise
 = convertError $ ConvertErrorExpApplicationOfNonPrimitive (annAnnot $ annotOfExp xx) xx
