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
        :: Ord n
        => Features () n
        -> QueryTop (Annot a n) n
        -> FreshT n (Either (ConvertError a n)) (C.Program () n)
convertQueryTop feats qt
 = do   inp <- fresh
        (ty,fs) <- lift
                 $ maybeToRight (ConvertErrorNoSuchFeature (feature qt))
                 $ Map.lookup (feature qt) (featuresConcretes feats)

        inpTy <- case valTypeOfType ty of
                  Nothing -> lift $ Left $ ConvertErrorCannotConvertType (annAnnot $ annotOfQuery $ query qt) ty
                  Just t' -> return t'
        let inpTy'dated = T.PairT inpTy T.DateTimeT

        (bs,ret) <- evalStateT (convertQuery $ query qt) (ConvertState inp inpTy'dated fs Map.empty)
        let bs'   = strm inp C.Source <> bs
        return (programOfBinds (queryName qt) inpTy bs' () ret)


-- | Convert a Query to Core
-- This takes the name of the input stream, the element types of the input,
-- and the query to transform.
-- It returns a list of program bindings, as well as the name of the binding
-- that is being "returned" in the program - essentially the last added binding.
convertQuery
        :: Ord n
        => Query (Annot a n) n
        -> ConvertM a n (CoreBinds () n, Name n)
convertQuery q
 = case contexts q of
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
     -> do  n'      <- lift fresh
            nv      <- lift fresh
            e'      <- convertWithInputName nv $ convertExp e

            (bs, b) <- convertWithInputName n' $ convertQuery q'
            (inpstream, inpty) <- convertInput

            let bs'  = strm n' (C.STrans (C.SFilter inpty) (CE.xLam nv inpty e') inpstream) <> bs

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
     -> do  n'      <- lift fresh
            (bs, b) <- convertWithInputName n' $ convertQuery q'

            let newerThan' =      convertWindowUnits newerThan
            let olderThan' = fmap convertWindowUnits olderThan

            (inpstream, inpty) <- convertInput
            let bs'  = strm n' (C.SWindow inpty newerThan' olderThan' inpstream) <> bs
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
    (Latest _ i : _)

     -- Special case: just a value.
     -- We still need to support eg
     --
     -- "latest 5 ~> filter P ~> Elem"
     -- and also
     -- "latest 5 ~> let V = Elem ~> Elem"
     --
     | case getTemporalityOrPure $ annResult $ annotOfQuery q' of
        TemporalityElement  -> True
        TemporalityPure     -> True
        _                   -> False
     -> do  n'arr   <- lift fresh
            n'map   <- lift fresh

            n'v     <- lift fresh

            x' <- convertWithInputName n'v $ convertExpQ q'
            t' <- convertValType' $ annResult $ annotOfQuery q'

            (inpstream, inpty) <- convertInput

            -- Do the map before the latest.
            -- Same result, just requires a smaller buffer
            let bmap  = strm  n'map
                      $ C.STrans (C.SMap inpty t') (CE.xLam n'v inpty x') inpstream
            let barr  = red  n'arr
                      $ C.RLatest t' (CE.constI i) n'map
            return (bmap <> barr, n'arr)

     | otherwise
     -> do  n'arr   <- lift fresh
            n'fold  <- lift fresh
            n'xtra  <- lift fresh

            n'a     <- lift fresh
            n'v     <- lift fresh

            -- Destruct the aggregate into a fold expression:
            -- x (fold k z inps)
            -- where x :: tV -> retty
            res
                    <- convertWithInputName n'v $ convertFold q'

            let tV'  = typeFold res

            (inpstream, inpty) <- convertInput

            -- Because Map_insertOrUpdate and Array_fold take their "k" in a different order,
            -- we need to flip the k here.
            let k'    = CE.xLam n'a tV'
                      $ CE.xLam n'v inpty
                      $ beta
                      ( foldKons res CE.@~ CE.xVar n'a)

            -- Construct the "latest" reduction,
            -- the fold over the resulting array,
            -- and the extraction of the actual result of the array.
            -- (See convertFold)
            let barr  = red  n'arr   (C.RLatest inpty (CE.constI i) inpstream)
            let bfold = post n'fold  (CE.xPrim (C.PrimFold (C.PrimFoldArray inpty) tV')
                                      CE.@~ k' CE.@~ beta (foldZero res) CE.@~ CE.xVar n'arr)
            let bxtra = post n'xtra (beta (mapExtract res CE.@~ CE.xVar n'fold))

            return (barr <> bfold <> bxtra, n'xtra)

    -- Convert a group by into the construction of a Map.
    -- The key is the "by" of the group, while the value is the result of the
    -- aggregate over the partitioned results.
    --
    -- The result of a group by must be an aggregate, which means we have a
    -- relatively good idea of an upper bound on memory here,
    -- as long as the "by" is something like an enum, and the "value" has bounded memory.
    --
    (GroupBy (Annot { annAnnot = ann, annResult = retty }) e : _)
     -> do  t1 <- getGroupByMapType ann retty
            n'      <- lift fresh
            n''     <- lift fresh
            nmap    <- lift fresh
            nval    <- lift fresh

            -- Convert the rest of the query into a fold.
            -- We have the "k"onstructor, the "z"ero, and the e"x"tract,
            -- as well as the intermediate result type before extraction.
            -- See convertFold.
            res     <- convertWithInputName nval $ convertFold q'

            let tV'  = typeFold res
            let t1'  = t1
            let t2'  = typeExtract res

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertWithInputName nval $ convertExp e

            let mapt = T.MapT t1' tV'


            (inpstream, inpty) <- convertInput

            -- For each input element, we use the group by as the key, and insert into a map.
            --
            -- If the map already has the key, we perform the "k" on the current element
            -- and the existing value - the fold accumulator.
            --
            -- If the map doesn't have the key, we insert the "k" of the current element
            -- with the zero accumulator.
            -- This is because the map doesn't start with "zero"s in it, unlike a normal fold.
            let priminsert
                    = C.PrimMapInsertOrUpdate t1' tV'

            let insertOrUpdate
                    = CE.makeApps () (CE.xPrim $ C.PrimMap $ priminsert)
                    [ foldKons res
                    , foldKons res `CE.xApp` foldZero res
                    , e'
                    , CE.xVar nmap]

            let insertOrUpdate'
                    = beta
                    $ CE.xLam nmap mapt
                    $ CE.xLam nval inpty
                    $ insertOrUpdate

            let emptyMap
                    = CE.emptyMap t1' tV'

            -- Perform the map fold
            let r = red n' 
                  $ C.RFold inpty mapt insertOrUpdate' emptyMap inpstream

            -- After all the elements have been seen, we go through the map and perform
            -- the "extract" on each value.
            -- This performs any fixups that couldn't be performed during the fold.

            let mapResult
                    = CE.xPrim (C.PrimMap $ C.PrimMapMapValues t1' tV' t2')
                    CE.@~ beta (mapExtract res)
                    CE.@~ CE.xVar n'

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
            convertModifyFeatures (Map.delete k)
            k' <- convertFreshenAdd k
            convertModifyFeatures (Map.delete v)
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
            (inpstream, inpty) <- convertInput
            let tval = inpty

            n'      <- lift fresh
            n''     <- lift fresh
            nmap    <- lift fresh
            nacc    <- lift fresh
            nval    <- lift fresh
            n'ignore<- lift fresh

            -- Convert the rest of the query into a fold.
            -- This is executed as a Map fold at the end, rather than
            -- as a stream fold.
            res     <- convertWithInputName nval $ convertFold q'

            let tV'  = typeFold res

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertWithInputName nval $ convertExp e

            let mapt = T.MapT tkey tval

            -- This is a little bit silly -
            -- this "insertOrUpdate" should really just be an insert.
            -- Just put each element in the map, potentially overwriting the last one.
            let insertOrUpdate
                  = beta
                  $ CE.xLam nmap mapt
                  $ CE.xLam nval inpty
                  ( CE.xPrim (C.PrimMap $ C.PrimMapInsertOrUpdate tkey tval)
                    CE.@~ (CE.xLam n'ignore inpty $ CE.xVar nval)
                    CE.@~ (CE.xVar nval)
                    CE.@~  e'
                    CE.@~ CE.xVar nmap )

            -- Perform the map fold
            let r = red n'
                  $ C.RFold inpty mapt insertOrUpdate 
                  ( CE.emptyMap tkey tval)
                    inpstream

            -- Perform a fold over that map
            let p = post n''
                  $ beta
                  ( mapExtract res CE.@~
                  ( CE.xPrim
                        (C.PrimFold (C.PrimFoldMap tkey tval) tV')
                    CE.@~ (CE.xLam nacc     tV'
                         $ CE.xLam n'ignore tkey
                         $ CE.xLam nval     tval
                         ( foldKons res CE.@~ CE.xVar nacc ))
                    CE.@~ foldZero res
                    CE.@~ CE.xVar n'))

            return (r <> p, n'')

    (Let _ b def : _)
     -> case getTemporalityOrPure $ annResult $ annotOfExp def of
         TemporalityElement
          -> do t' <- convertValType' $ annResult $ annotOfExp def
                (inpstream, inpty) <- convertInput
                let inpty' = T.PairT t' inpty

                n'e     <- lift fresh

                e'      <- convertWithInputName n'e $ convertExp def

                let xfst = CE.xApp
                         $ CE.xPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t' inpty
                let xsnd = CE.xApp
                         $ CE.xPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t' inpty

                convertModifyFeatures (Map.insert b (annResult $ annotOfExp def, xfst) . Map.map (\(t,f) -> (t, f . xsnd)))

                let pair = (CE.xPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair t' inpty)
                         CE.@~ e' CE.@~ CE.xVar n'e


                n'r     <- lift fresh
                let bs   = strm n'r
                         $ C.STrans (C.SMap inpty inpty') (CE.xLam n'e inpty pair) inpstream
                (bs', n'') <- convertWithInput n'r inpty' $ convertQuery q'

                return (bs <> bs', n'')

         TemporalityPure
          -> do e'      <- convertExp def
                convertModifyFeatures (Map.delete b)
                b'      <- convertFreshenAdd b
                let bs   = pre b' e'
                (bs', n') <- convertQuery q'
                return (bs <> bs', n')

         TemporalityAggregate
          -> do (bs,n')      <- convertReduce    def
                convertModifyFeatures (Map.delete b)
                b'           <- convertFreshenAdd b
                (bs',n'')    <- convertQuery     q'
                return (bs <> post b' (CE.xVar n') <> bs', n'')

         _
          -> convertError $ ConvertErrorGroupByHasNonGroupResult (annAnnot $ annotOfExp def) (annResult $ annotOfExp def)


    -- Converting fold1s.
    (LetFold _ f@Fold{ foldType = FoldTypeFoldl1 } : _)
     -> do  -- Type helpers
            tU <- convertValType' $ annResult $ annotOfExp $ foldWork f
            let tO = T.OptionT tU

            -- Generate fresh names
            -- Element of the stream
            -- :                input type
            n'elem <- lift fresh
            -- Current accumulator
            -- : Option tU
            n'a     <- lift fresh


            z   <- convertWithInputName n'elem $ convertExp (foldInit f)
            -- Current accumulator is only available in worker
            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            convertModifyFeatures (Map.delete (foldBind f))
            n'a'' <- convertFreshenAdd $ foldBind f
            k   <- convertWithInputName n'elem $ convertExp (foldWork f)

            -- Wrap zero and kons up in Some
            let z' = som tU $ z
            let k' = som tU $ k


            (inpstream, inpty) <- convertInput

            -- Worker function of the fold
            let go  = CE.xLam n'a tO
                    ( CE.xLam n'elem inpty
                    $ opt tU tO
                    ( CE.xLam n'a'' tU k')
                      z'
                      (CE.xVar n'a))


            -- Bind the fold to a fresh name
            n' <- lift fresh
            let bs = red n'
                        (C.RFold inpty tO go (none tU) inpstream)
                     -- Then unwrap the actual result and bind it
                  <> post n'a''
                        (opt tU tU
                            -- If the outer layer is a "Some",
                            -- just return inner layer as-is
                            (CE.xLam n'a'' tU $ CE.xVar n'a'')
                            -- Outer layer is a "None", so throw an exception
                            (CE.xValue tU $ VException ExceptFold1NoValue)
                            (CE.xVar n'))


            (bs', n'')      <- convertQuery q'

            return (bs <> bs', n'')


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

            -- Generate fresh names
            -- Element of the stream
            -- :                input type
            n'elem <- lift fresh

            z   <- convertWithInputName n'elem $ convertExp (foldInit f)
            -- Current accumulator is only available in worker
            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            convertModifyFeatures (Map.delete (foldBind f))
            n'a <- convertFreshenAdd $ foldBind f
            k   <- convertWithInputName n'elem $ convertExp (foldWork f)

            (inpstream, inpty) <- convertInput
            -- Worker function of the fold
            let go  = CE.xLam n'a tU
                    $ CE.xLam n'elem inpty k

            -- Bind the fold to the original name
            let bs = red n'a (C.RFold inpty tU go z inpstream)

            (bs', n'')      <- convertQuery q'

            return (bs <> bs', n'')

 where
  -- The remaining query after the current context is removed
  q' = q { contexts = drop 1 $ contexts q }

  -- Group bys can live in either Aggregate or Group universe.
  -- Because Core is explicitly typed, we need to pull out the key type and value type.
  getGroupByMapType ann ty
   | (_,_,t') <- decomposeT ty
   , (GroupT tk _tv) <- t'
   = convertValType' tk
   | otherwise
   = convertError $ ConvertErrorGroupByHasNonGroupResult ann ty

  -- Get the key and value type of a group inside a group-fold.
  getGroupFoldType a e
   = case e of
       Nested (Annot { annResult = ty }) _
         -> do t <- convertValType' ty
               case t of
                 T.MapT tk tv -> return (tk, tv)
                 _            -> convertError $ ConvertErrorGroupFoldNotOnGroup a e
       _ -> convertError $ ConvertErrorGroupFoldNotOnGroup a e

  -- Perform beta reduction, just to simplify the output a tiny bit.
  beta = Beta.betaToLets ()
       . Beta.beta Beta.isSimpleValue

  -- Some helpers for generating Option expressions
  -- Note that the "t"s here are the types without the outermost
  -- layer of Option wrapping
  opt t tret ss nn scrutinee
   = CE.xPrim (C.PrimFold (C.PrimFoldOption t) tret)
     CE.@~ ss CE.@~ nn CE.@~ scrutinee
  none t
   = CE.xValue (T.OptionT t) VNone
  som t
   = CE.some t

  convertValType' = convertValType (annAnnot $ annotOfQuery q)


-- | Convert an Aggregate computation at the end of a query.
-- This must be an aggregate, some primitive applied to at least one aggregate expression,
-- or a nested query.
convertReduce
        :: Ord n
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
 = do   let (pats,alts) = unzip patalts

        scrut' <- convertReduce scrut
        pats'  <- convertCaseFreshenPats pats
        alts'  <- mapM convertExp alts

        let bs' = fst scrut'

        let sX  = CE.xVar $ snd scrut'

        scrutT <- convertValType ann $ annResult $ annotOfExp scrut
        resT   <- convertValType ann $ retty

        x'     <- convertCase xx sX (pats' `zip` alts') scrutT resT
        nm     <- lift fresh

        let b'  | TemporalityPure <- getTemporalityOrPure retty
                = pre nm x'
                | otherwise
                = post nm x'

        return (b' <> bs', nm)

 -- It's not a variable or a nested query,
 -- so it must be an application of a non-primitive
 | otherwise
 = convertError $ ConvertErrorExpApplicationOfNonPrimitive (annAnnot $ annotOfExp xx) xx
