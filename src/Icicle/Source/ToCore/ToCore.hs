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

import                  Control.Monad.Trans.State.Lazy
import                  Control.Monad.Trans.Class
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
        => Features n
        -> QueryTop (a,UniverseType) n
        -> FreshT n (Either (ConvertError a n)) (C.Program n)
convertQueryTop feats qt
 = do   inp <- fresh
        (ty,fs) <- lift
                 $ maybeToRight (ConvertErrorNoSuchFeature (feature qt))
                 $ Map.lookup (feature qt) feats

        let inpTy       = ty
        let inpTy'dated = T.PairT inpTy T.DateTimeT

        (bs,ret) <- evalStateT (convertQuery $ query qt) (ConvertState inp inpTy'dated fs Map.empty)
        let bs'   = strm inp C.Source <> bs
        return (programOfBinds inpTy bs' ret)


-- | Convert a Query to Core
-- This takes the name of the input stream, the element types of the input,
-- and the query to transform.
-- It returns a list of program bindings, as well as the name of the binding
-- that is being "returned" in the program - essentially the last added binding.
convertQuery
        :: Ord n
        => Query (a,UniverseType) n
        -> ConvertM a n (CoreBinds n, Name n)
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

            let bs'  = strm n' (C.STrans (C.SFilter inpty) (CE.XLam nv inpty e') inpstream) <> bs

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
    (Latest (_,_retty) i : _)
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

            let tV'  = baseType $ typeFold res

            (inpstream, inpty) <- convertInput

            -- Because Map_insertOrUpdate and Array_fold take their "k" in a different order,
            -- we need to flip the k here.
            let k'    = CE.XLam n'a tV'
                      $ CE.XLam n'v inpty
                      $ beta
                      ( foldKons res CE.@~ CE.XVar n'a)

            -- Construct the "latest" reduction,
            -- the fold over the resulting array,
            -- and the extraction of the actual result of the array.
            -- (See convertFold)
            let barr  = red  n'arr   (C.RLatest inpty (CE.constI i) inpstream)
            let bfold = post n'fold  (CE.XPrim (C.PrimFold (C.PrimFoldArray inpty) tV')
                                      CE.@~ k' CE.@~ beta (foldZero res) CE.@~ CE.XVar n'arr)
            let bxtra = post n'xtra (beta (mapExtract res CE.@~ CE.XVar n'fold))

            return (barr <> bfold <> bxtra, n'xtra)

    -- Convert a group by into the construction of a Map.
    -- The key is the "by" of the group, while the value is the result of the
    -- aggregate over the partitioned results.
    --
    -- The result of a group by must be an aggregate, which means we have a
    -- relatively good idea of an upper bound on memory here,
    -- as long as the "by" is something like an enum, and the "value" has bounded memory.
    --
    (GroupBy (ann,retty) e : _)
     -> do  (t1,_) <- getGroupByMapType ann retty
            n'      <- lift fresh
            n''     <- lift fresh
            nmap    <- lift fresh
            nval    <- lift fresh

            -- Convert the rest of the query into a fold.
            -- We have the "k"onstructor, the "z"ero, and the e"x"tract,
            -- as well as the intermediate result type before extraction.
            -- See convertFold.
            res
                    <- convertWithInputName nval $ convertFold q'

            let tV'  = baseTypeOrOption $ typeFold res
            let t1'  = baseTypeOrOption ((snd $ annotOfExp e) { baseType = t1 })
            let t2'  = baseTypeOrOption $ typeExtract res

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
                    = CE.makeApps (CE.XPrim $ C.PrimMap $ priminsert)
                    [ foldKons res
                    , foldKons res `CE.XApp` foldZero res
                    , e'
                    , CE.XVar nmap]

            let insertOrUpdate'
                    = beta
                    $ CE.XLam nmap mapt
                    $ CE.XLam nval inpty
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
                    = CE.XPrim (C.PrimMap $ C.PrimMapMapValues t1' tV' t2')
                    CE.@~ beta (mapExtract res)
                    CE.@~ CE.XVar n'

            let traversed
                    | Possibly <- universePossibility $ universe retty
                    = CE.XPrim (C.PrimTraverse $ C.PrimTraverseByType $ T.MapT t1' t2') CE.@~ mapResult
                    | otherwise
                    = mapResult

            let p   = post n'' traversed

            return (r <> p, n'')

    -- Distinct is very similar to a group by, except instead of performing the fold
    -- as the elements are seen into the map,
    -- we insert/update the element in the map, then fold over essentially the last-seen
    -- for each group.
    (Distinct (_,_) e : _)
     -> do  let tkey = baseTypeOrOption $ snd $ annotOfExp e
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

            let tV'  = baseTypeOrOption $ typeFold res

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertWithInputName nval $ convertExp e

            let mapt = T.MapT tkey tval

            -- This is a little bit silly - 
            -- this "insertOrUpdate" should really just be an insert.
            -- Just put each element in the map, potentially overwriting the last one.
            let insertOrUpdate
                  = beta
                  $ CE.XLam nmap mapt
                  $ CE.XLam nval inpty
                  ( CE.XPrim (C.PrimMap $ C.PrimMapInsertOrUpdate tkey tval)
                    CE.@~ (CE.XLam n'ignore inpty $ CE.XVar nval)
                    CE.@~ (CE.XVar nval)
                    CE.@~  e'
                    CE.@~ CE.XVar nmap )

            -- Perform the map fold
            let r = red n' 
                  $ C.RFold inpty mapt insertOrUpdate 
                  ( CE.emptyMap tkey tval)
                    inpstream

            -- Perform a fold over that map
            let p = post n''
                  $ beta
                  ( mapExtract res CE.@~ 
                  ( CE.XPrim
                        (C.PrimFold (C.PrimFoldMap tkey tval) tV')
                    CE.@~ (CE.XLam nacc     tV'
                         $ CE.XLam n'ignore tkey
                         $ CE.XLam nval     tval
                         ( foldKons res CE.@~ CE.XVar nacc ))
                    CE.@~ foldZero res
                    CE.@~ CE.XVar n'))

            return (r <> p, n'')

    (Let _ b def : _)
     -> case universeTemporality $ universe $ snd $ annotOfExp def of
         Elem
          -> do let t'  = baseTypeOrOption $ snd $ annotOfExp def
                (inpstream, inpty) <- convertInput
                let inpty' = T.PairT t' inpty

                n'e     <- lift fresh
                
                e'      <- convertWithInputName n'e $ convertExp def

                let xfst = CE.XApp
                         $ CE.XPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t' inpty
                let xsnd = CE.XApp
                         $ CE.XPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t' inpty

                convertModifyFeatures (Map.insert b (t', xfst) . Map.map (\(t,f) -> (t, f . xsnd)))

                let pair = (CE.XPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair t' inpty)
                         CE.@~ e' CE.@~ CE.XVar n'e


                n'r     <- lift fresh
                let bs   = strm n'r
                         $ C.STrans (C.SMap inpty inpty') (CE.XLam n'e inpty pair) inpstream
                (bs', n'') <- convertWithInput n'r inpty' $ convertQuery q'

                return (bs <> bs', n'')

         Pure
          -> do e'      <- convertExp def
                convertModifyFeatures (Map.delete b)
                b'      <- convertFreshenAdd b
                let bs   = pre b' e'
                (bs', n') <- convertQuery q'
                return (bs <> bs', n')

         AggU
          -> do (bs,n')      <- convertReduce    def
                b'      <- convertFreshenAdd b
                (bs',n'')    <- convertQuery     q'
                return (bs <> post b' (CE.XVar n') <> bs', n'')

         Group _
          -> do (bs,n')      <- convertReduce    def
                b'      <- convertFreshenAdd b
                (bs',n'')    <- convertQuery     q'
                return (bs <> post b' (CE.XVar n') <> bs', n'')

    -- Converting fold1s.
    --
    -- Converting fold1s is complicated by the fact that the worker functions
    -- can return "possiblies" as well.
    -- For example, a division in the worker function:
    --
    -- > let fold1 rolling ~> value : rolling / value ~> ...
    --
    -- if "value" is zero at any point, the result should be "None"
    -- because of the division by zero.
    --
    -- Because fold1 already has an inherent possibly, we need to deal with
    -- three different cases.
    -- The obvious way to write this is:
    --
    -- > let fold1 test = zero value : kons value test
    -- >
    -- > ====>
    -- >
    -- > data Result
    -- > = WorkerError
    -- > | NoData
    -- > | Ok Value
    -- >
    -- > Stream.fold
    -- >  (\a : Result. i : Value.
    -- >      case a of
    -- >       WorkerError
    -- >        -> WorkerError
    -- >       NoData
    -- >        -> zero i
    -- >       Ok v
    -- >        -> kons i v)
    -- >  NoData
    -- >  stream
    --
    -- However the "zero" and "kons" return a "Option Value" instead of "Result",
    -- so we need to add case statements there.
    --
    -- > Stream.fold
    -- >  (\a : Result. i : Value.
    -- >      case a of
    -- >       WorkerError
    -- >        -> WorkerError
    -- >       NoData
    -- >        -> case zero i of
    -- >            None   -> WorkerError
    -- >            Some z -> Ok z
    -- >       Ok v
    -- >        -> case kons i v of
    -- >            None   -> WorkerError
    -- >            Some k -> Ok k)
    -- >  NoData
    -- >  stream
    --
    -- The next thing is that Core does not have arbitrary data types.
    -- But we have Options, so we can use an isomorphic type:
    --
    -- > type Result' = Option (Option Value)
    -- > WorkerError = None
    -- > NoData      = Some None
    -- > Ok v        = Some (Some v)
    -- >
    -- > Stream.fold
    -- >  (\a : Result'. i : Value.
    -- >      case a of
    -- >       None
    -- >        -> None
    -- >       Some a'
    -- >        -> case a' of
    -- >            None
    -- >             -> case zero i of
    -- >                 None   -> None
    -- >                 Some z -> Some (Some z)
    -- >            Some a''
    -- >             -> case kons i a'' of
    -- >                 None   -> None
    -- >                 Some k -> Some (Some k))
    -- >  (Some None)
    -- >  stream
    --
    -- Folds in Core have the "Some" case before the "None" case,
    -- so just reorder the cases.
    -- Trivial, but simplifies the derivation.
    --
    -- > Stream.fold
    -- >  (\a : Result'. i : Value.
    -- >      case a of
    -- >       Some a'
    -- >        -> case a' of
    -- >            Some a''
    -- >             -> case kons i a'' of
    -- >                 Some k -> Some (Some k)
    -- >                 None   -> None
    -- >            None
    -- >             -> case zero i of
    -- >                 Some z -> Some (Some z)
    -- >                 None   -> None
    -- >       None
    -- >        -> None)
    -- >  (Some None)
    -- >  stream
    --
    -- Finally, Core doesn't have case expressions, but does have
    -- folds over Options.
    -- We can rewrite it using folds:
    --
    -- > Stream.fold
    -- >  (\a : Result'. i : Value.
    -- >      Option.fold (\a' : Option Int.
    -- >         Option.fold (\a'' : Int.
    -- >            Option.fold (\k : Int. Some (Some k))
    -- >              None
    -- >              (kons i a''))
    -- >           (Option.fold (\z : Int. Some (Some k))
    -- >              None
    -- >              (zero i))
    -- >            a')
    -- >       None
    -- >       a)
    -- >  (Some None)
    -- >  stream
    --
    -- If one of @zero@ or @kons@ do not return possiblies, we
    -- can just replace the folds over them with "Some (Some (zero i))" etc.
    --
    (LetFold (_,retty) f@Fold{ foldType = FoldTypeFoldl1 } : _)
     -> do  -- Type helpers
            let tU = baseType retty
            let tO = T.OptionT tU
            let tOO= T.OptionT tO

            -- Generate fresh names
            -- Element of the stream
            -- :                input type
            n'elem <- lift fresh
            -- Current accumulator
            -- : Option (Option tU)
            n'a     <- lift fresh
            -- Unwrap accumulator by 1
            -- :         Option tU
            n'a'    <- lift fresh
            -- Fully unwrapped accumulator
            -- :                tU
            n'a'' <- convertFreshenAdd $ foldBind f

            -- result of a worker after unwrapping
            -- :                tU
            n'worker <- lift fresh


            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            convertModifyFeatures (Map.delete (foldBind f))
            z   <- convertWithInputName n'elem $ convertExp (foldInit f)
            k   <- convertWithInputName n'elem $ convertExp (foldWork f)

            -- (Some.Some)
            let somedotsome
                 = CE.XLam n'worker tU
                 $ som tO
                 $ som tU
                 $ CE.XVar n'worker

            -- Rewrap zero and kons if it's possibly,
            -- if not wrap in two Somes
            let z' | Possibly <- universePossibility $ universe $ snd $ annotOfExp $ foldInit f
                   = opt tU tOO somedotsome (none tO) z
                   | otherwise
                   = som tO $ som tU $ z

            let k' | Possibly <- universePossibility $ universe $ snd $ annotOfExp $ foldWork f
                   = opt tU tOO somedotsome (none tO) k
                   | otherwise
                   = som tO $ som tU $ k


            (inpstream, inpty) <- convertInput

            -- Worker function of the fold
            let go  = CE.XLam n'a tOO
                    ( CE.XLam n'elem inpty
                    $ opt tO tOO
                        (CE.XLam n'a' tO
                        $ opt tU tOO
                            ( CE.XLam n'a'' tU k')
                              z'
                          (CE.XVar n'a'))
                      (none tO)
                      (CE.XVar n'a))


            -- Bind the fold to a fresh name
            n' <- lift fresh
            let bs = red n'
                        (C.RFold inpty tOO go (som tO $ none tU) inpstream)
                     -- Then unwrap the actual result and bind it
                  <> post n'a''
                        (opt tO tO
                            -- If the outer layer is a "Some",
                            -- just return inner layer as-is
                            (CE.XLam n'a' tO $ CE.XVar n'a')
                            -- Outer layer is a "None", so return "None".
                            (none tU)
                            (CE.XVar n'))

            
            (bs', n'')      <- convertQuery q'

            return (bs <> bs', n'')


    -- In comparison, normal folds are quite easy.
    -- If the worker functions are not "possiblies":
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
    (LetFold (_,retty) f@Fold{ foldType = FoldTypeFoldl } : _)
     | Definitely <- universePossibility $ universe $ snd $ annotOfExp $ foldInit f
     , Definitely <- universePossibility $ universe $ snd $ annotOfExp $ foldWork f
     -> do  -- Type helpers
            let tU = baseType retty

            -- Generate fresh names
            -- Element of the stream
            -- :                input type
            n'elem <- lift fresh
            -- Accumulator
            -- :                tU
            n'a <- convertFreshenAdd $ foldBind f

            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            convertModifyFeatures (Map.delete (foldBind f))
            z   <- convertWithInputName n'elem $ convertExp (foldInit f)
            k   <- convertWithInputName n'elem $ convertExp (foldWork f)

            (inpstream, inpty) <- convertInput
            -- Worker function of the fold
            let go  = CE.XLam n'a tU
                    $ CE.XLam n'elem inpty k

            -- Bind the fold to the original name
            let bs = red n'a (C.RFold inpty tU go z inpstream)
            
            (bs', n'')      <- convertQuery q'

            return (bs <> bs', n'')


    -- If either of the worker functions *are* "possiblies":
    --
    -- > let fold summ ~> 0 : summ + value / 2 ~> ...
    -- >
    -- > =====>
    -- >
    -- > Stream.fold
    -- > (\a : Option Int. \v : Value.
    -- >   Option.fold a
    -- >    (\a' : Int. (a' + v) / 2)
    -- >    None)
    -- > (Some 0)
    -- > stream
    --
    --(LetFold (_,retty) f@Fold{ foldType = FoldTypeFoldl } : _)
    -- | Possibly <- universePossibility $ universe retty
     | otherwise
     -> do  -- Type helpers
            let tU = baseType retty
            let tO = T.OptionT tU

            -- Generate fresh names
            -- Element of the stream
            -- :                input type
            n'elem <- lift fresh
            -- Current accumulator
            -- :         Option tU
            n'a     <- lift fresh
            -- Fully unwrapped accumulator
            -- :                tU
            n'a' <- convertFreshenAdd $ foldBind f

            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            convertModifyFeatures (Map.delete (foldBind f))
            z   <- convertWithInputName n'elem $ convertExp (foldInit f)
            k   <- convertWithInputName n'elem $ convertExp (foldWork f)

            -- If zero and cons are possiblies, leave them alone.
            -- If not wrap in just one Some
            let z' | Possibly <- universePossibility $ universe $ snd $ annotOfExp $ foldInit f
                   = z
                   | otherwise
                   = som tU $ z

            let k' | Possibly <- universePossibility $ universe $ snd $ annotOfExp $ foldWork f
                   = k
                   | otherwise
                   = som tU $ k


            (inpstream, inpty) <- convertInput
            -- Worker function of the fold
            let go  = CE.XLam n'a tO
                    ( CE.XLam n'elem inpty
                    $ opt tU tO
                        (CE.XLam n'a' tU k')
                        (none tU)
                        (CE.XVar n'a))


            -- Bind the fold to the original name
            let bs = red n'a' (C.RFold inpty tO go z' inpstream)
            
            (bs', n'')      <- convertQuery q'

            return (bs <> bs', n'')



 where
  -- The remaining query after the current context is removed
  q' = q { contexts = drop 1 $ contexts q }

  -- Group bys can live in either Aggregate or Group universe.
  -- Because Core is explicitly typed, we need to pull out the key type and value type.
  getGroupByMapType ann ty
   | UniverseType (Universe (Group t1) _) t2         <- ty
   = return (t1, t2)
   | UniverseType (Universe AggU _) (T.MapT t1 t2)   <- ty
   = return (t1, t2)
   | otherwise
   = convertError $ ConvertErrorGroupByHasNonGroupResult ann ty

  -- Perform beta reduction, just to simplify the output a tiny bit.
  beta = Beta.betaToLets
       . Beta.beta Beta.isSimpleValue

  -- Some helpers for generating Option expressions
  -- Note that the "t"s here are the types without the outermost
  -- layer of Option wrapping
  opt t tret ss nn scrutinee
   = CE.XPrim (C.PrimFold (C.PrimFoldOption t) tret)
     CE.@~ ss CE.@~ nn CE.@~ scrutinee
  none t
   = CE.XValue (T.OptionT t) VNone
  som t
   = CE.some t


-- | Convert an Aggregate computation at the end of a query.
-- This must be an aggregate, some primitive applied to at least one aggregate expression,
-- or a nested query.
convertReduce
        :: Ord n
        => Exp (a,UniverseType) n
        -> ConvertM a n (CoreBinds n, Name n)
convertReduce xx
 | Just (p, (_,ty), args) <- takePrimApps xx
 = case p of
    Agg Count
     | [] <- args
     -- Count: just add 1, ignoring the value
     -> do  na <- lift fresh
            nv <- lift fresh
            mkFold T.IntT (CE.XVar na CE.+~ CE.constI 1) (CE.constI 0) na nv
     | otherwise
     -> errAggBadArgs

    Agg SumA
     | [x] <- args
     , Definitely <- universePossibility $ universe ty
     -> do  na <- lift fresh
            nv <- lift fresh
            -- Convert the element expression and sum over it
            x' <- convertWithInputName nv $ convertExp x
            mkFold T.IntT (CE.XVar na CE.+~ x') (CE.constI 0) na nv
     | [x] <- args
     , Possibly <- universePossibility $ universe ty
     -> do  nv <- lift fresh
            -- Convert the element expression and sum over it
            x' <- convertWithInputName nv $ convertExp x

            let plusX = CE.XPrim (C.PrimMinimal $ Min.PrimArith Min.PrimArithPlus)
            let plusT = UniverseType (definitely $ universe ty) T.IntT
            let argT  = UniverseType (possibly $ universe ty) T.IntT
            na  <- lift fresh
            fun <- applyPossibles plusX plusT
                            [(CE.XVar na, argT), (x', argT)]

            mkFold (T.OptionT T.IntT) fun
                   (CE.some T.IntT $ CE.constI 0) na nv

     | otherwise
     -> errAggBadArgs

    -- Find the newest / most recent.
    -- This just means the "last seen" one.
    Agg Newest
     | [x] <- args
     -> do  na <- lift fresh
            nv <- lift fresh
            -- Convert the element expression
            x' <- convertWithInputName nv $ convertExp x

            let argT = snd $ annotOfExp x
            let retty= T.OptionT $ baseType argT

            -- Start with Nothing
            let seed = CE.XValue retty VNone

            -- If the expression is already a "possibly",
            -- then its conversion already has type Option.
            -- So we can just return it unchanged.
            let fun
                 | Possibly <- universePossibility $ universe argT
                 = x'
                 | otherwise
                 -- Otherwise, wrap in a Some constructor
                 = CE.some (baseType argT) x'

            mkFold retty fun seed na nv

     | otherwise
     -> errAggBadArgs

    -- Find the oldest, or first seen value.
    Agg Oldest
     | [x] <- args
     -> do  na <- lift fresh
            nv <- lift fresh
            -- Convert the element expression
            x' <- convertWithInputName nv $ convertExp x

            let argT = snd $ annotOfExp x

            -- If x' is a possibly, we actually want to carry around two levels
            -- of options: that way we won't overwrite the Some Nothing, meaning
            -- the first element has been seen but was Nothing.
            let valty= baseTypeOrOption argT
            let retty= T.OptionT $ valty

            let seed = CE.XValue retty VNone
            let opt  = C.PrimFold (C.PrimFoldOption valty) retty

            na' <- lift fresh
            let return_acc
                     = CE.XLam na' valty (CE.XVar na)
            let some_arg
                     = CE.some valty x'

            let fun  = CE.XPrim opt
                       CE.@~ return_acc
                       CE.@~ some_arg
                       CE.@~ CE.XVar na

            (bs, n') <- mkFold retty fun seed na nv

            n'' <- lift fresh
            let flat
                     | Possibly <- universePossibility $ universe argT
                     = CE.XPrim (C.PrimFold (C.PrimFoldOption valty) valty)
                       CE.@~ (CE.XLam na' valty (CE.XVar na'))
                       CE.@~ (CE.XValue valty VNone)
                       CE.@~ (CE.XVar n')
                     | otherwise
                     = CE.XVar n'

            return (bs <> post n'' flat, n'')


     | otherwise
     -> errAggBadArgs

    -- For any other primitives:
    --   recurse into its arguments and get bindings for them
    --   apply those bindings to the primitive, as a postcomputation
    --
    -- If the binding is Pure however, it must not rely on any aggregates,
    -- so it might as well be a precomputation.
    _
     -> do  (bs,nms) <- unzip <$> mapM convertReduce args
            let tys  = fmap (snd . annotOfExp) args
            let xs   = fmap  CE.XVar           nms
            x' <- convertPrim p (fst $ annotOfExp xx) ty (xs `zip` tys)

            nm  <- lift fresh

            let bs'  = mconcat bs
            let b''  | Pure <- universeTemporality $ universe ty
                     = pre nm x'
                     | otherwise
                     = post nm x'

            return (bs' <> b'', nm)


 -- Convert a nested query
 | Nested _ q   <- xx
 = convertQuery q
 -- Any variable must be a let-bound aggregate, so we can safely assume it has a binding.
 | Var (ann,_) v      <- xx
 = (,) mempty <$> convertFreshenLookup ann v

 -- It's not a variable or a nested query,
 -- so it must be an application of a non-primitive
 | otherwise
 = convertError $ ConvertErrorExpApplicationOfNonPrimitive (fst $ annotOfExp xx) xx

 where
  -- Helper for creating a stream fold binding
  mkFold ta k z na nv
   = do n' <- lift fresh
        (inpstream, inpty) <- convertInput
        let k' = CE.XLam na ta
               $ CE.XLam nv inpty
               $ k
        return (red n' $ C.RFold inpty ta k' z inpstream, n')

  -- Bad arguments to an aggregate
  errAggBadArgs
   = convertError
   $ ConvertErrorReduceAggregateBadArguments (fst $ annotOfExp xx) xx

