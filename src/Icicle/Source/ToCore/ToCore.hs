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

import                  Control.Monad.Trans.Class
import                  Data.List (zip, unzip, unzip4)

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
        -> ConvertM a n (C.Program n)
convertQueryTop feats qt
 = do   inp <- fresh
        (ty,fs) <- lift
                 $ maybeToRight (ConvertErrorNoSuchFeature (feature qt))
                 $ Map.lookup (feature qt) feats

        let inpTy       = ty
        let inpTy'dated = T.PairT inpTy T.DateTimeT

        (bs,ret) <- convertQuery fs inp inpTy'dated (query qt)
        let bs'   = strm inp C.Source <> bs
        return (programOfBinds inpTy bs' ret)


-- | Convert a Query to Core
-- This takes the name of the input stream, the element types of the input,
-- and the query to transform.
-- It returns a list of program bindings, as well as the name of the binding
-- that is being "returned" in the program - essentially the last added binding.
convertQuery
        :: Ord n
        => FeatureContext n
        -> Name n -> T.ValType
        -> Query (a,UniverseType) n
        -> ConvertM a n (CoreBinds n, Name n)
convertQuery fs n nt q
 = case contexts q of
    -- There are no queries left, so deal with simple aggregates and nested queries.
    []
     -> convertReduce fs n nt (final q)

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
     -> do  n'      <- fresh
            nv      <- fresh
            e'      <- convertExp   fs nv nt e

            (bs, b) <- convertQuery fs n' nt q'
            let bs'  = strm n' (C.STrans (C.SFilter nt) (CE.XLam nv nt e') n) <> bs

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
     -> do  n'      <- fresh
            (bs, b) <- convertQuery fs n' nt q'

            let newerThan' =      convertWindowUnits newerThan
            let olderThan' = fmap convertWindowUnits olderThan

            let bs'  = strm n' (C.SWindow nt newerThan' olderThan' n) <> bs
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
     -> do  n'arr   <- fresh
            n'fold  <- fresh
            n'xtra  <- fresh

            n'a     <- fresh
            n'v     <- fresh

            -- Destruct the aggregate into a fold expression:
            -- x (fold k z inps)
            -- where x :: tV -> retty
            (k,z,x,tV)
                    <- convertGroupBy fs     n'v nt q'

            let tV'  = baseType tV

            -- Because Map_insertOrUpdate and Array_fold take their "k" in a different order,
            -- we need to flip the k here.
            let k'    = CE.XLam n'a tV'
                      $ CE.XLam n'v nt
                      $ beta
                      ( k CE.@~ CE.XVar n'a)

            -- Construct the "latest" reduction,
            -- the fold over the resulting array,
            -- and the extraction of the actual result of the array.
            -- (See convertGroupBy)
            let barr  = red  n'arr   (C.RLatest nt (CE.constI i) n)
            let bfold = post n'fold  (CE.XPrim (C.PrimFold (C.PrimFoldArray nt) tV')
                                      CE.@~ k' CE.@~ beta z CE.@~ CE.XVar n'arr)
            let bxtra = post n'xtra (beta (x CE.@~ CE.XVar n'fold))

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
     -> do  (t1,t2) <- getGroupByMapType ann retty
            n'      <- fresh
            n''     <- fresh
            nmap    <- fresh
            nval    <- fresh

            -- Convert the rest of the query into a fold.
            -- We have the "k"onstructor, the "z"ero, and the e"x"tract,
            -- as well as the intermediate result type before extraction.
            -- See convertGroupBy.
            (k,z,x,tV)
                    <- convertGroupBy  fs  nval nt q'

            let tV'  = baseType tV

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertExp      fs  nval nt e

            let mapt = T.MapT t1 tV'

            -- For each input element, we use the group by as the key, and insert into a map.
            --
            -- If the map already has the key, we perform the "k" on the current element
            -- and the existing value - the fold accumulator.
            --
            -- If the map doesn't have the key, we insert the "k" of the current element
            -- with the zero accumulator.
            -- This is because the map doesn't start with "zero"s in it, unlike a normal fold.
            let insertOrUpdate
                  = beta
                  $ CE.XLam nmap mapt
                  $ CE.XLam nval nt
                  ( CE.XPrim (C.PrimMap $ C.PrimMapInsertOrUpdate t1 tV')
                    CE.@~  k
                    CE.@~ (k CE.@~ z)
                    CE.@~  e'
                    CE.@~ CE.XVar nmap )

            -- Perform the map fold
            let r = red n' 
                  $ C.RFold nt mapt insertOrUpdate 
                  ( CE.emptyMap t1 tV')
                    n

            -- After all the elements have been seen, we go through the map and perform
            -- the "extract" on each value.
            -- This performs any fixups that couldn't be performed during the fold.
            let p = post n''
                  ( CE.XPrim
                        (C.PrimMap $ C.PrimMapMapValues t1 tV' t2)
                    CE.@~ beta x CE.@~ CE.XVar n' )

            return (r <> p, n'')

    -- Distinct is very similar to a group by, except instead of performing the fold
    -- as the elements are seen into the map,
    -- we insert/update the element in the map, then fold over essentially the last-seen
    -- for each group.
    (Distinct (_,_) e : _)
     -> do  let tkey = baseType $ snd $ annotOfExp e
            let tval = nt

            n'      <- fresh
            n''     <- fresh
            nmap    <- fresh
            nacc    <- fresh
            nval    <- fresh
            n'ignore<- fresh

            -- Convert the rest of the query into a fold.
            -- This is executed as a Map fold at the end, rather than
            -- as a stream fold.
            (k,z,x,tV)
                    <- convertGroupBy   fs nval nt q'

            let tV'  = baseType tV

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertExp       fs nval nt e

            let mapt = T.MapT tkey tval

            -- This is a little bit silly - 
            -- this "insertOrUpdate" should really just be an insert.
            -- Just put each element in the map, potentially overwriting the last one.
            let insertOrUpdate
                  = beta
                  $ CE.XLam nmap mapt
                  $ CE.XLam nval nt
                  ( CE.XPrim (C.PrimMap $ C.PrimMapInsertOrUpdate tkey tval)
                    CE.@~ (CE.XLam n'ignore nt $ CE.XVar nval)
                    CE.@~ (CE.XVar nval)
                    CE.@~  e'
                    CE.@~ CE.XVar nmap )

            -- Perform the map fold
            let r = red n' 
                  $ C.RFold nt mapt insertOrUpdate 
                  ( CE.emptyMap tkey tval)
                    n

            -- Perform a fold over that map
            let p = post n''
                  $ beta
                  ( x CE.@~ 
                  ( CE.XPrim
                        (C.PrimFold (C.PrimFoldMap tkey tval) tV')
                    CE.@~ (CE.XLam nacc     tV'
                         $ CE.XLam n'ignore tkey
                         $ CE.XLam nval     tval
                         ( k CE.@~ CE.XVar nacc ))
                    CE.@~ z
                    CE.@~ CE.XVar n'))

            return (r <> p, n'')

    (Let _ b def : _)
     -> case universeTemporality $ universe $ snd $ annotOfExp def of
         Elem
          -> do let t'  = baseTypeOrOption $ snd $ annotOfExp def
                let nt' = T.PairT t' nt

                n'e     <- fresh
                
                e'      <- convertExp       fs n'e nt def

                let xfst = CE.XApp
                         $ CE.XPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t' nt
                let xsnd = CE.XApp
                         $ CE.XPrim $ C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t' nt

                let fs'  = Map.insert b (t', xfst)
                         $ Map.map (\(t,f) -> (t, f . xsnd)) fs

                let pair = (CE.XPrim $ C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair t' nt)
                         CE.@~ e' CE.@~ CE.XVar n'e


                n'r     <- fresh
                let bs   = strm n'r (C.STrans (C.SMap nt nt') (CE.XLam n'e nt pair) n)
                (bs', n'') <- convertQuery fs' n'r nt' q'
                return (bs <> bs', n'')

         Pure
          -> do e'      <- convertExp       fs n nt def
                let bs   = pre (Name b) e'
                (bs', n') <- convertQuery fs n nt q'
                return (bs <> bs', n')

         AggU
          -> do (bs,n')      <- convertReduce    fs n nt def
                (bs',n'')    <- convertQuery     fs n nt q'
                return (bs <> post (Name b) (CE.XVar n') <> bs', n'')

         Group _
          -> do (bs,n')      <- convertReduce    fs n nt def
                (bs',n'')    <- convertQuery     fs n nt q'
                return (bs <> post (Name b) (CE.XVar n') <> bs', n'')

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
            -- :                nt
            n'elem <- fresh
            -- Current accumulator
            -- : Option (Option tU)
            n'a     <- fresh
            -- Unwrap accumulator by 1
            -- :         Option tU
            n'a'    <- fresh
            -- Fully unwrapped accumulator
            -- :                tU
            let n'a'' = Name (foldBind f)

            -- result of a worker after unwrapping
            -- :                tU
            n'worker <- fresh


            -- Remove binding before converting init and work expressions,
            -- just in case the same name has been used elsewhere
            let fs' = Map.delete (foldBind f) fs
            z   <- convertExp fs' n'elem nt (foldInit f)
            k   <- convertExp fs' n'elem nt (foldWork f)

            -- Some helpers for generating Option expressions
            -- Note that the "t"s here are the types without the outermost
            -- layer of Option wrapping
            let opt t tret som non scrutinee
                    = CE.XPrim (C.PrimFold (C.PrimFoldOption t) tret)
                    CE.@~ som CE.@~ non CE.@~ scrutinee
            let none t = CE.XValue (T.OptionT t) VNone
            let som t = CE.some t

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


            -- Worker function of the fold
            let go  = CE.XLam n'a tOO
                    ( CE.XLam n'elem nt
                    $ opt tO tOO
                        (CE.XLam n'a' tO
                        $ opt tU tOO
                            ( CE.XLam n'a'' tU k')
                              z'
                          (CE.XVar n'a'))
                      (none tO)
                      (CE.XVar n'a))


            -- Bind the fold to a fresh name
            n' <- fresh
            let bs = red n'
                        (C.RFold nt tOO go (som tO $ none tU) n)
                     -- Then unwrap the actual result and bind it
                  <> post (Name $ foldBind f)
                        (opt tO tO
                            -- If the outer layer is a "Some",
                            -- just return inner layer as-is
                            (CE.XLam n'a' tO $ CE.XVar n'a')
                            -- Outer layer is a "None", so return "None".
                            (none tU)
                            (CE.XVar n'))

            
            (bs', n'')      <- convertQuery fs' n nt q'

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
   = lift $ Left $ ConvertErrorGroupByHasNonGroupResult ann ty

  -- Perform beta reduction, just to simplify the output a tiny bit.
  beta = Beta.betaToLets
       . Beta.beta Beta.isSimpleValue


-- | Convert an Aggregate computation at the end of a query.
-- This must be an aggregate, some primitive applied to at least one aggregate expression,
-- or a nested query.
convertReduce
        :: Ord n
        => FeatureContext n
        -> Name n   -> T.ValType
        -> Exp (a,UniverseType) n
        -> ConvertM a n (CoreBinds n, Name n)
convertReduce fs n t xx
 | Just (p, (_,ty), args) <- takePrimApps xx
 = case p of
    Agg Count
     | [] <- args
     -- Count: just add 1, ignoring the value
     -> do  na <- fresh
            nv <- fresh
            mkFold T.IntT (CE.XVar na CE.+~ CE.constI 1) (CE.constI 0) na nv
     | otherwise
     -> errAggBadArgs

    Agg SumA
     | [x] <- args
     , Definitely <- universePossibility $ universe ty
     -> do  na <- fresh
            nv <- fresh
            -- Convert the element expression and sum over it
            x' <- convertExp fs nv t x
            mkFold T.IntT (CE.XVar na CE.+~ x') (CE.constI 0) na nv
     | [x] <- args
     , Possibly <- universePossibility $ universe ty
     -> do  nv <- fresh
            -- Convert the element expression and sum over it
            x' <- convertExp fs nv t x

            let plusX = CE.XPrim (C.PrimMinimal $ Min.PrimArith Min.PrimArithPlus)
            let plusT = UniverseType (definitely $ universe ty) T.IntT
            let argT  = UniverseType (possibly $ universe ty) T.IntT
            na  <- fresh
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
     -> do  na <- fresh
            nv <- fresh
            -- Convert the element expression
            x' <- convertExp fs nv t x

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
     -> do  na <- fresh
            nv <- fresh
            -- Convert the element expression
            x' <- convertExp fs nv t x

            let argT = snd $ annotOfExp x

            -- If x' is a possibly, we actually want to carry around two levels
            -- of options: that way we won't overwrite the Some Nothing, meaning
            -- the first element has been seen but was Nothing.
            let valty= baseTypeOrOption argT
            let retty= T.OptionT $ valty

            let seed = CE.XValue retty VNone
            let opt  = C.PrimFold (C.PrimFoldOption valty) retty

            na' <- fresh
            let return_acc
                     = CE.XLam na' valty (CE.XVar na)
            let some_arg
                     = CE.some valty x'

            let fun  = CE.XPrim opt
                       CE.@~ return_acc
                       CE.@~ some_arg
                       CE.@~ CE.XVar na

            (bs, n') <- mkFold retty fun seed na nv

            n'' <- fresh
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
     -> do  (bs,nms) <- unzip <$> mapM (convertReduce fs n t) args
            let tys  = fmap (snd . annotOfExp) args
            let xs   = fmap  CE.XVar           nms
            x' <- convertPrim p (fst $ annotOfExp xx) ty (xs `zip` tys)

            nm  <- fresh

            let bs'  = mconcat bs
            let b''  | Pure <- universeTemporality $ universe ty
                     = pre nm x'
                     | otherwise
                     = post nm x'

            return (bs' <> b'', nm)


 -- Convert a nested query
 | Nested _ q   <- xx
 = convertQuery fs n t q
 -- Any variable must be a let-bound aggregate, so we can safely assume it has a binding.
 | Var _ v      <- xx
 = return (mempty, Name v)
 -- TODO: actually this should never happen, except for a non-primitive application
 | otherwise
 = lift $ Left $ ConvertErrorTODO (fst $ annotOfExp xx) "convertReduce"

 where
  -- Helper for creating a stream fold binding
  mkFold ta k z na nv
   = do n' <- fresh
        let k' = CE.XLam na ta
               $ CE.XLam nv t
               $ k
        return (red n' $ C.RFold t ta k' z n, n')

  -- Bad arguments to an aggregate
  errAggBadArgs
   = lift
   $ Left
   $ ConvertErrorReduceAggregateBadArguments (fst $ annotOfExp xx) xx


-- | Convert an element-level expression.
-- These are worker functions for folds, filters and so on.
convertExp
        :: Ord n
        => FeatureContext n
        -> Name n   -> T.ValType
        -> Exp (a,UniverseType) n
        -> ConvertM a n (C.Exp n)
convertExp fs nElem t x
 | Var _ v <- x
 , Just (_, x') <- Map.lookup v fs
 = return $ x' $ CE.XVar nElem

 -- Primitive application: convert arguments, then convert primitive
 | Just (p, (ann,retty), args) <- takePrimApps x
 = do   args'   <- mapM (convertExp fs nElem t) args
        let tys  = fmap (snd . annotOfExp) args
        convertPrim p ann retty (args' `zip` tys)

 -- A real nested query should not appear here.
 -- However, if it has no contexts, it's really just a nested expression.
 | Nested _ (Query [] x') <- x
 = convertExp fs nElem t x'

 | otherwise
 = case x of
    -- Variable must be bound as a precomputation
    Var _ n
     -> return $ CE.XVar $ Name n
    Nested (ann,_) q
     -> lift
      $ Left
      $ ConvertErrorExpNestedQueryNotAllowedHere ann q
    App (ann,_) _ _
     -> lift
      $ Left
      $ ConvertErrorExpApplicationOfNonPrimitive ann x
    Prim (ann,retty) p
     -> convertPrim p ann retty []


-- | Convert the body of a group by (or other query) into a fold:
--
-- The fold is described by:
--  Konstukt : a -> b -> a
--  Zero     : a
--  Xtract   : a -> c
--
-- The extract is used for any postprocessing that can only be done on the
-- final result of the accumulator.
--
-- So for example, a sum would be
--  K = (+)
--  Z = 0
--  X = id
--
-- but if we wanted mean, we would store a pair of values in the accumulator,
-- the sum and the count, and then the extract would divide the two:
--  K = (\(s,c) v   -> (s + v, c + 1))
--  Z =                (0, 0)
--  X = (\(s,c)     -> s / c)
-- 
-- 
-- Not all subqueries are supported: windowing, grouping and distincts are banned.
--
--
convertGroupBy
        :: Ord n
        => FeatureContext n
        -> Name n -> T.ValType
        -> Query (a,UniverseType) n
        -> ConvertM a n (C.Exp n, C.Exp n, C.Exp n, UniverseType)
convertGroupBy fs nElem t q
 = case contexts q of
    -- No contexts, just an expression
    []
     -- Nested query; recurse
     | Nested _ qq <- final q
     -> convertGroupBy fs nElem t qq

     -- Primitive application
     | Just (p, (ann,retty), args) <- takePrimApps $ final q
     -> case p of
         -- Aggregates are relatively simple
         Agg SumA
          | [e] <- args
          -> do let retty' = baseType retty
                e' <- convertExp fs nElem t e

                n  <- fresh
                let k = CE.XLam n retty'
                      ( CE.XVar n CE.+~ e' )
                let z = CE.constI 0
                x    <- idFun retty'

                return (k, z, x, retty)
          | otherwise
          -> errAggBadArgs

         Agg Count
          | [] <- args
          -> do let retty' = baseType retty

                n  <- fresh
                let k = CE.XLam n retty'
                      ( CE.XVar n CE.+~ CE.constI 1 )
                let z = CE.constI 0
                x    <- idFun retty'

                return (k, z, x, retty)
          | otherwise
          -> errAggBadArgs


         -- Non-aggregate primitive operations such as (+) or (/) are a bit more involved:
         -- we convert the arguments to folds,
         -- then store the accumulator as nested pairs of arguments
         -- then, for the extract we destruct the pairs and apply the operator normally.
         _
          -> do -- Convert all arguments
                -- (create a query out of the expression,
                --  just because there is no separate convertGroupX function)
                (ks, zs, xs, ts) <- unzip4 <$> mapM (convertGroupBy fs nElem t . Query []) args

                let ts' = fmap baseType ts
                -- Create pairs for zeros
                (zz, tt) <- pairConstruct zs ts'

                -- For extraction:
                --  destruct the pairs,
                --  recursively extract the arguments,
                --  apply the primitive
                let cp ns
                        = convertPrim p ann retty
                            ((fmap (uncurry CE.XApp) (xs `zip` ns)) `zip` ts)
                xx       <- pairDestruct cp ts' (baseType retty)

                -- For konstrukt, we need to destruct the pairs, apply the sub-ks,
                -- then box it up again in pairs.
                let applyKs ns = fst <$> pairConstruct (fmap (uncurry CE.XApp) (ks `zip` ns)) ts'
                kk       <- pairDestruct applyKs ts' tt

                return (kk, zz, xx, retty { baseType = tt })

     -- It must be a variable or a non-primitive application
     | otherwise
      -> errTODO $ annotOfExp $ final q

    -- For filter, you convert the subquery as normal,
    -- then only apply the subquery's "k" when the filter predicate is true.
    --
    -- Note that this has different "history semantics" to the normal filter.
    (Filter _ e : _)
     -> do  (k,z,x,tt) <- convertGroupBy fs nElem t q'
            e'         <- convertExp     fs nElem t e
            prev       <- fresh
            let tt'     = baseType tt
            let prev'   = CE.XVar prev
            let k' = CE.XLam prev tt'
                   ( CE.XPrim (C.PrimFold C.PrimFoldBool tt')
                     CE.@~ (k CE.@~ prev') CE.@~ prev' CE.@~ e' )
            return (k', z, x, tt)

    (Windowed (ann,_) _ _ : _)
     -> errNotAllowed ann
    (Latest (ann,_) _ : _)
     -> errNotAllowed ann
    (GroupBy (ann,_) _ : _)
     -> errNotAllowed ann
    (Distinct (ann,_) _ : _)
     -> errNotAllowed ann
    -- TODO: let and letfold should probably be allowed
    (Let (ann,_) _ _ : _)
     -> errNotAllowed ann
    (LetFold (ann,_) _ : _)
     -> errNotAllowed ann


 where
  q' = q { contexts = drop 1 $ contexts q }

  errNotAllowed ann
   = lift $ Left $ ConvertErrorContextNotAllowedInGroupBy ann q
  errTODO ann
   = lift $ Left $ ConvertErrorTODO (fst ann) "convertGroupBy"

  errAggBadArgs
   = lift
   $ Left
   $ ConvertErrorReduceAggregateBadArguments (fst $ annotOfExp $ final q) (final q)


  -- Construct an identity function
  idFun tt = fresh >>= \n -> return (CE.XLam n tt (CE.XVar n))

  -- Create nested pair type for storing the result of subexpressions
  pairTypes ts
   = foldr T.PairT T.UnitT ts

  -- Create nested pairs of arguments
  pairConstruct xs ts
   = return
   $ foldr
   (\(xa,ta) (x',t')
    -> ( CE.XPrim
            (C.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta t')
            CE.@~ xa CE.@~ x'
       , T.PairT ta t'))
   ( CE.XValue T.UnitT VUnit, T.UnitT )
   ( zip xs ts )

  -- Destruct nested pairs.
  -- Call "f" with expression for each element of the pair.
  pairDestruct f [] _ret
   = do nl <- fresh
        f' <- f []
        return $ CE.XLam nl T.UnitT $ f'

  pairDestruct f (t1:ts) ret
   = do nl <- fresh
        n1 <- fresh

        let f' xs = f (CE.XVar n1 : xs)
        let tr    = pairTypes ts

        rest <- pairDestruct f' ts ret

        let xfst = CE.XPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t1 tr) CE.@~ CE.XVar nl
        let xsnd = CE.XPrim (C.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t1 tr) CE.@~ CE.XVar nl

        let xx = CE.XLam nl (T.PairT t1 tr)
               $ CE.XLet n1 xfst
               ( rest CE.@~ xsnd )

        return xx


convertWindowUnits :: WindowUnit -> C.Exp n
convertWindowUnits wu
 = CE.constI
 $ case wu of
    Days d -> d
    -- TODO: month should be... better
    Months m -> m * 30
    Weeks w -> w * 7

baseTypeOrOption :: UniverseType -> BaseType
baseTypeOrOption u
 | Possibly <- universePossibility $ universe u
 = T.OptionT $ baseType u
 | otherwise
 = baseType u
