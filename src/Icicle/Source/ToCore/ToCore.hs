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
import                  Icicle.Source.ToCore.Prim
import                  Icicle.Source.Type

-- TODO: this should not really rely on a specific name type,
-- after structs are supported.
-- But for now just look for variable called "value" and know that is the
-- input element.
import                  Icicle.Source.Lexer.Token (Variable(..))

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


type Nm = Name Variable

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
        :: QueryTop (a,UniverseType) Variable
        -> ConvertM a Variable (C.Program Variable)
convertQueryTop qt
 = do   inp <- fresh
        -- TODO: look this up in context
        let inpTy       = T.IntT
        let inpTy'dated = T.PairT inpTy T.DateTimeT

        (bs,ret) <- convertQuery inp inpTy'dated (query qt)
        let bs'   = strm inp C.Source <> bs
        return (programOfBinds inpTy bs' ret)


-- | Convert a Query to Core
-- This takes the name of the input stream, the element types of the input,
-- and the query to transform.
-- It returns a list of program bindings, as well as the name of the binding
-- that is being "returned" in the program - essentially the last added binding.
convertQuery
        :: Nm -> T.ValType
        -> Query (a,UniverseType) Variable
        -> ConvertM a Variable (CoreBinds Variable, Nm)
convertQuery n nt q
 = case contexts q of
    -- There are no queries left, so deal with simple aggregates and nested queries.
    []
     -> convertReduce n nt (final q)

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
            e'      <- convertExp nv nt e

            (bs, b) <- convertQuery n' nt q'
            let bs'  = strm n' (C.STrans (C.SFilter nt) (CE.XLam nv nt e') n) <> bs

            return (bs', b)


    -- Windowing in Core must be at the start of a chain of stream transformers.
    -- I think this is actually a shortcoming of Core, and can be relatively easily
    -- fixed there.
    -- For now, this doesn't support "filter X ~> window Y", but only "window Y ~> filter X".
    --
    -- TODO: change Core so windowing is not a stream source but a stream transformer.
    (Windowed _ (Days days) Nothing : _)
     -> do  n'      <- fresh
            (bs, b) <- convertQuery n' nt q'
            let bs'  = strm n' (C.SourceWindowedDays days) <> bs
            return (bs', b)
    -- Likewise, Core doesn't support double-sided windowing yet.
    -- We don't want unbounded "older than" windowing because then the window contents will
    -- continue to grow unboundedly.
    -- Instead, only allow "older than" windowing if there is also a "newer than" bound on the
    -- other side.
    -- We could support "older than" by storing reduce result of the end of window and
    -- storing all corresponding newer thans in the snapshot, so if this ends up being an issue
    -- we can address it.
    --
    -- TODO: change Core to support window "between"
    (Windowed (ann,_) _ _ : _)
     -> lift $ Left $ ConvertErrorTODO ann "support window ranges"


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
                    <- convertGroupBy     n'v nt q'

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
                    <- convertGroupBy     nval nt q'

            let tV'  = baseType tV

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertExp         nval nt e

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
                    <- convertGroupBy     nval nt q'

            let tV'  = baseType tV

            -- Convert the "by" to a simple expression.
            -- This becomes the map insertion key.
            e'      <- convertExp         nval nt e

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

    -- TODO: let, let fold
    (Let (ann,_) _ _ : _)
     -> lift $ Left $ ConvertErrorTODO ann "convertQuery.Let"
    (LetFold (ann,_) _ : _)
     -> lift $ Left $ ConvertErrorTODO ann "convertQuery.LetFold"

        
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
        :: Nm   -> T.ValType
        -> Exp (a,UniverseType) Variable
        -> ConvertM a Variable (CoreBinds Variable, Nm)
convertReduce n t xx
 | Just (p, (_,ty), args) <- takePrimApps xx
 = case p of
    Agg Count
     | [] <- args
     -- Count: just add 1, ignoring the value
     -> fresh >>= mkFold T.IntT (\na -> na CE.+~ CE.constI 1) (CE.constI 0)
     | otherwise
     -> errAggBadArgs

    Agg SumA
     | [x] <- args
     -> do  nv <- fresh
            -- Convert the element expression and sum over it
            x' <- convertExp nv t x
            mkFold T.IntT (\na -> na CE.+~ x') (CE.constI 0) nv
     | otherwise
     -> errAggBadArgs


    -- TODO: implement newest and oldest.
    -- These aren't hard to implement exactly, but the issue is they return "Maybe v"
    -- while we want to be able to treat them as simply "v".
    --
    --(Agg Newest, [x])
    -- ->
    --(Agg Oldest, [x])
    -- ->

    -- For any other primitives:
    --   recurse into its arguments and get bindings for them
    --   apply those bindings to the primitive, as a postcomputation
    --
    -- If the binding is Pure however, it must not rely on any aggregates,
    -- so it might as well be a precomputation.
    _
     -> do  (bs,nms) <- unzip <$> mapM (convertReduce n t) args
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
 = convertQuery n t q
 -- Any variable must be a let-bound aggregate, so we can safely assume it has a binding.
 | Var _ v      <- xx
 = return (mempty, Name v)
 -- TODO: actually this should never happen, except for a non-primitive application
 | otherwise
 = lift $ Left $ ConvertErrorTODO (fst $ annotOfExp xx) "convertReduce"

 where
  -- Helper for creating a stream fold binding
  mkFold ta k z nv
   = do n' <- fresh
        na <- fresh
        let k' = CE.XLam na ta
               $ CE.XLam nv t
               $ k (CE.XVar na)
        return (red n' $ C.RFold t ta k' z n, n')

  -- Bad arguments to an aggregate
  errAggBadArgs
   = lift
   $ Left
   $ ConvertErrorReduceAggregateBadArguments (fst $ annotOfExp xx) xx


-- | Convert an element-level expression.
-- These are worker functions for folds, filters and so on.
convertExp
        :: Nm   -> T.ValType
        -> Exp (a,UniverseType) Variable
        -> ConvertM a Variable (C.Exp Variable)
convertExp nElem t x
 -- TODO: these should be struct lookups, not hardcoded like this.
 | Var _ (Variable "value") <- x
 , T.PairT t1 t2 <- t
 , T.DateTimeT <- t2
 = do   n1 <- fresh
        n2 <- fresh
        let fstF    = CE.XLam n1 t1
                    $ CE.XLam n2 t2
                    $ CE.XVar n1
        let unpair  = CE.XPrim (C.PrimFold (C.PrimFoldPair t1 t2) t1)
                    CE.@~ fstF
                    CE.@~ CE.XVar nElem
        
        return unpair

 | Var _ (Variable "date") <- x
 , T.PairT t1 t2 <- t
 , T.DateTimeT <- t2
 = do   n1 <- fresh
        n2 <- fresh
        let sndF    = CE.XLam n1 t1
                    $ CE.XLam n2 t2
                    $ CE.XVar n2
        let unpair  = CE.XPrim (C.PrimFold (C.PrimFoldPair t1 t2) t2)
                    CE.@~ sndF
                    CE.@~ CE.XVar nElem
        
        return unpair

 -- Primitive application: convert arguments, then convert primitive
 | Just (p, (ann,retty), args) <- takePrimApps x
 = do   args'   <- mapM (convertExp nElem t) args
        let tys  = fmap (snd . annotOfExp) args
        convertPrim p ann retty (args' `zip` tys)

 -- A real nested query should not appear here.
 -- However, if it has no contexts, it's really just a nested expression.
 | Nested _ (Query [] x') <- x
 = convertExp nElem t x'

 | otherwise
 = case x of
    Var (ann,_) n
     -> lift
      $ Left
      $ ConvertErrorExpNoSuchVariable ann n
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
        :: Nm -> T.ValType
        -> Query (a,UniverseType) Variable
        -> ConvertM a Variable (C.Exp Variable, C.Exp Variable, C.Exp Variable, UniverseType)
convertGroupBy nElem t q
 = case contexts q of
    -- No contexts, just an expression
    []
     -- Nested query; recurse
     | Nested _ qq <- final q
     -> convertGroupBy nElem t qq

     -- Primitive application
     | Just (p, (ann,retty), args) <- takePrimApps $ final q
     -> case p of
         -- Aggregates are relatively simple
         Agg SumA
          | [e] <- args
          -> do let retty' = baseType retty
                e' <- convertExp nElem t e

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
                (ks, zs, xs, ts) <- unzip4 <$> mapM (convertGroupBy nElem t . Query []) args

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
     -> do  (k,z,x,tt) <- convertGroupBy nElem t q'
            e'         <- convertExp     nElem t e
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

        let xx = CE.XLam nl (T.PairT t1 tr)
               ( CE.XPrim (C.PrimFold (C.PrimFoldPair t1 tr) ret)
                 CE.@~ (CE.XLam n1 t1 $ rest)
                 CE.@~ CE.XVar nl)

        return xx

