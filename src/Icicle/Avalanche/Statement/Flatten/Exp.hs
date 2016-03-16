-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Flatten.Exp (
    flatX, flatXS
  ) where

import              Icicle.Avalanche.Statement.Flatten.Base
import              Icicle.Avalanche.Statement.Flatten.Type

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat     as Flat

import qualified    Icicle.Core.Exp.Prim           as Core
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import              Icicle.Common.Exp.Simp.Beta
import              Icicle.Common.Fresh

import              Icicle.Internal.Pretty

import              P
import              Control.Monad.Trans.Class

import              Data.Hashable                  (Hashable)


flatXS  :: (Pretty n, Hashable n, Eq n)
        => a
        -> [Exp a n Core.Prim]
        -> [Exp a n Flat.Prim]
        -> ([Exp a n Flat.Prim] -> FlatM a n)
        -> FlatM a n

flatXS _       []     ys stm = stm ys
flatXS a_fresh (x:xs) ys stm
 = flatX a_fresh x
 $ \x'
 -> flatXS a_fresh xs (ys <> [x']) stm


-- | Flatten an expression, wrapping the statement with any lets or loops or other bindings
-- The statement function takes the new expression.
flatX   :: (Pretty n, Hashable n, Eq n)
        => a
        -> Exp a n Core.Prim
        -> (Exp a n Flat.Prim -> FlatM a n)
        -> FlatM a n

flatX a_fresh xx stm
 = convX
 where
  -- Do a bit of simplification.
  -- Betas must be converted to lets even if they are not simple values:
  -- Unapplied lambdas aren't really able to be converted, since
  -- we're lifting some expressions to statements, and there is no statement-level
  -- lambda, only expression-level lambda.
  x' = beta isSimpleValue
     $ betaToLets a_fresh xx

  -- Annotation plumbing
  flatX'    = flatX    a_fresh
  makeApps' = makeApps a_fresh
  xVar      = XVar     a_fresh
  xPrim     = XPrim    a_fresh
  xValue    = XValue   a_fresh
  xApp      = XApp     a_fresh

  -- Convert the simplified expression.
  convX
   = case x' of
      -- If it doesn't do anything interesting, we can just call the statement
      -- with the original expression
      XVar a n
       -> stm $ XVar a n
      XValue a vt bv
       -> stm $ XValue a (flatT vt) (flatV bv)

      XApp{}
       -- Primitive applications are where it gets interesting.
       -- See below
       | Just (p,xs) <- takePrimApps x'
       -> flatPrim p xs

       -- What is the function of this application?
       --
       -- It's not a primitive.
       -- It's not a lambda, since we did betaToLets above.
       -- It's not a value, since that wouldn't typecheck
       -- It's not a let-bound variable, since lets can't bind funs & so wouldn't typecheck.
       --
       -- Therefore, this should not happen for a valid program.
       | otherwise
       -> lift $ Left $ FlattenErrorApplicationNonPrimitive x'

      XPrim _ p
       -> flatPrim p []

      -- Unapplied lambda: this should not happen for a well-typed program
      XLam{}
       -> lift $ Left $ FlattenErrorBareLambda x'


      -- Convert expression lets into statement lets
      XLet _ n p q
       -> flatX' p
       $ \p'
       -> Let n p' <$> flatX' q stm


  -- Handle primitive applications.
  -- PrimFolds get turned into loops and whatnot
  flatPrim p xs
   = case p of
      -- Arithmetic and simple stuff are easy, just move it over
      Core.PrimMinimal pm
       -> primApps (Flat.PrimMinimal $ flatPrimMinimal pm) xs []

      -- Handle folds below
      Core.PrimFold pf ta
       -> flatFold pf (flatT ta) xs

      -- Map: insert value into map, or if key already exists,
      -- apply update function to existing value
      Core.PrimMap (Core.PrimMapInsertOrUpdate tkOld tvOld)
       | [upd, ins, key, map]   <- xs
       , tk <- flatT tkOld
       , tv <- flatT tvOld
       -> flatX' key
       $ \key'
       -> flatX' map
       $ \map'
       -> do n'done      <- fresh
             n'map'k     <- fresh
             n'map'v     <- fresh
             n'map'sz    <- fresh
             let acc'done = Accumulator n'done  BoolT $ xValue BoolT $ VBool False
                 acc'map'k= Accumulator n'map'k (ArrayT tk) (fpMapKeys tk tv `xApp` map')
                 acc'map'v= Accumulator n'map'v (ArrayT tv) (fpMapVals tk tv `xApp` map')

                 x'done   = xVar n'done
                 x'map'k  = xVar n'map'k
                 x'map'v  = xVar n'map'v

                 read'k   = Read n'map'k n'map'k (ArrayT tk)
                 read'v   = Read n'map'v n'map'v (ArrayT tv)

                 sz       = xVar n'map'sz -- fpArrLen tk `xApp` x'map'k
                 eq       = xPrim $ Flat.PrimMinimal $ Min.PrimRelation Min.PrimRelationEq tk
                 get'k i  = fpArrIx  tk `makeApps'` [x'map'k, i]
                 get'v i  = fpArrIx  tv `makeApps'` [x'map'v, i]
                 put'v i x= fpArrUpd tv `makeApps'` [x'map'v, i, x]

                 upd' i   = slet (get'v i)
                          $ \v    -> flatX' (upd `xApp` v)
                          $ \u' -> return (Write n'map'v (put'v i u') <> Write n'done (xValue BoolT $ VBool True))

             loop1       <- forI sz  $ \i
                         ->  (read'k
                         <$> (read'v
                         <$> (If (eq `makeApps'` [get'k i, key'])
                               <$> upd' i
                               <*> return mempty)))

             loop2       <- pushArray tk n'map'k key'
             loop3       <- flatX' ins
                          $ pushArray tv n'map'v

             -- XX can't read with same name twice I think
             -- n'map'rr    <- fresh
             n'map'      <- fresh
             stm'        <- stm $ xVar n'map'

             let if_ins   = Read n'done n'done BoolT
                          $ If x'done mempty (loop2 <> loop3)

                 stm''    = read'k
                          $ read'v
                          $ Let n'map' (fpMapPack tk tv `makeApps'` [x'map'k, x'map'v])
                          $ stm'

                 ss       = InitAccumulator acc'done
                          $ InitAccumulator acc'map'k
                          $ InitAccumulator acc'map'v
                          $ read'k
                          $ Let n'map'sz (fpArrLen tk `xApp` x'map'k)
                          ( loop1 <> if_ins <> stm'' )

             return ss

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      -- Map: create new empty map, for each element, etc
      Core.PrimMap (Core.PrimMapMapValues tkOld tvOld tvOld')
       | [upd, map]   <- xs
       , tk <- flatT tkOld
       , tv <- flatT tvOld
       , tv'<- flatT tvOld'
       -> flatX' map
       $ \map'
       -> do n'keys <- fresh
             n'vals <- fresh

             stm'   <- flatX'
                     ( xPrim (Core.PrimArray $ Core.PrimArrayMap tv tv')
                       `makeApps'` [upd, xVar n'vals] )
                     (\v -> stm (fpMapPack tk tv `makeApps'` [xVar n'keys, v]))

             return  $ Let n'vals (fpMapVals tk tv `xApp` map')
                     $ Let n'keys (fpMapKeys tk tv `xApp` map')
                     $ stm'

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      -- Map: lookup by key
      Core.PrimMap (Core.PrimMapLookup tkOld tvOld)
       | [map, key]   <- xs
       , tk <- flatT tkOld
       , tv <- flatT tvOld
       -> flatX' map
       $ \map'
       -> flatX' key
       $ \key'
       -> do n'res       <- fresh
             let acc'res  = Accumulator n'res (OptionT tv) $ xValue (OptionT tv) VNone

                 read'r   = Read n'res n'res (OptionT tv)

                 eq       = xPrim $ Flat.PrimMinimal $ Min.PrimRelation Min.PrimRelationEq tk
                 get t ar i = fpArrIx  t `makeApps'` [ar, i]
                 upd' x   = Write n'res ((xPrim $ Flat.PrimMinimal $ Min.PrimConst $ Min.PrimConstSome tv) `xApp` x)

             loop1       <- slet (fpMapKeys tk tv `xApp` map')  $ \map'k
                         -> slet (fpMapVals tk tv `xApp` map')  $ \map'v
                         -> slet (fpArrLen  tk    `xApp` map'k) $ \sz
                         -> forI sz  $ \i
                         -> return
                            (If (eq `makeApps'` [get tk map'k i, key'])
                               (upd' $ get tv map'v i)
                               mempty)

             stm'        <- stm $ xVar n'res

             let ss       = InitAccumulator acc'res
                          ( loop1 <> read'r stm' )

             return ss

       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- Map over array: create new empty array, for each element, etc
      Core.PrimArray (Core.PrimArrayMap taOld tbOld)
       | [upd, arr]   <- xs
       , ta <- flatT taOld
       , tb <- flatT tbOld
       -> flatX' arr
       $ \arr'
       -> do    accN <- fresh
                stm' <- stm (xVar accN)

                let arrT = ArrayT tb

                loop <- forI (fpArrLen ta `xApp` arr')                 $ \iter
                     -> fmap    (Read accN accN arrT)                  $
                        slet    (fpArrIx ta `makeApps'` [arr', iter])  $ \elm
                     -> flatX'  (upd `xApp` elm)                    $ \elm'
                     -> slet    (fpArrUpd tb `makeApps'` [xVar accN, iter, elm']) $ \arr''
                     -> return  (Write accN arr'')

                return $ InitAccumulator
                            (Accumulator accN arrT (fpArrNew tb `xApp` (fpArrLen ta `xApp` arr')))
                            (loop <> Read accN accN arrT stm')


       -- Map with wrong arguments
       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- LatestPush b f v -> (BufPush (fst b) f, BufPush (snd b) v)
      Core.PrimLatest (Core.PrimLatestPush i tOld)
       | [buf, factid, e]    <- xs
       , t <- flatT tOld
       -> flatX' e
       $  \e'
       -> flatX' factid
       $  \factid'
       -> flatX' buf
       $  \buf'
       -> do   tmpN      <- fresh
               let tb1 = BufT i FactIdentifierT 
               let tb2 = BufT i t
               let proj' ix = projFlat ix tb1 tb2 buf'
               let fpPush t' b v = xPrim (Flat.PrimBuf $ Flat.PrimBufPush i t') `xApp` b `xApp` v
               let fpPair a b = xPrim (Flat.PrimMinimal $ Min.PrimConst $ Min.PrimConstPair tb1 tb2) `xApp` a `xApp` b

               let pushed = fpPair
                       (fpPush FactIdentifierT (proj' False) factid')
                       (fpPush t (proj' True) e')

               stm'      <- stm (xVar tmpN)
               return
                 $ Let tmpN pushed stm'

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


      -- LatestRead b     -> BufRead (snd b)
      Core.PrimLatest (Core.PrimLatestRead i tOld)
       | [buf] <- xs
       , t <- flatT tOld
       -> flatX' buf
       $  \buf'
       -> do   tmpN       <- fresh
               let tb1 = BufT i FactIdentifierT 
               let tb2 = BufT i t
               let proj' ix = projFlat ix tb1 tb2 buf'
               let fpRead  = xPrim (Flat.PrimBuf $ Flat.PrimBufRead i t) `xApp` (proj' True)
               stm'       <- stm (xVar tmpN)
               return
                 $ Let tmpN fpRead stm'

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs

      Core.PrimWindow newerThan olderThan
       | [now, fact, factid] <- xs
       -> flatX' now
       $  \now'
       -> flatX' fact
       $  \fact'
       -> flatX' factid
       $  \factid'
       -> let  ge    = xPrim $ Flat.PrimMinimal $ Min.PrimRelation Min.PrimRelationGe TimeT
               andb  = xPrim $ Flat.PrimMinimal $ Min.PrimLogical  Min.PrimLogicalAnd
               newer = ge `makeApps'` [fact', windowEdge now' newerThan]
               both  | Just olderThan' <- olderThan
                     = andb `makeApps'` [ newer, ge `makeApps'` [windowEdge now' olderThan', fact']]
                     | otherwise
                     = newer
          in do stm' <- stm both
                return (If newer (KeepFactInHistory factid') mempty <> stm')

       | otherwise
       -> lift $ Left $ FlattenErrorPrimBadArgs p xs


  -- Convert arguments to a simple primitive.
  -- conv is what we've already converted
  primApps p [] conv
   = stm
   $ makeApps' (xPrim p)
   $ reverse conv

  primApps p (a:as) conv
   = flatX' a
   $ \a'
   -> primApps p as (a' : conv)

  -- Create a let binding with a fresh name
  slet x ss
   = do n  <- fresh
        Let n x <$> ss (xVar n)

  -- For loop with fresh name for iterator
  forI to ss
   = do n  <- fresh
        ForeachInts n (xValue IntT (VInt 0)) to <$> ss (xVar n)

  -- Update an accumulator
  update acc t x
   = do n'x <- fresh
        return $ Read n'x acc t $ Write acc $ x $ xVar n'x


  -- Handle primitive folds
  --
  -- Bool is just an if
  flatFold Core.PrimFoldBool valT [then_, else_, pred]
   = flatX' pred
   $ \pred'
   -> do -- Fresh name for accumulator and result.
         -- We can use same name for acc & result variables because accumulators and variables are in different scopes
         acc <- fresh
         -- Compute the rest of the computation, assuming we've stored result in variable named acc
         stm' <- stm (xVar acc)
         sthen <- flatX' then_ $ (return . Write acc)
         selse <- flatX' else_ $ (return . Write acc)
         -- Perform if and write result
         let if_ =  If pred' sthen selse
         -- After if, read back result from accumulator and then go do the rest of the statements
         let read_ = Read acc acc valT stm'
         return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))

  -- Array fold becomes a loop
  flatFold (Core.PrimFoldArray telemOld) valT [k, z, arr]
   = do accN <- fresh
        stm' <- stm (xVar accN)
        let telem = flatT telemOld

        -- Loop body updates accumulator with k function
        loop <-  flatX' arr                                       $ \arr'
             ->  forI   (fpArrLen telem `xApp` arr')              $ \iter
             ->  fmap   (Read accN accN valT)                     $
                 slet   (fpArrIx  telem `makeApps'` [arr', iter]) $ \elm
             ->  flatX' (makeApps' k [xVar accN, elm])            $ \x
             ->  return (Write accN x)

        -- Initialise accumulator with value z, execute loop, read from accumulator
        flatX' z $ \z' ->
            return (InitAccumulator (Accumulator accN valT z')
                   (loop <> Read accN accN valT stm'))


  -- Fold over map. Very similar to above
  flatFold (Core.PrimFoldMap tkOld tvOld) valT [k, z, mmm]
   =  flatX' mmm
   $ \mmm'
   -> do n'keys  <- fresh
         n'vals  <- fresh
         n'zips  <- fresh
         let tk = flatT tkOld
         let tv = flatT tvOld

         n'kv <- fresh
         n'ac <- fresh
         let k'   = XLam a_fresh n'ac  valT
                  $ XLam a_fresh n'kv (PairT tk tv)
                  ( k `makeApps'` [ xVar n'ac
                                  , projCore False tk tv $ xVar n'kv
                                  , projCore True  tk tv $ xVar n'kv ])

         res     <- flatX'
                  ( xPrim (Core.PrimFold (Core.PrimFoldArray (PairT tk tv)) valT)
                     `makeApps'` [k', z, xVar n'zips] ) stm

         return   $ Let n'keys (fpMapKeys tk tv `xApp` mmm')
                  $ Let n'vals (fpMapVals tk tv `xApp` mmm')
                  $ Let n'zips (fpArrZip  tk tv `makeApps'` [xVar n'keys, xVar n'vals])
                  $ res



  -- Fold over an option is just "maybe" combinator.
  flatFold (Core.PrimFoldOption taOld) valT [xsome, xnone, opt]
   = flatX' opt
   $ \opt'
   -> do
      acc  <- fresh
      stm' <- stm (xVar acc)
      tmp  <- fresh
      let ta = flatT taOld
      let vunit = xValue UnitT VUnit
      ssome <- flatX' (xsome `xApp` (xVar tmp)) $ (return . Write acc)
      snone <- flatX' (xnone `xApp` vunit)  $ (return . Write acc)
      let if_   = If (fpIsSome ta `xApp` opt') (Let tmp (fpOptionGet ta `xApp` opt') ssome) snone
          -- After if, read back result from accumulator and then go do the rest of the statements
          read_ = Read acc acc valT stm'
      return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))

  -- Fold over an either
  flatFold (Core.PrimFoldSum taOld tbOld) valT [xleft, xright, scrut]
   = let ta          = flatT taOld
         tb          = flatT tbOld
         fpIsRight   = xPrim (Flat.PrimProject  (Flat.PrimProjectSumIsRight ta tb))
         fpLeft      = xPrim (Flat.PrimUnsafe   (Flat.PrimUnsafeSumGetLeft  ta tb))
         fpRight     = xPrim (Flat.PrimUnsafe   (Flat.PrimUnsafeSumGetRight ta tb))
     in  flatX' scrut
      $ \scrut'
      -> do
         acc  <- fresh
         stm' <- stm (xVar acc)
         tmp  <- fresh
         tmp' <- fresh
         sleft   <- flatX' (xleft  `xApp` (xVar tmp )) $ (return . Write acc)
         sright  <- flatX' (xright `xApp` (xVar tmp')) $ (return . Write acc)

         let if_   = If (fpIsRight `xApp` scrut') (Let tmp' (fpRight `xApp` scrut') sright) (Let tmp (fpLeft `xApp` scrut') sleft)
           -- After if, read back result from accumulator and then go do the rest of the statements
             read_ = Read acc acc valT stm'
         return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))


  -- None of the above cases apply, so must be bad arguments
  flatFold pf rt xs
   = lift $ Left $ FlattenErrorPrimBadArgs (Core.PrimFold pf rt) xs

  -- Create a fst# or snd#
  proj pmin t ta tb e
   = let pm | not t
            = Min.PrimPairFst ta tb
            | otherwise
            = Min.PrimPairSnd ta tb
     in (xPrim $ pmin $ Min.PrimPair $ pm) `xApp` e
  projCore = proj Core.PrimMinimal
  projFlat = proj Flat.PrimMinimal


  fpArrLen t = xPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength t)
  fpArrIx  t = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayIndex   t)
  fpArrNew t = xPrim (Flat.PrimUnsafe  $ Flat.PrimUnsafeArrayCreate  t)
  fpArrUpd t = xPrim (Flat.PrimUpdate  $ Flat.PrimUpdateArrayPut     t)
  fpArrZip k v = xPrim (Flat.PrimArray $ Flat.PrimArrayZip           k v)


  fpMapPack k v = xPrim (Flat.PrimMap (Flat.PrimMapPack         k v))
  fpMapKeys k v = xPrim (Flat.PrimMap (Flat.PrimMapUnpackKeys   k v))
  fpMapVals k v = xPrim (Flat.PrimMap (Flat.PrimMapUnpackValues k v))

  fpIsSome    t = xPrim (Flat.PrimProject (Flat.PrimProjectOptionIsSome t))
  fpOptionGet t = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeOptionGet t))

  pushArray t n'acc push
   = do let t'   = ArrayT t
            sz arr = fpArrLen t `xApp` arr
            put arr i x = fpArrUpd t `makeApps'` [arr, i, x]

        update n'acc t' (\arr -> put arr (sz arr) push)

  windowEdge x (Days   d) = xPrim (Flat.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusDays)   `makeApps'` [x, xValue IntT $ VInt d]
  windowEdge x (Weeks  w) = xPrim (Flat.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusDays)   `makeApps'` [x, xValue IntT $ VInt (7*w)]
  windowEdge x (Months m) = xPrim (Flat.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusMonths) `makeApps'` [x, xValue IntT $ VInt m]


