-- | Turn Core primitives into Flat - removing the folds
-- The input statements must be in A-normal form.
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Icicle.Avalanche.Statement.Flatten.Exp (
    flatX, flatXS
  ) where

import              Icicle.Avalanche.Statement.Flatten.Base
import              Icicle.Avalanche.Statement.Flatten.Type

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat      as Flat
import qualified    Icicle.Avalanche.Prim.Compounds as Flat

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
import              Data.String                    (IsString)


flatXS  :: (Pretty n, Hashable n, Eq n, IsString n)
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
flatX   :: forall a n . (Pretty n, Hashable n, Eq n, IsString n)
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
  flatX'           = flatX        a_fresh
  Flat.FlatOps{..} = Flat.flatOps a_fresh
  makeApps'        = makeApps a_fresh

  xVar   = XVar   a_fresh
  xPrim  = XPrim  a_fresh
  xValue = XValue a_fresh
  xApp   = XApp   a_fresh

  xMin    = xPrim . Flat.PrimMinimal
  xMath   = xMin  . Min.PrimBuiltinFun . Min.PrimBuiltinMath
  xArith  = xMin  . flip Min.PrimArithBinary Min.ArithIntT
  xAnd    = xMin  $ Min.PrimLogical Min.PrimLogicalAnd
  xRel  t = xMin  . flip Min.PrimRelation t


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
  flatPrim :: Core.Prim -> [Exp a n Core.Prim] -> FlatM a n
  flatPrim p xs
   = case p of
      -- Built-in functions that are supported differently in Source/Core vs. in Avalanche/Sea
      -- Implement heap sort in Avalanche so that it can be melted.
      Core.PrimMinimal (Min.PrimBuiltinFun (Min.PrimBuiltinArray (Min.PrimBuiltinSort t)))
       | [array] <- xs
       -> flatX' array
       $ \array'
       -> do let xIndex arr i
                  = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeArrayIndex t))
                     `xApp` arr
                     `xApp` i
                 xLen arr
                  = xPrim (Flat.PrimProject (Flat.PrimProjectArrayLength t))
                     `xApp` arr
                 xNil      = xValue IntT (VInt 0)
                 xOne      = xValue IntT (VInt 1)
                 xTwo      = xValue IntT (VInt 2)
                 xMinusOne = xValue IntT (VInt (-1))
                 xTrue     = xValue BoolT (VBool True)
                 xFalse    = xValue BoolT (VBool False)
                 xMinus x y
                  = xArith Min.PrimArithMinus `xApp` x `xApp` y
                 xHalf i
                  = xMath Min.PrimBuiltinFloor
                     `xApp` (xMath Min.PrimBuiltinDiv
                              `xApp` (xMath Min.PrimBuiltinToDoubleFromInt
                                       `xApp` i)
                              `xApp` xValue DoubleT (VDouble 2.0))
                 xTimesTwo i
                  = xArith Min.PrimArithMul
                     `xApp` i
                     `xApp` xTwo
                 -- left i = 2 * i + 1
                 xLeft i
                  = xArith Min.PrimArithPlus
                     `xApp` xTimesTwo i
                     `xApp` xOne
                 -- right i = 2 * i + 2
                 xRight i
                  = xArith Min.PrimArithPlus
                     `xApp` xTimesTwo i
                     `xApp` xTwo
                 -- i < heap_size
                 xHeapSize i heapSize
                  = xRel IntT Min.PrimRelationLt
                     `xApp` i
                     `xApp` heapSize
                 -- left < heap_size && arr[left] > arr[i]
                 xHeapLeft arr heapSize l i
                  = xAnd
                     `xApp` xHeapSize l heapSize
                     `xApp` (xRel t Min.PrimRelationGt
                              `xApp` xIndex arr l
                              `xApp` xIndex arr i)
                 -- right < heap_size && arr[right] > arr[largest]
                 xHeapRight arr heapSize r largest 
                  = xAnd
                     `xApp` xHeapSize r heapSize
                     `xApp` (xRel t Min.PrimRelationGt
                              `xApp` xIndex arr r
                              `xApp` xIndex arr largest )
                 -- largest  /= i
                 xHeap i largest
                  = xRel IntT Min.PrimRelationNe
                     `xApp` largest
                     `xApp` i
                 -- swap (A[i], A[largest])
                 xSwap arr i largest 
                  = xPrim (Flat.PrimArray (Flat.PrimArraySwap t))
                     `xApp` arr
                     `xApp` i
                     `xApp` largest 

                 -- max_heap(array, heap_size, i):
                 --   left  = 2 * i + 1
                 --   right = 2 * i + 2
                 --   if left < heap_size && array[left] > array[i]
                 --     largest  = left
                 --   else
                 --     largest  = i
                 --   if right < heap_size && array[right] > array[largest ]
                 --     largest  = right
                 --   if largest  /= i
                 --     swap (array[i], array[largest ])
                 --     max_heap (array, heap_size, largest )
                 maxHeap n_acc_arr n_acc_heapSize n_acc_index
                  = do n_acc_left      <- freshPrefix "max_heap_acc_left"
                       n_acc_right     <- freshPrefix "max_heap_acc_right"
                       n_acc_largest   <- freshPrefix "max_heap_acc_largest "
                       n_acc_end       <- freshPrefix "max_heap_acc_end"
                       n_loc_left      <- freshPrefix "max_heap_left"
                       n_loc_right     <- freshPrefix "max_heap_right"
                       n_loc_largest   <- freshPrefix "max_heap_largest "
                       n_loc_arr       <- freshPrefix "max_heap_array"
                       n_loc_heapSize  <- freshPrefix "max_heap_size"
                       n_loc_index     <- freshPrefix "max_heap_index"
                       let v_left      = xVar n_loc_left
                           v_right     = xVar n_loc_right
                           v_largest   = xVar n_loc_largest
                           v_arr       = xVar n_loc_arr
                           v_heapSize  = xVar n_loc_heapSize
                           v_index     = xVar n_loc_index
                       return
                         $ initInt  n_acc_left     xMinusOne
                         $ initInt  n_acc_right    xMinusOne
                         $ initInt  n_acc_largest  xMinusOne
                         $ initBool n_acc_end      xFalse
                         $ While WhileEq n_acc_end xFalse
                         $ readInt   n_loc_index    n_acc_index
                         $ readArr t n_loc_arr      n_acc_arr
                         $ readInt   n_loc_heapSize n_acc_heapSize
                         $ Block
                           -- left  = 2 * i + 1
                           [ Write n_acc_left  (xLeft  v_index)
                           -- right = 2 * i + 2
                           , Write n_acc_right (xRight v_index)
                           , readInt n_loc_left     n_acc_left
                           $ readInt n_loc_right    n_acc_right
                           $ Block
                             -- if left < heap_size && array[left] > array[i]
                             [ If (xHeapLeft  v_arr v_heapSize v_left  v_index)
                                  -- then largest  = left
                                  (Write n_acc_largest  v_left)
                                  -- else largest  = i
                                  (Write n_acc_largest  v_index)
                             -- if right < heap_size && array[right] > array[largest ]
                             , readInt n_loc_largest  n_acc_largest
                             $ If (xHeapRight v_arr v_heapSize v_right v_largest )
                                  -- then largest  = right
                                  (Write n_acc_largest  v_right)
                                  mempty
                             -- if largest  /= i
                             , readInt n_loc_largest   n_acc_largest 
                             $ If (xHeap v_largest  v_index)
                                  -- then swap (array[i], array[largest ])
                                  (Block
                                    [ Write n_acc_arr (xSwap v_arr v_index v_largest )
                                    , Write n_acc_index v_largest 
                                    ])
                                  (Write n_acc_end xTrue)
                             ]
                           ]

                 -- build_max_heap (array):
                 --   heap_size = length (array)
                 --   for (i = floor (length (array) / 2); i >= 0; i--)
                 --     array = max_heap (array, heap_size, i)
                 buildMaxHeap n_acc_arr n_acc_heapSize
                  = do n_acc_index    <- freshPrefix "build_max_heap_acc_index"
                       n_loc_index    <- freshPrefix "build_max_heap_index"
                       n_loc_arr      <- freshPrefix "build_max_heap_array"
                       let v_index     = xVar n_loc_index
                           v_arr       = xVar n_loc_arr
                       body           <- maxHeap n_acc_arr n_acc_heapSize n_acc_index
                       return
                         $ readArr t n_loc_arr n_acc_arr
                         $ Block
                           -- heap_size = length (array)
                           [ Write   n_acc_heapSize (xLen v_arr)
                           , initInt n_acc_index    xMinusOne
                           -- for (i = floor (length (array) / 2); i > -1; i--)
                           $ ForeachInts ForeachStepDown n_loc_index (xHalf (xLen v_arr)) xMinusOne
                           $ Block
                             [ Write n_acc_index v_index
                             -- array = max_heap (array, heap_size, i)
                             , body
                             ]
                           ]

                 -- heap_sort (array):
                 --   array = build_max_heap (array)
                 --   for (i = length array - 1; i >= 2; i--)
                 --     array = swap (array[0], array[i])
                 --     heap_size = heap_size - 1
                 --     array = max_heap (array, heap_size, 0)
                 heapSort n_acc_arr n_acc_heapSize
                   = do n_acc_index    <- freshPrefix "heap_sort_acc_index"
                        n_loc_index    <- freshPrefix "heap_sort_index"
                        n_loc_heapSize <- freshPrefix "heap_sort_heapSize"
                        n_loc_arr      <- freshPrefix "heap_sort_arr"
                        let v_index     = xVar n_loc_index
                            v_heapSize  = xVar n_loc_heapSize
                            v_arr       = xVar n_loc_arr
                        body1 <- buildMaxHeap n_acc_arr n_acc_heapSize
                        body3 <- maxHeap      n_acc_arr n_acc_heapSize n_acc_index
                        let body2
                             = readArr t n_loc_arr n_acc_arr
                             $ Block
                               -- for (i = length array - 1; i > 0; i --)
                               [ ForeachInts ForeachStepDown n_loc_index (xMinus (xLen v_arr) xOne) xNil
                                 $ Block
                                   [ -- array = swap (array[0], array[i])
                                     Write   n_acc_arr (xSwap v_arr xNil v_index)
                                   , readInt n_loc_heapSize n_acc_heapSize
                                   $ Block
                                     -- heap_size = heap_size - 1
                                     [ Write n_acc_heapSize (xMinus v_heapSize xOne)
                                     , Write n_acc_index    xNil
                                     -- array = max_heap (array, heap_size, 0)
                                     , body3
                                     ]
                                   ]
                               ]
                        return
                          $ initInt   n_acc_index xMinusOne
                          $ readArr t n_loc_arr n_acc_arr
                          $ initInt   n_acc_heapSize (xLen v_arr)
                          $ Block [ body1, body2 ]

             n_loc_arr      <- freshPrefix "sort_array"
             n_acc_arr      <- freshPrefix "sort_acc_array"
             n_acc_heapSize <- freshPrefix "sort_acc_heap_size"
             sort   <- heapSort n_acc_arr n_acc_heapSize
             sorted <- stm (xVar n_loc_arr)
             return
               $ initArr t n_acc_arr array'
               $ Block
               [ sort
               , readArr t n_loc_arr n_acc_arr sorted
               ]


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
                 acc'map'k= Accumulator n'map'k (ArrayT tk) (mapKeys tk tv map')
                 acc'map'v= Accumulator n'map'v (ArrayT tv) (mapVals tk tv map')

                 x'done   = xVar n'done
                 x'map'k  = xVar n'map'k
                 x'map'v  = xVar n'map'v

                 read'k   = Read n'map'k n'map'k (ArrayT tk)
                 read'v   = Read n'map'v n'map'v (ArrayT tv)

                 sz       = xVar n'map'sz
                 get'k i  = arrIx  tk x'map'k i
                 get'v i  = arrIx  tv x'map'v i
                 put'v i x= arrUpd tv x'map'v i x

                 upd' i   = slet (get'v i)
                          $ \v    -> flatX' (upd `xApp` v)
                          $ \u' -> return (Write n'map'v (put'v i u') <> Write n'done (xValue BoolT $ VBool True))

             loop1       <- forI sz  $ \i
                         ->  (read'k
                         <$> (read'v
                         <$> (If (relEq tk (get'k i) key')
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
                          $ Let n'map' (mapPack tk tv x'map'k x'map'v)
                          $ stm'

                 ss       = InitAccumulator acc'done
                          $ InitAccumulator acc'map'k
                          $ InitAccumulator acc'map'v
                          $ read'k
                          $ Let n'map'sz (arrLen tk x'map'k)
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
                     (\v -> stm (mapPack tk tv (xVar n'keys) v))

             return  $ Let n'vals (mapVals tk tv map')
                     $ Let n'keys (mapKeys tk tv map')
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
                 upd' x   = Write n'res (someF tv x)

             loop1       <- slet (mapKeys tk tv map')  $ \map'k
                         -> slet (mapVals tk tv map')  $ \map'v
                         -> slet (arrLen  tk    map'k) $ \sz
                         -> forI sz  $ \i
                         -> return
                            (If (relEq tk (arrIx tk map'k i) key')
                               (upd' $ arrIx tv map'v i)
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

                loop <- forI    (arrLen ta arr')                  $ \iter
                     -> fmap    (Read accN accN arrT)                  $
                        slet    (arrIx ta arr' iter)              $ \elm
                     -> flatX'  (upd `xApp` elm)                       $ \elm'
                     -> slet    (arrUpd tb (xVar accN) iter elm') $ \arr''
                     -> return  (Write accN arr'')

                return $ InitAccumulator
                            (Accumulator accN arrT (arrNew tb (arrLen ta arr')))
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

               let pushed = pair tb1 tb2
                       (bufPush i FactIdentifierT (fstF tb1 tb2 buf') factid')
                       (bufPush i t               (sndF tb1 tb2 buf') e')

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
               let fpRead  = bufRead i t (sndF tb1 tb2 buf')
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
        ForeachInts ForeachStepUp n (xValue IntT (VInt 0)) to <$> ss (xVar n)

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
             ->  forI   (arrLen telem  arr')                 $ \iter
             ->  fmap   (Read accN accN valT)                     $
                 slet   (arrIx  telem arr' iter)             $ \elm
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

         return   $ Let n'keys (mapKeys tk tv mmm')
                  $ Let n'vals (mapVals tk tv mmm')
                  $ Let n'zips (arrZip  tk tv (xVar n'keys) (xVar n'vals))
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
      let if_   = If (isSome ta opt') (Let tmp (optionGet ta opt') ssome) snone
          -- After if, read back result from accumulator and then go do the rest of the statements
          read_ = Read acc acc valT stm'
      return (InitAccumulator (Accumulator acc valT $ xValue valT $ defaultOfType valT) (if_ <> read_))

  -- Fold over an either
  flatFold (Core.PrimFoldSum taOld tbOld) valT [xleft, xright, scrut]
   = let ta          = flatT taOld
         tb          = flatT tbOld
     in  flatX' scrut
      $ \scrut'
      -> do
         acc  <- fresh
         stm' <- stm (xVar acc)
         tmp  <- fresh
         tmp' <- fresh
         sleft   <- flatX' (xleft  `xApp` (xVar tmp )) $ (return . Write acc)
         sright  <- flatX' (xright `xApp` (xVar tmp')) $ (return . Write acc)

         let if_   = If (isRightF ta tb scrut')
                        (Let tmp' (right ta tb scrut') sright)
                        (Let tmp  (left  ta tb scrut') sleft)
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


  pushArray t n'acc push
   = do let t'   = ArrayT t
            sz arr = arrLen t arr
            put arr i x = arrUpd t arr i x

        update n'acc t' (\arr -> put arr (sz arr) push)

  windowEdge x (Days   d) = xPrim (Flat.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusDays)   `makeApps'` [x, xValue IntT $ VInt d]
  windowEdge x (Weeks  w) = xPrim (Flat.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusDays)   `makeApps'` [x, xValue IntT $ VInt (7*w)]
  windowEdge x (Months m) = xPrim (Flat.PrimMinimal $ Min.PrimTime Min.PrimTimeMinusMonths) `makeApps'` [x, xValue IntT $ VInt m]

  initInt acc init
    = InitAccumulator (Accumulator acc IntT init)
  readInt local acc
    = Read local acc IntT
  initArr t acc init
    = InitAccumulator (Accumulator acc (ArrayT t) init)
  readArr t local acc
    = Read local acc (ArrayT t)
  initBool acc init
    = InitAccumulator (Accumulator acc BoolT init)
