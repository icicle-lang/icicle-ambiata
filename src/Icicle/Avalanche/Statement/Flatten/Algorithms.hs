-- | When converting from Core to Avalanche, we need to implement
--   some Core prims with non-trivial Avalanche.

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Icicle.Avalanche.Statement.Flatten.Algorithms where

import              Icicle.Avalanche.Statement.Flatten.Base

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat      as Flat

import qualified    Icicle.Core.Exp.Prim           as Core
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              Icicle.Internal.Pretty

import              P

import              Data.Hashable                  (Hashable)
import              Data.String                    (IsString)


type FlatX a n = a -> Exp a n Core.Prim -> (Exp a n Flat.Prim -> FlatM a n) -> FlatM a n
type StmtX a n = Exp a n Flat.Prim -> FlatM a n

--------------------------------------------------------------------------------

initInt :: Name n -> Exp a n p -> Statement a n p -> Statement a n  p
initInt acc init
  = InitAccumulator (Accumulator acc IntT init)

readInt :: Name n -> Name n -> Statement a n p -> Statement a n  p
readInt local acc
  = Read local acc IntT

initArr :: ValType -> Name n -> Exp a n p -> Statement a n p -> Statement a n p
initArr t acc init
  = InitAccumulator (Accumulator acc (ArrayT t) init)

readArr :: ValType -> Name n -> Name n -> Statement a n p -> Statement a n p
readArr t local acc
  = Read local acc (ArrayT t)

initBool :: Name n -> Exp a n p -> Statement a n p -> Statement a n  p
initBool acc init
  = InitAccumulator (Accumulator acc BoolT init)

--------------------------------------------------------------------------------

avalancheHeapSort
  :: forall a n . (Pretty n, Hashable n, Eq n, IsString n)
  => a
  -> StmtX a n
  -> FlatX a n
  -> ValType
  -> Exp a n Core.Prim
  -> FlatM a n

avalancheHeapSort a_fresh stm flatX t array
  = flatX a_fresh array $ \array' -> do
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

  where

    xVar   = XVar   a_fresh
    xPrim  = XPrim  a_fresh
    xValue = XValue a_fresh
    xApp   = XApp   a_fresh

    xMin     = xPrim . Flat.PrimMinimal
    xMath    = xMin  . Min.PrimBuiltinFun . Min.PrimBuiltinMath
    xArith   = xMin  . flip Min.PrimArithBinary Min.ArithIntT
    xRel  ty = xMin  . flip Min.PrimRelation ty

    xIndex arr i
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
     = [ xHeapSize l heapSize
       , xRel t Min.PrimRelationGt
          `xApp` xIndex arr l
          `xApp` xIndex arr i
       ]
    -- right < heap_size && arr[right] > arr[largest]
    xHeapRight arr heapSize r largest
     = [ xHeapSize r heapSize
       , xRel t Min.PrimRelationGt
           `xApp` xIndex arr r
           `xApp` xIndex arr largest
       ]
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
                [ nestedIfs
                    (xHeapLeft  v_arr v_heapSize v_left  v_index)
                    -- then largest  = left
                    (Write n_acc_largest  v_left)
                    -- else largest  = i
                    (Write n_acc_largest  v_index)
                -- if right < heap_size && array[right] > array[largest ]
                , readInt n_loc_largest  n_acc_largest
                $ nestedIfs
                    (xHeapRight v_arr v_heapSize v_right v_largest )
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
