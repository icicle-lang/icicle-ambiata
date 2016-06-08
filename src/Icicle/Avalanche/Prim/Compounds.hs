{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Icicle.Avalanche.Prim.Compounds (
    X, S, A
  , FlatOps(..)
  , flatOps
  , FlatCons(..)
  , flatCons
  , writeThen
  ) where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import qualified    Icicle.Common.Exp.Prim.Minimal as Min
import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Statement.Statement

import              P

type X a n = Exp a n Prim
type S a n = Statement a n Prim
type A a n = Accumulator a n Prim

data FlatOps a n = FlatOps {
    arrLen  :: ValType -> X a n -> X a n
  , arrIx   :: ValType -> X a n -> X a n -> X a n
  , arrNew  :: ValType -> X a n -> X a n
  , arrUpd  :: ValType -> X a n -> X a n -> X a n -> X a n
  , arrUpd' :: ValType -> X a n -> X a n -> X a n -> X a n
  , arrDel  :: ValType -> X a n -> X a n -> X a n
  , arrZip  :: ValType -> ValType
            -> X a n -> X a n -> X a n

  , mapPack :: ValType -> ValType -> X a n -> X a n -> X a n
  , mapKeys :: ValType -> ValType -> X a n -> X a n
  , mapVals :: ValType -> ValType -> X a n -> X a n

  , bufPush :: Int -> ValType -> X a n -> X a n -> X a n
  , bufRead :: Int -> ValType -> X a n -> X a n

  , isSome   :: ValType -> X a n -> X a n
  , optionGet:: ValType -> X a n -> X a n
  , someF    :: ValType -> X a n -> X a n

  , isRightF :: ValType -> ValType -> X a n -> X a n
  , left     :: ValType -> ValType -> X a n -> X a n
  , right    :: ValType -> ValType -> X a n -> X a n

  , fstF     :: ValType -> ValType -> X a n -> X a n
  , sndF     :: ValType -> ValType -> X a n -> X a n
  , pair     :: ValType -> ValType -> X a n -> X a n -> X a n

  , structGet:: StructField -> ValType -> StructType
             -> X a n -> X a n

  , relEq    :: ValType -> X a n -> X a n -> X a n
  , relNe    :: ValType -> X a n -> X a n -> X a n
  , relGt    :: ValType -> X a n -> X a n -> X a n
  , relGe    :: ValType -> X a n -> X a n -> X a n
  , relLt    :: ValType -> X a n -> X a n -> X a n
  , relLe    :: ValType -> X a n -> X a n -> X a n

  }

flatOps :: a -> FlatOps a n
flatOps a_fresh
 = FlatOps{..}
 where
  xPrim         = XPrim  a_fresh
  xApp          = XApp   a_fresh

  p1 p x        = xPrim p  `xApp` x
  p2 p x y      = p1 p x   `xApp` y
  p3 p x y z    = p2 p x y `xApp` z

  arrLen  t     = p1 (PrimProject $ PrimProjectArrayLength t)
  arrIx   t     = p2 (PrimUnsafe  $ PrimUnsafeArrayIndex   t)
  arrNew  t     = p1 (PrimUnsafe  $ PrimUnsafeArrayCreate  t)
  arrUpd  t     = p3 (PrimArray   $ PrimArrayPutImmutable  t)
  arrUpd' t     = p3 (PrimArray   $ PrimArrayPutMutable  t)
  arrDel  t     = p2 (PrimArray   $ PrimArrayDel           t)
  arrZip  k v   = p2 (PrimArray   $ PrimArrayZip           k v)


  mapPack k v   = p2 (PrimMap $ PrimMapPack         k v)
  mapKeys k v   = p1 (PrimMap $ PrimMapUnpackKeys   k v)
  mapVals k v   = p1 (PrimMap $ PrimMapUnpackValues k v)

  bufPush n t   = p2 (PrimBuf $ PrimBufPush n t)
  bufRead n t   = p1 (PrimBuf $ PrimBufRead n t)

  isSome    t   = p1 (PrimProject (PrimProjectOptionIsSome t))
  optionGet t   = p1 (PrimUnsafe (PrimUnsafeOptionGet t))
  someF     t   = p1 (PrimMinimal $ Min.PrimConst $ Min.PrimConstSome t)

  isRightF t u  = p1 (PrimProject $ PrimProjectSumIsRight t u)
  left     t u  = p1 (PrimUnsafe  $ PrimUnsafeSumGetLeft  t u)
  right    t u  = p1 (PrimUnsafe  $ PrimUnsafeSumGetRight t u)

  fstF   t u    = p1 (PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t u)
  sndF   t u    = p1 (PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t u)
  pair   t u    = p2 (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair t u)

  structGet f t st
                = p1 (PrimMinimal $ Min.PrimStruct $ Min.PrimStructGet f t st)

  relEq     t   = p2 (PrimMinimal $ Min.PrimRelation Min.PrimRelationEq t)
  relNe     t   = p2 (PrimMinimal $ Min.PrimRelation Min.PrimRelationNe t)
  relGt     t   = p2 (PrimMinimal $ Min.PrimRelation Min.PrimRelationGt t)
  relGe     t   = p2 (PrimMinimal $ Min.PrimRelation Min.PrimRelationGe t)
  relLt     t   = p2 (PrimMinimal $ Min.PrimRelation Min.PrimRelationLt t)
  relLe     t   = p2 (PrimMinimal $ Min.PrimRelation Min.PrimRelationLe t)

--------------------------------------------------------------------------------


data FlatCons a n = FlatCons {
    xVar   :: Name n -> X a n
  , xPrim  :: Prim -> X a n
  , xValue :: ValType -> BaseValue -> X a n
  , xApp   :: X a n -> X a n -> X a n

  , xArray :: PrimArray -> X a n
  , xMin   :: Min.Prim -> X a n
  , xMath  :: Min.PrimBuiltinMath -> X a n
  , xArith :: Min.PrimArithBinary -> X a n
  , xRel   :: ValType -> Min.PrimRelation -> X a n
  }

flatCons :: a -> FlatCons a n
flatCons a_fresh
  = FlatCons{..}
  where
    xVar   = XVar   a_fresh
    xPrim  = XPrim  a_fresh
    xValue = XValue a_fresh
    xApp   = XApp   a_fresh

    xArray   = xPrim . PrimArray
    xMin     = xPrim . PrimMinimal
    xMath    = xMin  . Min.PrimBuiltinFun . Min.PrimBuiltinMath
    xArith   = xMin  . flip Min.PrimArithBinary Min.ArithIntT
    xRel  ty = xMin  . flip Min.PrimRelation ty

--------------------------------------------------------------------------------

writeThen :: Name n -> Exp a n p -> Statement a n p -> Statement a n p
writeThen n x s = Write n x <> s
