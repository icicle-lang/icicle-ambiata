-- | Special "Ord" instance for Thresher transform.
-- Faster.
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Statement.Simp.ThreshOrd (
    ThreshMapOrd(..)
  ) where

import              Icicle.Avalanche.Prim.Flat
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Common.Exp

import              P

import              Data.Hashable (Hashable)

-- | Ord instance specifically for Thresher.
-- The original Ord instance checks equality on types, annotations, and all sorts of junk.
-- This isn't necessary most of the time, because if you have "plus" applied to two arguments,
-- it doesn't matter what the type is: it only matters if the two arguments are the same.
newtype ThreshMapOrd a n
 = ThreshMapOrd (Exp a n Prim)

instance (Ord a, Hashable n, Eq n) => Eq (ThreshMapOrd a n) where
 (==) a b = (compare a b) == EQ

instance (Ord a, Hashable n, Eq n) => Ord (ThreshMapOrd a n) where
 compare (ThreshMapOrd topx) (ThreshMapOrd topy) = goX topx topy
  where
   goX x y
    = case (x, y) of
      (XVar _ n, XVar _ n')
       -> compare n n'
      (XPrim _ p, XPrim _ p')
       -> goP p p'
      (XValue{}, XValue{})
       -> compare x y -- compare v v'
      (XApp _ p q, XApp _ p' q')
       -> case goX p p' of
           LT -> LT
           EQ -> goX q q'
           GT -> GT

      -- Lambdas and lets should not occur, so we can defer to slow case for those
      _ -> compare x y

   goP x y
    = case (x, y) of
       (PrimMinimal p, PrimMinimal p')-> goPm p p'
       (PrimProject p, PrimProject p')-> goPr p p'
       (PrimUnsafe p, PrimUnsafe p')  -> goPu p p'
       (PrimArray p,  PrimArray p')   -> goPa p p'
       (PrimMelt p,   PrimMelt p')    -> goPMe p p'
       (PrimMap p,    PrimMap p')     -> goPMa p p'
       (PrimBuf p,    PrimBuf p')     -> goPB p p'
       (_, _) -> compare x y

   goPm x y
    = case (x, y) of
       (Min.PrimArithUnary p _, Min.PrimArithUnary p' _) -> compare p p'
       (Min.PrimArithBinary p _, Min.PrimArithBinary p' _) -> compare p p'
       (Min.PrimRelation p _, Min.PrimRelation p' _) -> compare p p'
       (Min.PrimLogical p, Min.PrimLogical p') -> compare p p'
       -- Constructors actually do matter what their type is.
       -- > left [Int] [Bool] 0
       -- can't be replaced with
       -- > left [Int] [big thing] 0
       -- can it?
       (Min.PrimConst p, Min.PrimConst p') ->  compare p p'
       (Min.PrimPair Min.PrimPairFst{}, Min.PrimPair Min.PrimPairFst{}) -> EQ
       (Min.PrimPair Min.PrimPairSnd{}, Min.PrimPair Min.PrimPairSnd{}) -> EQ
       (Min.PrimStruct (Min.PrimStructGet p _ _), Min.PrimStruct (Min.PrimStructGet p' _ _)) -> compare p p'
       (Min.PrimTime p, Min.PrimTime p') -> compare p p'
       (Min.PrimBuiltinFun p, Min.PrimBuiltinFun p') -> compare p p'

       (_, _) -> compare x y

   goPr x y
    = case (x, y) of
       (PrimProjectArrayLength{}, PrimProjectArrayLength{}) -> EQ
       (PrimProjectOptionIsSome{}, PrimProjectOptionIsSome{}) -> EQ
       (PrimProjectSumIsRight{}, PrimProjectSumIsRight{}) -> EQ
       (_, _) -> compare x y

   goPu x y
    = case (x, y) of
       (PrimUnsafeArrayIndex{}, PrimUnsafeArrayIndex{}) -> EQ
       (PrimUnsafeArrayCreate{}, PrimUnsafeArrayCreate{}) -> compare x y
       (PrimUnsafeSumGetLeft{}, PrimUnsafeSumGetLeft{}) -> EQ
       (PrimUnsafeSumGetRight{}, PrimUnsafeSumGetRight{}) -> EQ
       (PrimUnsafeOptionGet{}, PrimUnsafeOptionGet{}) -> EQ

       (_, _) -> compare x y

   goPa x y
    = case (x, y) of
       (PrimArrayPutMutable{}, PrimArrayPutMutable{}) -> EQ
       (PrimArrayPutImmutable{}, PrimArrayPutImmutable{}) -> EQ
       (PrimArrayZip{}, PrimArrayZip{}) -> EQ
       (_, _) -> compare x y

   goPMe x y
    = case (x, y) of
       (PrimMeltPack{}, PrimMeltPack{}) -> EQ
       (PrimMeltUnpack i _, PrimMeltUnpack i' _) -> compare i i'
       (_, _) -> compare x y

   goPMa x y
    = case (x, y) of
       (PrimMapPack{}, PrimMapPack{}) -> EQ
       (PrimMapUnpackKeys{}, PrimMapUnpackKeys{}) -> EQ
       (PrimMapUnpackValues{}, PrimMapUnpackValues{}) -> EQ
       (_, _) -> compare x y

   goPB x y
    = case (x, y) of
       (PrimBufMake{}, PrimBufMake{}) -> compare x y
       (PrimBufPush{}, PrimBufPush{}) -> EQ
       (PrimBufRead{}, PrimBufRead{}) -> EQ
       (_, _) -> compare x y



