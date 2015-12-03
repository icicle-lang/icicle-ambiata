{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp.Constructor (
    constructor
  ) where

import              Icicle.Avalanche.Prim.Eval
import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Statement.Simp.ExpEnv
import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Common.Base
import              Icicle.Common.Exp
import              Icicle.Common.FixT
import              Icicle.Common.Fresh
import              Icicle.Common.Type

import              P

import qualified    Data.List as List
import qualified    Data.Map as Map


-- | Simplify applied primitives.
--
constructor :: (Eq a, Ord n) => a -> Statement a n Prim -> FixT (Fresh n) (Statement a n Prim)
constructor a_fresh statements
 = transformUDStmt goS emptyExpEnv statements
 where
  xApp       = XApp   a_fresh
  xPrim      = XPrim  a_fresh
  xValue     = XValue a_fresh
  xTrue      = xValue BoolT (VBool True)
  xFalse     = xValue BoolT (VBool False)
  xDefault t = xValue t (defaultOfType t)

  primAnd = primLogical  Min.PrimLogicalAnd
  primOr  = primLogical  Min.PrimLogicalOr

  primEq  = primRelation Min.PrimRelationEq
  primNe  = primRelation Min.PrimRelationNe

  primFold1 append xs
   = case xs of
      []     -> xValue UnitT VUnit -- this case shouldn't happen, make it a type error
      (y:[]) -> y
      (y:ys) -> y `append` primFold1 append ys

  primLogical op x y
   = xPrim (PrimMinimal (Min.PrimLogical op)) `xApp` x `xApp` y

  primRelation op t x y
   = xPrim (PrimMinimal (Min.PrimRelation op t)) `xApp` x `xApp` y

  primBufRead i t x
   = xPrim (PrimBuf (PrimBufRead i t)) `xApp` x

  primBufPush i t x y
   = xPrim (PrimBuf (PrimBufPush i t)) `xApp` x `xApp` y

  primArrayCreate i t
   = xPrim (PrimUnsafe (PrimUnsafeArrayCreate t)) `xApp` i

  primArrayLength t a
   = xPrim (PrimProject (PrimProjectArrayLength t)) `xApp` a

  primArrayGet i t a
   = xPrim (PrimUnsafe (PrimUnsafeArrayIndex t)) `xApp` a `xApp` i

  primArrayPut i t a v
   = xPrim (PrimUpdate (PrimUpdateArrayPut t)) `xApp` a `xApp` i `xApp` v

  primUnpack ix t x
   = xPrim (PrimMelt (PrimMeltUnpack ix t)) `xApp` x

  primPack env t xs
   = foldl (\f x -> f `xApp` x) (xPrim (PrimMelt (PrimMeltPack t))) (unpack env xs)

  primRepack env t skip take x
   = let start = length (concatMap meltType skip)
         end   = start + length (meltType take) - 1
     in primPack env take (fmap (\ix -> primUnpack ix t x) [start..end])

  goS env s
   = let env' = updateExpEnv s env
         ret s' = return (updateExpEnv s' env', s')

         goWith x s'
          = do x' <- goX env' x
               ret $ s' x'

     in  case s of
          If x t e
           -> goWith x $ \x' -> If x' t e
          Let n x ss
           -> goWith x $ \x' -> Let n x' ss
          ForeachInts n from to ss
           -> do from' <- goX env' from
                 to'   <- goX env' to
                 ret $ ForeachInts n from' to' ss
          InitAccumulator (Accumulator n vt x) ss
           -> goWith x $ \x' -> InitAccumulator (Accumulator n vt x') ss
          Write n x
           -> goWith x $ \x' -> Write n x'
          Output n t xts
           -> do xs <- mapM (goX env' . fst) xts
                 let ts = fmap snd xts
                 ret $ Output n t (List.zip xs ts)
          _
           -> ret s

  goX env x
   | Just prima <- takePrimApps x
   , Just x' <- goX' env prima
   = progress x'
   | otherwise
   = return x


  goX' env prima
   | (PrimMelt (PrimMeltPack t), [n]) <- prima
   , Nothing <- tryMeltType t
   = Just n

   | (PrimMelt (PrimMeltUnpack ix _), [n]) <- prima
   , Just x' <- resolve env n
   , Just v  <- fromPacked ix x'
   = Just v

   -- repacking projections
   | (PrimMinimal (Min.PrimPair (Min.PrimPairFst ta tb)), [n]) <- prima
   = Just $ primRepack env (PairT ta tb) [] ta n

   | (PrimMinimal (Min.PrimPair (Min.PrimPairSnd ta tb)), [n]) <- prima
   = Just $ primRepack env (PairT ta tb) [ta] tb n

   | (PrimMap (PrimMapUnpackKeys tk tv), [n]) <- prima
   = Just $ primRepack env (MapT tk tv) [] (ArrayT tk) n

   | (PrimMap (PrimMapUnpackValues tk tv), [n]) <- prima
   = Just $ primRepack env (MapT tk tv) [ArrayT tk] (ArrayT tv) n

   | (PrimProject (PrimProjectOptionIsSome tx), [n]) <- prima
   = Just $ primRepack env (OptionT tx) [] BoolT n

   | (PrimUnsafe (PrimUnsafeOptionGet tx), [n]) <- prima
   = Just $ primRepack env (OptionT tx) [BoolT] tx n

   | (PrimProject (PrimProjectSumIsRight ta tb), [n]) <- prima
   = Just $ primRepack env (SumT ta tb) [] BoolT n

   | (PrimUnsafe (PrimUnsafeSumGetLeft ta tb), [n]) <- prima
   = Just $ primRepack env (SumT ta tb) [BoolT] ta n

   | (PrimUnsafe (PrimUnsafeSumGetRight ta tb), [n]) <- prima
   = Just $ primRepack env (SumT ta tb) [BoolT, ta] tb n

   | (PrimMinimal (Min.PrimStruct (Min.PrimStructGet f tf (StructType fs))), [n]) <- prima
   , fs'        <- Map.insert f tf fs
   , (pre, _:_) <- List.span (/= f)       (Map.keys  fs')
   , tpre       <- List.take (length pre) (Map.elems fs')
   = Just $ primRepack env (StructT (StructType fs')) tpre tf n

   -- repacking const
   | (PrimMinimal (Min.PrimConst (Min.PrimConstPair ta tb)), [na, nb]) <- prima
   = Just $ primPack env (PairT ta tb) [na, nb]

   | (PrimArray (PrimArrayZip ta tb), [nk, nv]) <- prima
   = Just $ primPack env (ArrayT (PairT ta tb)) [nk, nv]

   | (PrimMap (PrimMapPack tk tv), [nk, nv]) <- prima
   = Just $ primPack env (MapT tk tv) [nk, nv]

   | (PrimMinimal (Min.PrimConst (Min.PrimConstSome tx)), [n]) <- prima
   = Just $ primPack env (OptionT tx) [xTrue, n]

   | (PrimMinimal (Min.PrimConst (Min.PrimConstLeft ta tb)), [n]) <- prima
   = Just $ primPack env (SumT ta tb) [xFalse, n, xDefault tb]

   | (PrimMinimal (Min.PrimConst (Min.PrimConstRight ta tb)), [n]) <- prima
   = Just $ primPack env (SumT ta tb) [xTrue, xDefault ta, n]


   -- buffers
   | (PrimBuf (PrimBufPush i tx), [nb, nx]) <- prima
   , Just ts <- tryMeltType tx
   , tis     <- List.zip ts [0..]
   = Just $ primPack env (BufT i tx)
   $ fmap (\(t, ix) -> primBufPush i t (primUnpack ix (BufT i tx) nb)
                                       (primUnpack ix tx          nx)) tis

   | (PrimBuf (PrimBufRead i tx), [n]) <- prima
   , Just ts <- tryMeltType tx
   , tis     <- List.zip ts [0..]
   = Just $ primPack env (ArrayT tx)
   $ fmap (\(t, ix) -> primBufRead i t (primUnpack ix (BufT i tx) n)) tis


   -- arrays
   | (PrimUnsafe (PrimUnsafeArrayCreate tx), [n]) <- prima
   , Just ts <- tryMeltType tx
   = Just $ primPack env (ArrayT tx)
   $ fmap (primArrayCreate n) ts

   | (PrimProject (PrimProjectArrayLength tx), [n]) <- prima
   , Just (t:_) <- tryMeltType tx
   = Just $ primArrayLength t (primUnpack 0 (ArrayT tx) n)

   | (PrimUnsafe (PrimUnsafeArrayIndex tx), [n, aix]) <- prima
   , Just ts <- tryMeltType tx
   , tis     <- List.zip ts [0..]
   = Just $ primPack env tx
   $ fmap (\(t, ix) -> primArrayGet aix t (primUnpack ix (ArrayT tx) n)) tis

   | (PrimUpdate (PrimUpdateArrayPut tx), [na, aix, nv]) <- prima
   , Just ts <- tryMeltType tx
   , tis     <- List.zip ts [0..]
   = Just $ primPack env (ArrayT tx)
   $ fmap (\(t, ix) -> primArrayPut aix t (primUnpack ix (ArrayT tx) na)
                                          (primUnpack ix         tx  nv)) tis

   -- comparison
   | (PrimMinimal (Min.PrimRelation Min.PrimRelationEq t), [nx, ny]) <- prima
   , Just ts <- tryMeltType t
   , tis     <- List.zip ts [0..]
   = Just $ primFold1 primAnd
   $ fmap (\(tv, i) -> primEq tv (primUnpack i t nx) (primUnpack i t ny)) tis

   | (PrimMinimal (Min.PrimRelation Min.PrimRelationNe t), [nx, ny]) <- prima
   , Just ts <- tryMeltType t
   , tis     <- List.zip ts [0..]
   = Just $ primFold1 primOr
   $ fmap (\(tv, i) -> primNe tv (primUnpack i t nx) (primUnpack i t ny)) tis


   | otherwise
   = Nothing

  fromPacked ix x
   | XValue _ t v <- x
   , ts           <- meltType t
   , Just vs      <- meltValue v t
   , (t',v'):_    <- drop ix (List.zip ts vs)
   = Just (xValue t' v')

   | Just (PrimMelt (PrimMeltPack _), xs) <- takePrimApps x
   , (xx:_)                              <- drop ix xs
   = Just xx

   | otherwise
   = Nothing


  unpack env xs
   = concatMap (unpack' env) xs

  unpack' env x
   | XValue _ t v <- x
   , Just ts      <- tryMeltType t
   , Just vs      <- meltValue v t
   = List.zipWith xValue ts vs

   | XVar _ n <- x
   , Just x'  <- getFromEnv env n
   = unpack' env x'

   | Just (PrimMelt (PrimMeltPack _), ns) <- takePrimApps x
   = concatMap (unpack' env) ns

   | otherwise
   = [x]


  -- Either lookup a name, or just return the value if it's already a constant.
  resolve env    (XVar   _ n)   = getFromEnv env n
  resolve _   xx@(XValue _ _ _) = Just xx
  resolve _       _             = Nothing

