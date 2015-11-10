{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -w #-}
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
import              Icicle.Common.Fresh
import              Icicle.Common.Type

import              P

import qualified    Data.List as List
import qualified    Data.Map as Map


-- | Simplify applied primitives.
--
constructor :: (Eq a, Ord n) => a -> Statement a n Prim -> Fresh n (Statement a n Prim)
constructor a_fresh statements
 = transformUDStmt goS emptyExpEnv statements
 where
  xApp       = XApp   a_fresh
  xPrim      = XPrim  a_fresh
  xValue     = XValue a_fresh
  xTrue      = xValue BoolT (VBool True)
  xFalse     = xValue BoolT (VBool False)
  xDefault t = xValue t (defaultOfType t)

  primBufRead t x
   = xPrim (PrimBuf (PrimBufRead t)) `xApp` x

  primBufPush t x y
   = xPrim (PrimBuf (PrimBufPush t)) `xApp` x `xApp` y

  primArrayCreate i t
   = xPrim (PrimUnsafe (PrimUnsafeArrayCreate t)) `xApp` i

  primArrayLength t a
   = xPrim (PrimProject (PrimProjectArrayLength t)) `xApp` a

  primArrayGet i t a
   = xPrim (PrimUnsafe (PrimUnsafeArrayIndex t)) `xApp` a `xApp` i

  primArrayPut i t a v
   = xPrim (PrimUpdate (PrimUpdateArrayPut t)) `xApp` a `xApp` i `xApp` v

  primPackGet ix t x
   = xPrim (PrimPack (PrimPackGet ix t)) `xApp` x

  primPackAll env t xs
   = foldl (\f x -> f `xApp` x) (xPrim (PrimPack (PrimPackAll t))) (unpack env xs)

  primRepack  env t skip take x
   = let start = length (concatMap meltType skip)
         end   = start + length (meltType take) - 1
     in primPackAll env take (fmap (\ix -> primPackGet ix t x) [start..end])

  goS env s
   = let env' = updateExpEnv s env
         go   = goX env
         ret s' = return (updateExpEnv s' env', s')
     in  case s of
          If x t e
           -> ret $ If (go x) t e
          Let n x ss
           -> ret $ Let n (go x) ss
          ForeachInts n from to ss
           -> ret $ ForeachInts n (go from) (go to) ss
          InitAccumulator (Accumulator n vt x) ss
           -> ret $ InitAccumulator (Accumulator n vt (go x)) ss
          Write n x
           -> ret $ Write n (go x)
          Output n t xts
           | xs <- fmap (go . fst) xts
           , ts <- fmap snd xts
           -> ret $ Output n t (List.zip xs ts)
          _
           -> ret s

  goX env x
   | x' <- goX' env x
   , x /= x'
   = goX env x'
   | otherwise
   = x


  goX' env x
   | Just (PrimPack (PrimPackAll t), [n]) <- takePrimApps x
   , Nothing <- tryMeltType t
   = n

   | Just (PrimPack (PrimPackGet ix _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just v  <- fromPacked ix x'
   = v

   -- repacking projections
   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairFst ta tb)), [n]) <- takePrimApps x
   = primRepack env (PairT ta tb) [] ta n

   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairSnd ta tb)), [n]) <- takePrimApps x
   = primRepack env (PairT ta tb) [ta] tb n

   | Just (PrimProject (PrimProjectOptionIsSome tx), [n]) <- takePrimApps x
   = primRepack env (OptionT tx) [] BoolT n

   | Just (PrimUnsafe (PrimUnsafeOptionGet tx), [n]) <- takePrimApps x
   = primRepack env (OptionT tx) [BoolT] tx n

   | Just (PrimProject (PrimProjectSumIsRight ta tb), [n]) <- takePrimApps x
   = primRepack env (SumT ta tb) [] BoolT n

   | Just (PrimUnsafe (PrimUnsafeSumGetLeft ta tb), [n]) <- takePrimApps x
   = primRepack env (SumT ta tb) [BoolT] ta n

   | Just (PrimUnsafe (PrimUnsafeSumGetRight ta tb), [n]) <- takePrimApps x
   = primRepack env (SumT ta tb) [BoolT, ta] tb n

   | Just (PrimMinimal (Min.PrimStruct (Min.PrimStructGet f tf ts@(StructType fs))), [n]) <- takePrimApps x
   , (pre, _:_) <- List.span (/= f) (Map.keys fs)
   , tpre       <- List.take (length pre) (Map.elems fs)
   = primRepack env (StructT ts) tpre tf n

   -- repacking const
   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstPair ta tb)), [n1, n2]) <- takePrimApps x
   = primPackAll env (PairT ta tb) [n1, n2]

   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstSome tx)), [n]) <- takePrimApps x
   = primPackAll env (OptionT tx) [xTrue, n]

   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstLeft ta tb)), [n]) <- takePrimApps x
   = primPackAll env (SumT ta tb) [xFalse, n, xDefault tb]

   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstRight ta tb)), [n]) <- takePrimApps x
   = primPackAll env (SumT ta tb) [xTrue, xDefault ta, n]


   -- buffers
   | Just (PrimBuf (PrimBufPush tx), [nb, nx]) <- takePrimApps x
   , Just ts <- tryMeltType tx
   , tis     <- List.zip ts [0..]
   = primPackAll env (BufT tx)
   $ fmap (\(t, ix) -> primBufPush t (primPackGet ix (BufT tx) nb)
                                     (primPackGet ix tx        nx)) tis

   | Just (PrimBuf (PrimBufRead tx), [n]) <- takePrimApps x
   , Just ts <- tryMeltType tx
   , tis     <- List.zip ts [0..]
   = primPackAll env (ArrayT tx)
   $ fmap (\(t, ix) -> primBufRead t (primPackGet ix (BufT tx) n)) tis


   -- arrays
   | Just (PrimUnsafe (PrimUnsafeArrayCreate tx), [n]) <- takePrimApps x
   , Just ts <- tryMeltType tx
   = primPackAll env (ArrayT tx)
   $ fmap (primArrayCreate n) ts

   | Just (PrimProject (PrimProjectArrayLength tx), [n]) <- takePrimApps x
   , Just (t:_) <- tryMeltType tx
   = primArrayLength t (primPackGet 0 (ArrayT tx) n)

   | Just (PrimUnsafe (PrimUnsafeArrayIndex tx), [n, aix]) <- takePrimApps x
   , Just ts <- tryMeltType tx
   , tis     <- List.zip ts [0..]
   = primPackAll env tx
   $ fmap (\(t, ix) -> primArrayGet aix t (primPackGet ix (ArrayT tx) n)) tis

   | Just (PrimUpdate (PrimUpdateArrayPut tx), [na, aix, nv]) <- takePrimApps x
   , Just ts <- tryMeltType tx
   , tis     <- List.zip ts [0..]
   = primPackAll env (ArrayT tx)
   $ fmap (\(t, ix) -> primArrayPut aix t (primPackGet ix (ArrayT tx) na)
                                          (primPackGet ix         tx  nv)) tis

   | otherwise
   = x


  fromPacked ix x
   | XValue _ t v <- x
   , ts           <- meltType t
   , Just vs      <- meltValue v t
   , (t',v'):_    <- drop ix (List.zip ts vs)
   = Just (xValue t' v')

   | Just (PrimPack (PrimPackAll _), xs) <- takePrimApps x
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
   , Just x'  <- get env n
   = unpack' env x'

   | Just (PrimPack (PrimPackAll _), ns) <- takePrimApps x
   = concatMap (unpack' env) ns

   | otherwise
   = [x]


  -- Either lookup a name, or just return the value if it's already a constant.
  resolve env    (XVar   _ n)   = get env n
  resolve _   xx@(XValue _ _ _) = Just xx
  resolve _       _             = Nothing

  -- Lookup a name in the environment.
  get env n
   | (_,x'):_ <- filter ((==n).fst) env
   = Just x'
   | otherwise
   = Nothing
