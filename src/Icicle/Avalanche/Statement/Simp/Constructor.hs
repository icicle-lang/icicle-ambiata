{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp.Constructor (
    constructor
  ) where

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


constructor :: Ord n => a -> Statement a n Prim -> Fresh n (Statement a n Prim)
constructor a_fresh statements
 = transformUDStmt goS emptyExpEnv statements
 where
  xApp       = XApp   a_fresh
  xPrim      = XPrim  a_fresh
  xValue     = XValue a_fresh
  xTrue      = xValue BoolT (VBool True)
  xFalse     = xValue BoolT (VBool False)
  xDefault t = xValue t (defaultOfType t)

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
          InitAccumulator (Accumulator n at vt x) ss
           -> ret $ InitAccumulator (Accumulator n at vt (go x)) ss
          Write n x
           -> ret $ Write n (go x)
          Push n x
           -> ret $ Push n (go x)
          Output n t xts
           | xs <- fmap (go . fst) xts
           , ts <- fmap snd xts
           -> ret $ Output n t (List.zip xs ts)
          _
           -> ret s

  goX env x
   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairFst _ _)), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,a,_) <- fromPair x'
   = a

   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairSnd _ _)), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,_,b) <- fromPair x'
   = b

   | Just (PrimMinimal (Min.PrimStruct (Min.PrimStructGet f _ _)), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just v  <- fromStruct f x'
   = v

   | Just (PrimProject (PrimProjectOptionIsSome _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,b,_) <- fromOption x'
   = b

   | Just (PrimUnsafe (PrimUnsafeOptionGet _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,v) <- fromOption x'
   = v

   | Just (PrimProject (PrimProjectSumIsRight _ _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,i,_,_) <- fromSum x'
   = i

   | Just (PrimUnsafe (PrimUnsafeSumGetLeft _ _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,_,a,_) <- fromSum x'
   = a

   | Just (PrimUnsafe (PrimUnsafeSumGetRight _ _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,_,_,b) <- fromSum x'
   = b

   | Just (PrimUnsafe (PrimUnsafeArrayIndex _), [XVar _ n, ix]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, tb, a, b) <- fromZippedArray x'
   = xPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta tb)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex ta)) `xApp` a `xApp` ix)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex tb)) `xApp` b `xApp` ix)

   | Just (PrimProject (PrimProjectArrayLength _), [XVar _ n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, _, a, _) <- fromZippedArray x'
   = xPrim (PrimProject $ PrimProjectArrayLength ta)
    `xApp` a

   | otherwise
   = x

  fromZippedArray x
   | Just (PrimArray (PrimArrayZip ta tb), [a, b]) <- takePrimApps x
   = Just (ta, tb, a, b)
   | otherwise
   = Nothing

  fromPair x
   | XValue _ (PairT ta tb) (VPair a b) <- x
   = Just (ta, tb, xValue ta a, xValue tb b)

   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstPair ta tb)), [a,b]) <- takePrimApps x
   = Just (ta, tb, a, b)

   | otherwise
   = Nothing

  fromStruct f x
   | XValue _ (StructT (StructType ts)) (VStruct vs) <- x
   = xValue <$> Map.lookup f ts <*> Map.lookup f vs

   | Just (PrimPack (PrimStructPack (StructType ts)), xs) <- takePrimApps x
   , fxs <- Map.fromList (List.zip (Map.keys ts) xs)
   = Map.lookup f fxs

   | otherwise
   = Nothing

  fromOption x
   | XValue _ (OptionT tv) VNone <- x
   = Just (tv, xFalse, xDefault tv)

   | XValue _ (OptionT tv) (VSome v) <- x
   = Just (tv, xTrue, xValue tv v)

   | Just (PrimPack (PrimOptionPack tv), [b, v]) <- takePrimApps x
   = Just (tv, b, v)

   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstSome tv)), [v]) <- takePrimApps x
   = Just (tv, xTrue, v)

   | otherwise
   = Nothing

  fromSum x
   | XValue _ (SumT ta tb) (VLeft a) <- x
   = Just (ta, tb, xFalse, xValue ta a, xDefault tb)

   | XValue _ (SumT ta tb) (VRight b) <- x
   = Just (ta, tb, xTrue, xDefault ta, xValue tb b)

   | Just (PrimPack (PrimSumPack ta tb), [i, a, b]) <- takePrimApps x
   = Just (ta, tb, i, a, b)

   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstLeft ta tb)), [a]) <- takePrimApps x
   = Just (ta, tb, xFalse, a, xDefault tb)

   | Just (PrimMinimal (Min.PrimConst (Min.PrimConstRight ta tb)), [b]) <- takePrimApps x
   = Just (ta, tb, xTrue, xDefault ta, b)

   | otherwise
   = Nothing

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
