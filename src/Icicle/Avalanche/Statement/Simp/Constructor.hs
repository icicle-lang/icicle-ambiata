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
   | x' <- goX' env x
   , x /= x'
   = goX env x'
   | otherwise
   = x

  goX' env x
   -- min
   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairFst _ _)), [n]) <- takePrimApps x
   , Just x'        <- resolve env n
   , Just (_,_,a,_) <- fromPair x'
   = a
   | Just (PrimMinimal (Min.PrimPair (Min.PrimPairSnd _ _)), [n]) <- takePrimApps x
   , Just x'        <- resolve env n
   , Just (_,_,_,b) <- fromPair x'
   = b

   | Just (PrimMinimal (Min.PrimStruct (Min.PrimStructGet f _ _)), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just v  <- fromStruct f x'
   = v

   -- safe projections
   | Just (PrimProject (PrimProjectArrayLength _), [XVar _ n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, _, a, _) <- fromZippedArray x'
   = xPrim (PrimProject $ PrimProjectArrayLength ta)
    `xApp` a

   | Just (PrimProject (PrimProjectArrayLength _), [XVar _ n]) <- takePrimApps x
   , Just x' <- get env n
   , Just (_, _, i, _, _) <- fromSummedArray x'
   = xPrim (PrimProject $ PrimProjectArrayLength BoolT)
    `xApp` i

   | Just (PrimProject (PrimProjectOptionIsSome _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,b,_) <- fromOption x'
   = b

   | Just (PrimProject (PrimProjectSumIsRight _ _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,i,_,_) <- fromSum x'
   = i

   -- unsafe projections
   | Just (PrimUnsafe (PrimUnsafeArrayIndex _), [XVar _ n, ix]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, tb, a, b) <- fromZippedArray x'
   = xPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair ta tb)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex ta)) `xApp` a `xApp` ix)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex tb)) `xApp` b `xApp` ix)

   | Just (PrimUnsafe (PrimUnsafeArrayIndex _), [XVar _ n, ix]) <- takePrimApps x
   , Just x' <- get env n
   , Just (ta, tb, i, a, b) <- fromSummedArray x'
   = xPrim (PrimPack $ PrimSumPack ta tb)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex BoolT)) `xApp` i `xApp` ix)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex ta))    `xApp` a `xApp` ix)
    `xApp` (xPrim (PrimUnsafe (PrimUnsafeArrayIndex tb))    `xApp` b `xApp` ix)

   | Just (PrimUnsafe (PrimUnsafeOptionGet _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,v) <- fromOption x'
   = v

   | Just (PrimUnsafe (PrimUnsafeSumGetLeft _ _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,_,a,_) <- fromSum x'
   = a

   | Just (PrimUnsafe (PrimUnsafeSumGetRight _ _), [n]) <- takePrimApps x
   , Just x' <- resolve env n
   , Just (_,_,_,_,b) <- fromSum x'
   = b

   -- * Update
   --   put (sum i a b) ~> sum (put i) (put a) (put b)
   | Just (PrimUpdate (PrimUpdateArrayPut _), [arr,ix,v]) <- takePrimApps x
   , Just arrx              <- resolve env arr
   , Just (ta, tb, i, a, b) <- fromSummedArray arrx
   , boool                  <- xPrim (PrimProject (PrimProjectSumIsRight ta tb)) `xApp` v
   , l                      <- xPrim (PrimUnsafe  (PrimUnsafeSumGetLeft  ta tb)) `xApp` v
   , r                      <- xPrim (PrimUnsafe  (PrimUnsafeSumGetRight ta tb)) `xApp` v
   , l'                     <- goX env l
   , r'                     <- goX env r
   = xPrim (PrimArray $ PrimArraySum ta tb)
   `xApp` (xPrim (PrimUpdate (PrimUpdateArrayPut BoolT)) `xApp` i `xApp` ix `xApp` boool)
   `xApp` (xPrim (PrimUpdate (PrimUpdateArrayPut ta))    `xApp` a `xApp` ix `xApp` l')
   `xApp` (xPrim (PrimUpdate (PrimUpdateArrayPut tb))    `xApp` b `xApp` ix `xApp` r')

   -- put (zip a b) ~> zip (put a) (put b)
   | Just (PrimUpdate (PrimUpdateArrayPut _), [arr,ix,v]) <- takePrimApps x
   , Just arrx           <- resolve env arr
   , Just (ta, tb, a, b) <- fromZippedArray arrx
   , f                   <- xPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairFst ta tb) `xApp` v
   , s                   <- xPrim (PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd ta tb) `xApp` v
   , f'                  <- goX env f
   , s'                  <- goX env s
   = xPrim (PrimArray $ PrimArrayZip ta tb)
   `xApp` (xPrim (PrimUpdate (PrimUpdateArrayPut ta)) `xApp` a `xApp` ix `xApp` f')
   `xApp` (xPrim (PrimUpdate (PrimUpdateArrayPut ta)) `xApp` b `xApp` ix `xApp` s')

   -- * Arrays
   --   unzip [e1,e2,...en] ~> ([fst e1, fst e2, ..., fst en ], [snd e1, snd e2, ..., snd en])
   | Just (PrimArray (PrimArrayUnzip _ _), [arr])       <- takePrimApps x
   , Just (XValue _ (ArrayT (PairT ta tb)) (VArray xs)) <- resolve env arr
   , Just (as, bs)                                      <- unzip' xs
   = xValue
      (PairT (ArrayT ta) (ArrayT tb))
      (VPair (VArray as) (VArray bs))

   -- unsum [e1..en] ~> ([isRight e1..isRight en], ([getLeft e1..getLeft en], [getRight e1..getRight en]))
   | Just (PrimArray (PrimArrayUnsum _ _), [arr])      <- takePrimApps x
   , Just (XValue _ (ArrayT (SumT ta tb)) (VArray xs)) <- resolve env arr
   , Just (is, as, bs)                                 <- unsum' ta tb xs
   = xValue
      (PairT (ArrayT BoolT) (PairT (ArrayT ta) (ArrayT tb)))
      (VPair (VArray is) (VPair (VArray as) (VArray bs)))

   -- * "Rewrite rules"
   --   unsum (sum i a b) ~> (i, (a, b))
   | Just (PrimArray (PrimArrayUnsum ta tb), [n])       <- takePrimApps x
   , Just arr                                           <- resolve env n
   , Just (PrimArray (PrimArraySum   _  _),  [i, a, b]) <- takePrimApps arr
   = xPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair (ArrayT BoolT) (PairT (ArrayT ta) (ArrayT tb)))
   `xApp` i
   `xApp` (xPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair (ArrayT ta) (ArrayT tb))
           `xApp` a `xApp` b)

   --   unzip (zip a b) ~> (a, b)
   | Just (PrimArray (PrimArrayUnzip ta tb), [n])    <- takePrimApps x
   , Just arr                                        <- resolve env n
   , Just (PrimArray (PrimArrayZip   _  _),  [a, b]) <- takePrimApps arr
   = xPrim (PrimMinimal $ Min.PrimConst $ Min.PrimConstPair (ArrayT ta) (ArrayT tb))
   `xApp` a `xApp` b

   | otherwise
   = x


  fromZippedArray x
   | Just (PrimArray (PrimArrayZip ta tb), [a, b]) <- takePrimApps x
   = Just (ta, tb, a, b)
   | otherwise
   = Nothing

  fromSummedArray x
   | Just (PrimArray (PrimArraySum ta tb), [i, a, b]) <- takePrimApps x
   = Just (ta, tb, i, a, b)
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

  unzip' []
    = return ([], [])
  unzip' (VPair a b : xs)
    = do (as, bs) <- unzip' xs
         return (a:as, b:bs)
  unzip' _
    = Nothing

  unsum' _ _ []
   = return ([], [], [])
  unsum' ta tb (VLeft a : xs)
   = do (is', as', bs') <- unsum' ta tb xs
        return (VBool False : is', a : as', defaultOfType tb : bs')
  unsum' ta tb (VRight b : xs)
   = do (is', as', bs') <- unsum' ta tb xs
        return (VBool True : is', defaultOfType ta : as', b : bs')
  unsum' _ _ _
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
