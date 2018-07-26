{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 800
#if darwin_HOST_OS
-- Disable pattern checks on OS/X because goX' below hits a pathalogical case
-- in GHC and we don't want to wait for it on our dev machines.
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
#else
-- On the build bot we want to make sure the pattern matches are still safe but
-- we need to increase a few limits to allow GHC to do this.
--
--   https://ghc.haskell.org/trac/ghc/ticket/11822
--
{-# OPTIONS_GHC -fmax-pmcheck-iterations=10000000 #-}
#endif
#endif

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
import qualified    Data.Set  as Set
import qualified    Data.Map  as Map


-- | Simplify applied primitives.
--
constructor
  :: forall a n . Eq n
  => a
  -> Statement (Ann a n) n Prim
  -> FixT (Fresh n) (Statement (Ann a n) n Prim)
constructor a_fresh statements
 = transformUDStmt goS emptyExpEnv statements
 where
  xPrim      = XPrim  (a_fresh, Set.empty)
  xValue     = XValue (a_fresh, Set.empty)
  xTrue      = xValue BoolT (VBool True)
  xFalse     = xValue BoolT (VBool False)
  xDefault t = xValue t (defaultOfType t)
  xApp x1 x2
    = let vars1 = snd $ annotOfExp x1
          vars2 = snd $ annotOfExp x2
      in  XApp (a_fresh, vars1 <> vars2) x1 x2

  primAnd = primLogical  Min.PrimLogicalAnd
  primOr  = primLogical  Min.PrimLogicalOr

  primEq  = primRelation Min.PrimRelationEq
  primNe  = primRelation Min.PrimRelationNe
  primGt  = primRelation Min.PrimRelationGt
  primLt  = primRelation Min.PrimRelationLt

  primFold1 def append xs
   = case xs of
      []     -> def
      (y:[]) -> y
      (y:ys) -> y `append` primFold1 def append ys

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

  primArrayPutImmutable i t a v
   = xPrim (PrimArray (PrimArrayPutImmutable t)) `xApp` a `xApp` i `xApp` v

  primArrayPutMutable i t a v
   = xPrim (PrimArray (PrimArrayPutMutable t)) `xApp` a `xApp` i `xApp` v

  primArraySwap ix1 ix2 t a
   = xPrim (PrimArray (PrimArraySwap t)) `xApp` a `xApp` ix1 `xApp` ix2

  primArrayDel i t a
   = xPrim (PrimArray (PrimArrayDel t)) `xApp` a `xApp` i

  primUnpack ix t x
   = xPrim (PrimMelt (PrimMeltUnpack ix t)) `xApp` x

  primPack env t xs
   = foldl' (\f x -> f `xApp` x) (xPrim (PrimMelt (PrimMeltPack t))) (unpack env xs)

  primRepack env t skip take x
   = let start = length (concatMap meltType skip)
         end   = start + length (meltType take) - 1
     in primPack env take (fmap (\ix -> primUnpack ix t x) [start..end])


  -- | Melt constructors in statements
  goS :: ExpEnv (Ann a n) n Prim
      -> Statement (Ann a n) n Prim
      -> FixT (Fresh n) ( ExpEnv    (Ann a n) n Prim
                        , Statement (Ann a n) n Prim )
  goS env s
   = let ret s' = return (env, s')

         goWith x s'
          = do x' <- goX env x
               ret $! s' x'

     in  case s of
          If x t e
           -> goWith x $ \x' -> If x' t e
          Let n x ss
           -> do x' <- goX env x
                 let !env'
                        | pertinent x'
                        = Map.insert n x' env
                        | otherwise
                        = Map.delete n env
                 return (env', Let n x' ss)

          ForeachInts t n from to ss
           -> do from' <- goX env from
                 to'   <- goX env to
                 ret $ ForeachInts t n from' to' ss
          While t n nt end ss
           -> do end'  <- goX env end
                 ret $ While t n nt end' ss
          InitAccumulator (Accumulator n vt x) ss
           -> goWith x $ \x' -> InitAccumulator (Accumulator n vt x') ss
          Write n x
           -> goWith x $ \x' -> Write n x'
          Output n t xts
           -> do xs <- mapM (goX env . fst) xts
                 let ts = fmap snd xts
                 ret $ Output n t (List.zip xs ts)
          _
           -> ret s

  -- | Melt constructors in expressions
  goX :: Monad m
      => ExpEnv (Ann a n) n Prim
      -> Exp    (Ann a n) n Prim
      -> FixT m (Exp (Ann a n) n Prim)
  goX env x
   | Just prima <- takePrimApps x
   , Just x'    <- goX' env prima
   = progress x'
   | otherwise
   = return x

  goX' :: ExpEnv (Ann a n) n Prim
       -> (Prim, [Exp (Ann a n) n Prim])
       -> Maybe (Exp (Ann a n) n Prim)
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

   | (PrimMinimal (Min.PrimBuiltinFun (Min.PrimBuiltinMap (Min.PrimBuiltinKeys tk tv))), [n]) <- prima
   = Just $ primRepack env (MapT tk tv) [] (ArrayT tk) n

   | (PrimMinimal (Min.PrimBuiltinFun (Min.PrimBuiltinMap (Min.PrimBuiltinVals tk tv))), [n]) <- prima
   = Just $ primRepack env (MapT tk tv) [ArrayT tk] (ArrayT tv) n

   | (PrimProject (PrimProjectOptionIsSome tx), [n]) <- prima
   = Just $ primRepack env (OptionT tx) [] BoolT n

   | (PrimUnsafe (PrimUnsafeOptionGet tx), [n]) <- prima
   = Just $ primRepack env (OptionT tx) [BoolT] tx n

   | (PrimProject (PrimProjectSumIsRight ta tb), [n]) <- prima
   , ErrorT <- ta
   = let err   = primRepack env (SumT ta tb) [] ErrorT n
         valOk = xValue ErrorT $ VError ExceptNotAnError
         eq    = xPrim $ PrimMinimal $ Min.PrimRelation Min.PrimRelationEq ErrorT
     in  Just (eq `xApp` err `xApp` valOk)

   | (PrimProject (PrimProjectSumIsRight ta tb), [n]) <- prima
   = Just $ primRepack env (SumT ta tb) [] BoolT n

   | (PrimUnsafe (PrimUnsafeSumGetLeft ta tb), [n]) <- prima
   , ErrorT <- ta
   = Just $ primRepack env (SumT ta tb) [] ta n

   | (PrimUnsafe (PrimUnsafeSumGetLeft ta tb), [n]) <- prima
   = Just $ primRepack env (SumT ta tb) [BoolT] ta n

   | (PrimUnsafe (PrimUnsafeSumGetRight ta tb), [n]) <- prima
   , ErrorT <- ta
   = Just $ primRepack env (SumT ta tb) [ta] tb n

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
   , ErrorT <- ta
   = Just $ primPack env (SumT ta tb) [n, xDefault tb]

   | (PrimMinimal (Min.PrimConst (Min.PrimConstLeft ta tb)), [n]) <- prima
   = Just $ primPack env (SumT ta tb) [xFalse, n, xDefault tb]

   | (PrimMinimal (Min.PrimConst (Min.PrimConstRight ta tb)), [n]) <- prima
   , ErrorT <- ta
   = Just $ primPack env (SumT ta tb) [xDefault ta, n]

   | (PrimMinimal (Min.PrimConst (Min.PrimConstRight ta tb)), [n]) <- prima
   = Just $ primPack env (SumT ta tb) [xTrue, xDefault ta, n]


   -- buffers
   | (PrimBuf (PrimBufPush i tx), [nb, nx]) <- prima
   , Just tis <- withIndex tryMeltType tx
   = Just $ primPack env (BufT i tx)
   $ fmap (\(t, ix) -> primBufPush i t (primUnpack ix (BufT i tx) nb)
                                       (primUnpack ix tx          nx)) tis

   | (PrimBuf (PrimBufRead i tx), [n]) <- prima
   , Just tis <- withIndex tryMeltType tx
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
   , Just tis <- withIndex tryMeltType tx
   = Just $ primPack env tx
   $ fmap (\(t, ix) -> primArrayGet aix t (primUnpack ix (ArrayT tx) n)) tis

   | (PrimArray (PrimArrayPutImmutable tx), [na, aix, nv]) <- prima
   , Just tis <- withIndex tryMeltType tx
   = Just $ primPack env (ArrayT tx)
   $ fmap (\(t, ix) -> primArrayPutImmutable aix t (primUnpack ix (ArrayT tx) na)
                                                   (primUnpack ix         tx  nv)) tis

   | (PrimArray (PrimArrayPutMutable tx), [na, aix, nv]) <- prima
   , Just tis <- withIndex tryMeltType tx
   = Just $ primPack env (ArrayT tx)
   $ fmap (\(t, ix) -> primArrayPutMutable aix t (primUnpack ix (ArrayT tx) na)
                                                 (primUnpack ix         tx  nv)) tis

   | (PrimArray (PrimArraySwap tx), [na, ix1, ix2]) <- prima
   , Just tis <- withIndex tryMeltType tx
   = Just $ primPack env (ArrayT tx)
   $ fmap (\(t, ix) -> primArraySwap ix1 ix2 t (primUnpack ix (ArrayT tx) na))
     tis

   | (PrimArray (PrimArrayDel tx), [na, i]) <- prima
   , Just tis <- withIndex tryMeltType tx
   = Just $ primPack env (ArrayT tx)
   $ fmap (\(t, ix) -> primArrayDel i t (primUnpack ix (ArrayT tx) na))
     tis

   | (PrimMinimal (Min.PrimRelation op t), [nx, ny]) <- prima
   , melts <- meltLogical t
   , melts /= [MeltRep t]
   = let tis = List.zip melts [0..]
         with0 p tvi = withPrim p t nx ny tvi
         mk = makeComparisons with0 tis

         -- init `join` prim x0 y0 `join` prim x1 y1 `join` ...
         simple init0 join0 prim0
            = primFold1 init0 join0
            $ fmap (withPrim prim0 t nx ny)
            $ fmap (first repOfMelt) tis
     in Just $ case op of
          -- True && x0 == y0 && x1 == y1 && ...
          Min.PrimRelationEq -> simple xTrue  primAnd  primEq
          -- False || x0 /= y0 || x1 /= y1 || ...
          Min.PrimRelationNe -> simple xFalse primOr   primNe

          -- (x0,x1) < (y0,y1)
          -- ==>
          -- > case compare x0 y0 of
          -- >  LT -> True
          -- >  EQ -> case compare x1 y1 of
          -- >         LT -> True
          -- >         EQ -> False
          -- >         GT -> False
          -- >  GT -> False
          Min.PrimRelationLt -> mk xTrue  xFalse xFalse
          Min.PrimRelationLe -> mk xTrue  xTrue  xFalse
          Min.PrimRelationGt -> mk xFalse xFalse xTrue
          Min.PrimRelationGe -> mk xFalse xTrue  xTrue

   | otherwise
   = Nothing

  withPrim p t x y (tv, i)
   = p tv (primUnpack i t x) (primUnpack i t y)

  withIndex f t
   = fmap (flip List.zip [0..]) (f t)

  -- Lexicographic ordering on melted tuples: start by comparing the firsts, and if they are
  -- greater or lesser, use "gt" or "lt".
  -- If the firsts are equal, move on to the seconds.
  -- Once you reach the end, you know all are equal, so use "eq".
  --
  -- > case compare x0 y0 of
  -- >  LT -> lt
  -- >  EQ -> case compare x1 y1 of
  -- >         LT -> lt
  -- >         EQ -> eq
  -- >         GT -> gt
  -- >  GT -> gt
  --
  -- However, because we don't have cases and the comparison primitive isn't exposed, this becomes:
  --
  -- > (x0 <  y0 && lt) ||
  -- > (x0 == y0 &&
  -- >   (
  -- >    (x1 <  y1 && lt) ||
  -- >    (x1 == y1 && eq) ||
  -- >    (x1 >  y1 && gt)
  -- >   )) ||
  -- > (x0 >  y0 && gt)
  --
  -- Pretty terrible.
  --
  makeComparisons with0 tis0 lt eq gt
   = let go [] = eq

         go ((MeltRep t, i) : tis) =
          (with0 primLt (t,i) `primAnd` lt) `primOr`
          (with0 primEq (t,i) `primAnd` go tis) `primOr`
          (with0 primGt (t,i) `primAnd` gt)

         -- Convert the faked error tag to an explicit boolean tag using (== NotAnError)
         go ((MeltTagSumError,i) : tis) =
          let tis'      = (MeltRep ErrorT, i) : tis
              tag       = xValue ErrorT $ VError ExceptNotAnError
              withTag p = with0 (\_ a b -> p BoolT (primEq ErrorT a tag) (primEq ErrorT b tag)) (ErrorT, i)
          in (withTag primLt `primAnd` lt) `primOr`
             (withTag primEq `primAnd` go tis') `primOr`
             (withTag primGt `primAnd` gt)
     in go tis0


  -- | Unpack a packed value
  fromPacked :: Int -> Exp (Ann a n) n Prim -> Maybe (Exp (Ann a n) n Prim)
  fromPacked ix x
   | XValue _ t v <- x
   , ts           <- meltType t
   , Just vs      <- meltValue v t
   , (t',v'):_    <- drop ix (List.zip ts vs)
   = Just (xValue t' v')

   | Just (PrimMelt (PrimMeltPack _), xs) <- takePrimApps x
   , (xx:_)                               <- drop ix xs
   = Just xx

   | otherwise
   = Nothing


  -- | Unpack values
  unpack :: ExpEnv (Ann a n) n Prim -> [Exp (Ann a n) n Prim] -> [Exp (Ann a n) n Prim]
  unpack env xs
   = concatMap (unpack1 env) xs

  -- | Unpack a value
  unpack1 :: ExpEnv (Ann a n) n Prim -> Exp (Ann a n) n Prim -> [Exp (Ann a n) n Prim]
  unpack1 env x
   | XValue _ t v <- x
   , Just ts      <- tryMeltType t
   , Just vs      <- meltValue v t
   = List.zipWith xValue ts vs

   | XVar _ n <- x
   , Just x'  <- getFromEnv env n
   = unpack1 env x'

   | Just (PrimMelt (PrimMeltPack _), ns) <- takePrimApps x
   = concatMap (unpack1 env) ns

   | otherwise
   = [x]

  -- | Check if a transformed expression is worth keeping in the environment.
  -- If we remember this, will it allow a melt/unmelt transform later?
  pertinent :: Exp (Ann a n) n Prim -> Bool
  pertinent x
   | XValue{} <- x
   = True
   | XVar{} <- x
   = True
   | Just (PrimMelt{},_) <- takePrimApps x
   = True
   | otherwise
   = False


-- Either lookup a name, or just return the value if it's already a constant.
resolve
  :: Eq n
  => ExpEnv a n p
  -> Exp    a n p
  -> Maybe (Exp a n p)
resolve env    (XVar   _ n)   = getFromEnv env n
resolve _   xx@(XValue _ _ _) = Just xx
resolve _       _             = Nothing

