-- | Evaluate Flat primitives
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Prim.Eval (
      evalPrim
    , meltValue
    , unmeltValue
    ) where

import Icicle.Common.Base
import Icicle.Common.Exp.Eval
import Icicle.Common.Type
import Icicle.Common.Value
import Icicle.Avalanche.Prim.Flat

import              P
import              Data.List (lookup, zip, zipWith)

import qualified    Data.List as List
import qualified    Data.Map  as Map
import qualified    Icicle.Common.Exp.Prim.Eval as Min

evalPrim :: Ord n => EvalPrim a n Prim
evalPrim p vs
 = case p of
     PrimMinimal m
      -> Min.evalPrim m p vs

     PrimProject (PrimProjectArrayLength _)
      | [VBase (VArray var)]    <- vs
      -> return $ VBase $ VInt $ length var
      | otherwise
      -> primError

     PrimProject (PrimProjectOptionIsSome _)
      | [VBase VNone]  <- vs
      -> return $ VBase $ VBool False
      | [VBase (VSome _)]  <- vs
      -> return $ VBase $ VBool True
      | otherwise
      -> primError

     PrimProject (PrimProjectSumIsRight _ _)
      | [VBase (VLeft _)]  <- vs
      -> return $ VBase $ VBool False
      | [VBase (VRight _)]  <- vs
      -> return $ VBase $ VBool True
      | otherwise
      -> primError

     PrimBuf (PrimBufMake _)
      | [VBase (VInt i)] <- vs
      -> return . VBase . VBuf i $ []
      | otherwise
      -> primError

     PrimBuf (PrimBufPush _)
      | [VBase (VBuf i as), VBase e] <- vs
      -> return . VBase . VBuf i
      $  circ i e as
      | otherwise
      -> primError

     PrimBuf (PrimBufRead _)
      | [VBase (VBuf _ as)] <- vs
      -> return . VBase . VArray $ as
      | otherwise
      -> primError

     -- TODO: give better errors here - at least that an unsafe went wrong
     PrimUnsafe (PrimUnsafeArrayIndex _)
      | [VBase (VArray var), VBase (VInt ix)]  <- vs
      , Just v <- lookup ix (zip [0..] var)
      -> return $ VBase $ v
      | otherwise
      -> primError

     -- Create an uninitialised array.
     -- We have nothing to put in there, so we might as well just
     -- fill it up with units.
     PrimUnsafe (PrimUnsafeArrayCreate t)
      | [VBase (VInt sz)]  <- vs
      -> return $ VBase $ VArray $ [defaultOfType t | _ <- [1..sz]]
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeOptionGet t)
      | [VBase (VSome v)]  <- vs
      -> return $ VBase v
      | [VBase VNone]      <- vs
      -> return $ VBase (defaultOfType t)
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeSumGetLeft t _)
      | [VBase (VLeft v)]  <- vs
      -> return $ VBase v
      | [VBase (VRight _)] <- vs
      -> return $ VBase (defaultOfType t)
      | otherwise
      -> primError

     PrimUnsafe (PrimUnsafeSumGetRight _ t)
      | [VBase (VRight v)] <- vs
      -> return $ VBase v
      | [VBase (VLeft _)]  <- vs
      -> return $ VBase (defaultOfType t)
      | otherwise
      -> primError


     PrimUpdate (PrimUpdateArrayPut _)
      | [VBase (VArray varr), VBase (VInt ix), VBase v]  <- vs
      -> return $ VBase
       $ VArray [ if i == ix then v else u
                | (i,u) <- zip [0..] varr ]
      | otherwise
      -> primError


     PrimArray (PrimArrayZip _ _)
      | [VBase (VArray arr1), VBase (VArray arr2)]  <- vs
      -> return $ VBase
       $ VArray (zipWith VPair arr1 arr2)
      | otherwise
      -> primError

     PrimArray (PrimArrayUnzip _ _)
      | [VBase (VArray arr)] <- vs
      -> do (arr1, arr2)     <- foldM uz mempty arr
            return . VBase $ VPair (VArray $ reverse arr1)
                                   (VArray $ reverse arr2)
      | otherwise
      -> primError

     PrimArray (PrimArraySum _ _)
      | [VBase (VArray arr), VBase (VArray arr1), VBase (VArray arr2)]  <- vs
      -> do arr' <- zipWithM pick arr (zip arr1 arr2)
            return . VBase . VArray $ arr'
      | otherwise
      -> primError

     PrimArray (PrimArrayUnsum tl tr)
      | [VBase (VArray arr)]   <- vs
      -> do (arr0, arr1, arr2) <- foldM (us tl tr) mempty arr
            return . VBase $ VPair (VArray $ reverse arr0)
                                   (VPair (VArray $ reverse arr1)
                                           (VArray $ reverse arr2))
      | otherwise
      -> primError


     PrimPack (PrimPackAll t)
      | Just vs' <- traverse unpack vs
      , Just v   <- unmeltValue vs' t
      -> return (VBase v)
      | otherwise
      -> primError

     PrimPack (PrimPackGet ix t)
      | [VBase v]   <- vs
      , Just (v':_) <- fmap (drop ix) (meltValue v t)
      -> return (VBase v')
      | otherwise
      -> primError


     PrimMap (PrimMapPack _ _)
      | [VBase (VArray ks), VBase (VArray vals)] <- vs
      -> return $ VBase $ VMap $ Map.fromList $ List.zip ks vals
      | otherwise
      -> primError
     PrimMap (PrimMapUnpackKeys _ _)
      | [VBase (VMap m)] <- vs
      -> return $ VBase $ VArray $ Map.keys m
      | otherwise
      -> primError
     PrimMap (PrimMapUnpackValues _ _)
      | [VBase (VMap m)] <- vs
      -> return $ VBase $ VArray $ Map.elems m
      | otherwise
      -> primError
 where
  circ n x xs
   | length xs < n
   = xs <> [x]
   | otherwise
   = List.drop 1 (xs <> [x])

  uz (fs, ss) (VPair f s)
   = return (f     : fs, s     : ss)
  uz _ _
   = primError

  us _ tr (bs, ls, rs) (VLeft l)
   = return ( VBool  False     : bs
            , l                : ls
            , defaultOfType tr : rs )
  us tl _ (bs, ls, rs) (VRight r)
   = return ( VBool  True      : bs
            , defaultOfType tl : ls
            , r                : rs )
  us _ _ _ _
   = primError

  primError
   = Left $ RuntimeErrorPrimBadArgs p vs

  pick (VBool b) (l,r)
   = return $ if b then VRight r else VLeft l
  pick _ _
   = primError

  unpack (VBase x)    = Just x
  unpack (VFun _ _ _) = Nothing

------------------------------------------------------------------------

meltValue :: BaseValue -> ValType -> Maybe [BaseValue]
meltValue v t
 = let apcat x y = (<>) <$> x <*> y
   in case v of
     VUnit{}     -> Just [v]
     VInt{}      -> Just [v]
     VDouble{}   -> Just [v]
     VBool{}     -> Just [v]
     VDateTime{} -> Just [v]
     VString{}   -> Just [v]
     VError{}    -> Just [v]
     VMap{}      -> Just [v]

     VArray vs
      | Nothing <- tryMeltType t
      -> Just [v]

      | [] <- vs
      , ts <- meltType t
      -> Just (List.replicate (length ts) (VArray []))

      | ArrayT ta <- t
      -> do vss <- traverse (\vv -> meltValue vv ta) vs
            Just (fmap VArray (List.transpose vss))

      | otherwise
      -> Nothing

     VBuf i vs
      | Nothing <- tryMeltType t
      -> Just [v]

      | [] <- vs
      , ts <- meltType t
      -> Just (List.replicate (length ts) (VBuf i []))

      | BufT ta <- t
      -> do vss <- traverse (\vv -> meltValue vv ta) vs
            Just (fmap (VBuf i) (List.transpose vss))

      | otherwise
      -> Nothing

     VPair a b
      | PairT ta tb <- t
      -> meltValue a ta `apcat` meltValue b tb

      | otherwise
      -> Nothing

     VLeft a
      | SumT ta tb <- t
      -> pure [VBool False] `apcat` meltValue a ta `apcat` meltValue (defaultOfType tb) tb

      | otherwise
      -> Nothing

     VRight b
      | SumT ta tb <- t
      -> pure [VBool True] `apcat` meltValue (defaultOfType ta) ta `apcat` meltValue b tb

      | otherwise
      -> Nothing

     VNone
      | OptionT tv <- t
      -> pure [VBool False] `apcat` meltValue (defaultOfType tv) tv

      | otherwise
      -> Nothing

     VSome x
      | OptionT tx <- t
      -> pure [VBool True] `apcat` meltValue x tx

      | otherwise
      -> Nothing

     VStruct fvs
      | StructT (StructType fs) <- t
      , Map.null fs
      -> pure [VUnit]

      | StructT ts <- t
      , fts        <- Map.toList (getStructType ts)
      -> let go (nf,tf) = do
               fv <- Map.lookup nf fvs
               meltValue fv tf
         in concat <$> traverse go fts

      | otherwise
      -> Nothing

------------------------------------------------------------------------

unmeltValue :: [BaseValue] -> ValType -> Maybe BaseValue
unmeltValue vs t
 = case unmeltValue' vs t of
     Just (v, [])   -> Just v
     Nothing        -> Nothing

     -- if we still have values left over
     -- after unmelting, it's a type error
     Just (_v, _xs) -> Nothing

unmeltValue' :: [BaseValue] -> ValType -> Maybe (BaseValue, [BaseValue])
unmeltValue' vs0 t
 = case (vs0, t) of
     -- value was not properly melted in the first place
     (VPair{}:_,   _) -> Nothing
     (VNone{}:_,   _) -> Nothing
     (VSome{}:_,   _) -> Nothing
     (VLeft{}:_,   _) -> Nothing
     (VRight{}:_,  _) -> Nothing
     (VStruct{}:_, _) -> Nothing

     (v:vs, UnitT{})     -> Just (v, vs)
     (v:vs, IntT{})      -> Just (v, vs)
     (v:vs, DoubleT{})   -> Just (v, vs)
     (v:vs, BoolT{})     -> Just (v, vs)
     (v:vs, DateTimeT{}) -> Just (v, vs)
     (v:vs, StringT{})   -> Just (v, vs)
     (v:vs, MapT{})      -> Just (v, vs)
     (v:vs, ErrorT{})    -> Just (v, vs)

     (v:vs, ArrayT ta)
      | Just ts <- tryMeltType ta
      -> do let n = length ts
            xss <- List.transpose <$> traverse unArray (List.take n vs0)
            xs  <- traverse (\xs -> unmeltValue xs ta) xss
            Just (VArray xs, List.drop n vs0)

      | otherwise
      -> Just (v, vs)

     (v:vs, BufT ta)
      | Just ts <- tryMeltType ta
      -> do let n = length ts
            i   <- unBufLen v
            xss <- List.transpose <$> traverse unBuf (List.take n vs0)
            xs  <- traverse (\xs -> unmeltValue xs ta) xss
            Just (VBuf i xs, List.drop n vs0)

      | otherwise
      -> Just (v, vs)

     (_, PairT ta tb)
      -> do (a, vs1) <- unmeltValue' vs0 ta
            (b, vs2) <- unmeltValue' vs1 tb
            Just (VPair a b, vs2)

     (_, SumT ta tb)
      -> do (i, vs1) <- unmeltValue' vs0 BoolT
            (a, vs2) <- unmeltValue' vs1 ta
            (b, vs3) <- unmeltValue' vs2 tb
            case i of
              VBool False -> Just (VLeft  a, vs3)
              VBool True  -> Just (VRight b, vs3)
              _           -> Nothing

     (_, OptionT tx)
      -> do (b, vs1) <- unmeltValue' vs0 BoolT
            (x, vs2) <- unmeltValue' vs1 tx
            case b of
              VBool False -> Just (VNone,   vs2)
              VBool True  -> Just (VSome x, vs2)
              _           -> Nothing

     (VUnit:vs, StructT (StructType fs))
      | Map.null fs
      -> Just (VStruct Map.empty, vs)

     (_, StructT (StructType ts))
      -> do let go (acc, vs1) ft = do
                  (fv, vs2) <- unmeltValue' vs1 ft
                  return (acc <> [fv], vs2)

            (acc, vs3) <- foldM go ([], vs0) (Map.elems ts)
            let fvs = Map.fromList $ List.zip (Map.keys ts) acc
            Just (VStruct fvs, vs3)

     ([], _)
      -> Nothing

unArray :: BaseValue -> Maybe [BaseValue]
unArray (VArray vs) = Just vs
unArray _           = Nothing

unBuf :: BaseValue -> Maybe [BaseValue]
unBuf (VBuf _ vs) = Just vs
unBuf _           = Nothing

unBufLen :: BaseValue -> Maybe Int
unBufLen (VBuf i _) = Just i
unBufLen _          = Nothing
