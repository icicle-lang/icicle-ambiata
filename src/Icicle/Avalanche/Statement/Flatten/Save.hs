-- | Save the buffers inside accumulators by looping through
-- and calling KeepFactInHistory
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Flatten.Save (
    flattenSaveAccumulator
  ) where

import              Icicle.Avalanche.Statement.Flatten.Base

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat     as Flat
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Exp
import              Icicle.Common.Fresh

import              P

import qualified    Data.Map                        as Map
import              Data.Hashable                  (Hashable)

-- | Dig through the type and find any buffers of FactIdentifiers.
-- If there are any, create loops etc to mark them with KeepFactInHistory
flattenSaveAccumulator :: Hashable n => a -> Accumulator a n p -> FlatM a n
flattenSaveAccumulator a_fresh (Accumulator nm ty _)
 = do nm' <- freshPrefixBase $ nameBase nm
      n   <- flattenSave' a_fresh (XVar a_fresh nm') ty
      case n of
       Just st
        -> return $ Read nm' nm ty st
       Nothing
        -> return   mempty



-- | Dig through the type and find any buffers of FactIdentifiers.
-- Return Nothing if there is no Buf FactIdentifier.
flattenSave'
        :: (Monad m, Hashable n)
        => a
        -> Exp a n Flat.Prim
        -> ValType
        -> FreshT n m (Maybe (Statement a n Flat.Prim))
flattenSave' a_fresh xx ty
 = case ty of
    -- Base types with no nested buffers
    BoolT       -> no
    TimeT       -> no
    DoubleT     -> no
    IntT        -> no
    StringT     -> no
    UnitT       -> no
    ErrorT      -> no
    FactIdentifierT-> no
    -- Maps will not appear at this late stage
    MapT t u
     -> do s1 <- forArr (fpMapKeys t u `xApp` xx) t (go t)
           s2 <- forArr (fpMapVals t u `xApp` xx) u (go u)
           return (s1 <> s2)

    -- A buffer! Look inside and mark them as necessary.
    BufT n t 
     | FactIdentifierT <- t
     -> forBuf xx n t (yes . KeepFactInHistory)
     -- Allow for nested buffers: while we don't allow them now, they may be useful in the future
     | otherwise
     -> forBuf xx n t (go t)

    -- Look through the contents of the array for any Buf FactIdentifierT.
    ArrayT t
     -> forArr xx t (go t)

    -- The Option may contain a nested Buf FactIdentifierT, so try it
    OptionT t
     -> do s <- go t (fpOptionGet t `xApp` xx)
           case s of
            -- If there is no nested buf, we must be very careful to return Nothing
            -- Otherwise we might loop through this stuff for no reason.
            Nothing -> no
            Just s' -> yes $ If (fpIsSome t `xApp` xx) s' mempty

    PairT t u
     -> do s1 <- go t (fpFst t u `xApp` xx)
           s2 <- go u (fpSnd t u `xApp` xx)
           -- If either side contains something, this is something
           return (s1 <> s2)

    SumT t u
     -> do s1 <- go t (fpSumLeft  t u `xApp` xx)
           s2 <- go u (fpSumRight t u `xApp` xx)
           case (s1 <> s2) of
            Nothing
             -> no
            Just _ 
             -> yes
              $ If (fpIsRight t u `xApp` xx) (stmtOf s2) (stmtOf s1)

    StructT st
     -> mconcat <$> mapM (\(nm,t) -> go t (fpStructGet nm t st `xApp` xx))
                    (Map.toList (getStructType st))



 where
  no = return Nothing
  yes = return . Just

  go t x' = flattenSave' a_fresh x' t

  forBuf x n t f
   = do nm' <- fresh
        s <- forArr (xVar nm') t f
        case s of
         Nothing -> no
         Just s' -> yes
                  $ Let nm' (xPrim (Flat.PrimBuf $ Flat.PrimBufRead n t) `xApp` x) s'

  forArr x t f
   = do nm' <- fresh
        let ix = xPrim (Flat.PrimUnsafe $ Flat.PrimUnsafeArrayIndex t) `makeApps'` [x, xVar nm']
        f'  <- f ix
        case f' of
         Nothing -> no
         Just s -> yes
                 $ ForeachInts nm' xZero (xPrim (Flat.PrimProject $ Flat.PrimProjectArrayLength t) `xApp` x) s

  stmtOf = fromMaybe mempty

  -- Annotation plumbing
  makeApps' = makeApps a_fresh
  xVar      = XVar     a_fresh
  xPrim     = XPrim    a_fresh
  xValue    = XValue   a_fresh
  xApp      = XApp     a_fresh

  xZero     = xValue IntT (VInt 0)

  fpIsSome    t = xPrim (Flat.PrimProject (Flat.PrimProjectOptionIsSome t))
  fpOptionGet t = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeOptionGet t))

  fpIsRight  t u = xPrim (Flat.PrimProject (Flat.PrimProjectSumIsRight t u))
  fpSumLeft  t u = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeSumGetLeft  t u))
  fpSumRight t u = xPrim (Flat.PrimUnsafe (Flat.PrimUnsafeSumGetRight t u))

  fpFst t u     = xPrim (Flat.PrimMinimal $ Min.PrimPair $ Min.PrimPairFst t u)
  fpSnd t u     = xPrim (Flat.PrimMinimal $ Min.PrimPair $ Min.PrimPairSnd t u)

  fpStructGet f t st
    = xPrim (Flat.PrimMinimal $ Min.PrimStruct $ Min.PrimStructGet f t st)

  fpMapKeys k v = xPrim (Flat.PrimMap (Flat.PrimMapUnpackKeys   k v))
  fpMapVals k v = xPrim (Flat.PrimMap (Flat.PrimMapUnpackValues k v))

