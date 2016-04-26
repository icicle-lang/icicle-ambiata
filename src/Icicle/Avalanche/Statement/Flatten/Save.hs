-- | Save the buffers inside accumulators by looping through
-- and calling KeepFactInHistory
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
module Icicle.Avalanche.Statement.Flatten.Save (
    flattenSaveAccumulator
  ) where

import              Icicle.Avalanche.Statement.Flatten.Base

import              Icicle.Avalanche.Statement.Statement
import qualified    Icicle.Avalanche.Prim.Flat     as Flat
import qualified    Icicle.Avalanche.Prim.Compounds as Flat

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
     -> do s1 <- forArr (mapKeys t u xx) t (go t)
           s2 <- forArr (mapVals t u xx) u (go u)
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
     -> do s <- go t (optionGet t xx)
           case s of
            -- If there is no nested buf, we must be very careful to return Nothing
            -- Otherwise we might loop through this stuff for no reason.
            Nothing -> no
            Just s' -> yes $ If (isSome t xx) s' mempty

    PairT t u
     -> do s1 <- go t (fstF t u xx)
           s2 <- go u (sndF t u xx)
           -- If either side contains something, this is something
           return (s1 <> s2)

    SumT t u
     -> do s1 <- go t (left  t u xx)
           s2 <- go u (right t u xx)
           case (s1 <> s2) of
            Nothing
             -> no
            Just _ 
             -> yes
              $ If (isRightF t u xx) (stmtOf s2) (stmtOf s1)

    StructT st
     -> mconcat <$> mapM (\(nm,t) -> go t (structGet nm t st xx))
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
                  $ Let nm' (bufRead n t x) s'

  forArr x t f
   = do nm' <- fresh
        let ix = arrIx t x (xVar nm')
        f'  <- f ix
        case f' of
         Nothing -> no
         Just s -> yes
                 $ ForeachInts ForeachStepUp nm' xZero (arrLen t x) s

  stmtOf = fromMaybe mempty

  -- Annotation plumbing
  xVar      = XVar     a_fresh
  xValue    = XValue   a_fresh
  xZero     = xValue IntT (VInt 0)

  Flat.FlatOps{..} = Flat.flatOps a_fresh


