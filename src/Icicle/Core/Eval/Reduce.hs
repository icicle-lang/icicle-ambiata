-- | Simple reduce evaluation
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Reduce (
      RuntimeError
    , eval
    ) where

import              Icicle.BubbleGum

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Value as V
import              Icicle.Core.Reduce
import qualified    Icicle.Common.Exp.Eval  as XV
import qualified    Icicle.Core.Eval.Exp    as XV
import qualified    Icicle.Core.Eval.Stream as SV
import              Icicle.Core.Exp.Prim

import              Icicle.Data

import              P

import              Data.Either.Combinators
import qualified    Data.Map as Map


-- | Things that can go wrong for reduce evaluation
-- are actually the exact same as for streams.
type RuntimeError n
 = SV.RuntimeError n

-- | Evaluating a reduction also gives us the history of its inputs
type ReduceValue n =
 ( BubbleGumOutput n BaseValue
 , BaseValue )


-- | Evaluate a reduction.
-- We take the precomputation environment and the stream environment.
-- A single value is returned.
-- The name of this reduction is used to produce the value history, or bubblegum
eval    :: Ord n
        => Name          n
        -> V.Heap      a n Prim
        -> SV.StreamHeap n
        -> Reduce      a n
        -> Either (RuntimeError a n) (ReduceValue n)
eval reduction_name xh sh r
 = case r of
    -- Fold over all stream data
    RFold _ _ k z n
     -> do  -- First get the stream
            sv <- maybeToRight  (SV.RuntimeErrorNoSuchStream n)
                                (Map.lookup n sh)
            -- Evaluate the zero with no arguments
            z' <- evalX z
            -- Perform the fold!
            v  <- foldM (apply2 k) z' (fmap (V.VBase . snd . fact) $ fst sv)

            v' <- case v of
                   V.VFun{}   -> Left (SV.RuntimeErrorExpNotBaseType v)
                   V.VBase v' -> return v'

            -- Check if the input is windowed;
            let bg  | SV.Windowed _ <- snd sv
                    -- since values drop off the start of the window, we can't just
                    -- store the end result of the fold
                    = BubbleGumFacts $ flavoursOfSource $ fst sv
                    | otherwise
                    -- but for an unwindowed reduction, we can incrementally compute
                    = BubbleGumReduction reduction_name v'

            return (bg, v')

 where
  -- Evaluate expression with environment,
  -- raise to a stream error if it fails
  evalX
   = mapLeft SV.RuntimeErrorExp
   . XV.eval XV.evalPrim xh

  -- Apply expression to two value arguments.
  apply2 fX aV bV
   = do fV  <- evalX fX
        fa  <- appV fV aV
        fab <- appV fa bV
        return fab

  appV f a
   = mapLeft SV.RuntimeErrorExp
   $ XV.applyValues XV.evalPrim f a

  -- In real code we use a circular buffer, but here we can
  -- afford to keep everything in memory
  latest i vs
   = let len = length vs
     in  drop (len - i) vs

  -- Get the history of some stream input
  flavoursOfSource
   = fmap (\(AsAt { fact = (BubbleGumFact f, _)}) -> f)

