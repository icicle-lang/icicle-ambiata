-- | Simple reduce evaluation
--
-- TODO: also compute required facts / lookbehind?
--       requires changing this and stream evaluation
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Reduce (
      RuntimeError
    , eval
    ) where

import              Icicle.BubbleGum

import              Icicle.Core.Type
import              Icicle.Core.Reduce
import qualified    Icicle.Core.Eval.Exp    as XV
import qualified    Icicle.Core.Eval.Stream as SV

import              P

import              Data.Either.Combinators
import qualified    Data.Map as Map


-- | Things that can go wrong for reduce evaluation
-- are actually the exact same as for streams.
type RuntimeError n
 = SV.RuntimeError n

-- | Evaluating a reduction also gives us the history of its inputs
type ReduceValue n =
 ( BubbleGumOutput n (XV.Value n)
 , XV.Value n )


-- | Evaluate a reduction.
-- We take the precomputation environment and the stream environment.
-- A single value is returned.
eval    :: Ord n
        => XV.Heap        n
        -> SV.StreamHeap  n
        -> Reduce n
        -> Either (RuntimeError n) (ReduceValue n)
eval xh sh r
 = case r of
    -- Fold over all stream data
    RFold _ _ k z n
     -> do  -- First get the stream
            sv <- maybeToRight  (SV.RuntimeErrorNoSuchStream n)
                                (Map.lookup n sh)
            -- Evaluate the zero with no arguments
            z' <- evalX z
            -- Perform the fold!
            v  <- foldM (apply2 k) z' (fmap snd $ fst sv)

            -- Check if the input is windowed
            let bg  | SV.Windowed _ <- snd sv
                    = BubbleGumFacts $ flavoursOfSource $ fst sv
                    | otherwise
                    = BubbleGumReduction v
            
            return (bg, v)

    -- Get most recent or last num elements
    RLatest _ num n
     -> do  -- First get the stream
            sv <- maybeToRight  (SV.RuntimeErrorNoSuchStream n)
                                (Map.lookup n sh)
            -- Evaluate the number
            num' <- evalX num
            -- It better be an Int
            case num' of
             XV.VInt i
              -> let sv' = latest i $ fst sv
                 in  return ( BubbleGumFacts $ flavoursOfSource sv'
                            , XV.VArray $ fmap snd sv' )
             _
              -> Left (SV.RuntimeErrorExpNotOfType num' IntT)

 where
  -- Evaluate expression with environment,
  -- raise to a stream error if it fails
  evalX
   = mapLeft SV.RuntimeErrorExp
   . XV.eval xh

  -- Apply expression to two value arguments.
  apply2 fX aV bV
   = do fV  <- evalX fX
        fa  <- appV fV aV
        fab <- appV fa bV
        return fab

  appV f a
   = mapLeft SV.RuntimeErrorExp
   $ XV.applyValues f a

  -- In real code we use a circular buffer, but here we can
  -- afford to keep everything in memory
  latest i vs
   = let len = length vs
     in  drop (len - i) vs

  flavoursOfSource
   = fmap (\(BubbleGumFact f _, _) -> f)

