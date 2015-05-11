-- | Simple stream evaluation
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Stream (
      StreamValue
    , DatedStreamValue
    , StreamHeap 
    , StreamWindow (..)
    , RuntimeError (..)
    , eval
    ) where

import              Icicle.BubbleGum

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Value as V
import              Icicle.Core.Stream
import qualified    Icicle.Common.Exp.Eval  as XV -- eXpression eVal
import qualified    Icicle.Core.Eval.Exp    as XV -- eXpression eVal
import              Icicle.Core.Exp.Prim

import              Icicle.Data
import              Icicle.Data.DateTime

import              P

import              Data.Either.Combinators
import qualified    Data.Map as Map



-- | A stream value is a list of values, but also the list of cookies or bubblegum.
-- This bubblegum is used to track the flow - which values are used by the computation,
-- and specifically which values will be needed by tomorrow's or next computation.
--
-- We also need to know whether the stream is windowed: if it is not windowed, any
-- reduction like sum can just start where it left off.
-- If it is windowed, we instead store the values needed to recompute, as values will drop off
-- the start of the window.
type StreamValue
 = ([(BubbleGumFact, BaseValue)], StreamWindow)

-- | Whether this stream is the result - directly or indirectly - of a windowed source
data StreamWindow = Windowed Int | UnWindowed
 deriving (Eq,Ord,Show)

-- | Right at the start, we need dates on the stream values.
-- These can be used by windowing functions or ignored.
-- Afterwards they are thrown away, but could still be included in the value itself.
type DatedStreamValue
 = [AsAt (BubbleGumFact, BaseValue)]


-- | A stream heap maps from names to stream values
type StreamHeap n
 = Map.Map (Name n) StreamValue


-- | Things that can go wrong for stream evaluation
data RuntimeError n
 = RuntimeErrorExp          (XV.RuntimeError n Prim)
 | RuntimeErrorNoSuchStream (Name n)
 | RuntimeErrorExpNotOfType (V.Value n Prim) ValType
 | RuntimeErrorExpNotBaseType (V.Value n Prim)
 deriving (Show, Eq)


-- | Evaluate a stream.
-- We take the precomputation environment, the stream of concrete values,
-- and the heap with all preceding streams stored.
eval    :: Ord n
        => DateTime             -- ^ Snapshot date for checking against windows
        -> V.Heap      n Prim   -- ^ The expression heap with precomputations
        -> DatedStreamValue     -- ^ Concrete inputs start with dates attached
        -> StreamHeap       n   -- ^ Any streams that have already been evaluated
        -> Stream           n   -- ^ Stream to evaluate
        -> Either (RuntimeError n) StreamValue
eval window_check xh concreteValues sh s
 = case s of
    -- Raw input is easy
    Source
     -> return (fmap streamvalue concreteValues, UnWindowed)

    -- Windowed input.
    -- The dates are assured to be increasing, so we could really use a takeWhile.dropWhile or something
    SourceWindowedDays window
     -> return ( fmap streamvalue
               $ filter (\v -> withinWindow (time v) window_check window)
               $ concreteValues

               , Windowed window)

    -- Transformers are slightly more involved
    STrans st x n
     -> do  -- First get the input source of the transformer
            sv <- maybeToRight  (RuntimeErrorNoSuchStream n)
                                (Map.lookup n sh)

            -- Now execute the transformer over those values
            sv' <- evalTrans st x (fst sv)
            -- Transformers preserve windows
            return (sv', snd sv)

 where
  streamvalue v
   = (fst $ fact v, VPair (snd $ fact v) (VDateTime $ time v))

  -- Evaluate transform over given values.
  -- We don't need the stream heap any more.
  --
  -- The stream values still have their bubblegum attached
  -- but that doesn't really matter here.
  -- We just need to dig down into the expression without
  -- losing the bubblegum.
  evalTrans st x sv
   = case st of
      SFilter _
       -> filterM (evalFilt x . snd) sv

      SMap _ _
       -> mapM    (applySnd x) sv

  -- Apply x to arg, if it's a bool we know whether to filter
  evalFilt x arg
   = do v <- applyX x arg
        case v of
         V.VBase (VBool b)
          -> return b
         _
          -> Left (RuntimeErrorExpNotOfType v BoolT)

  -- Evaluate expression with environment,
  -- raise to a stream error if it fails
  evalX
   = mapLeft RuntimeErrorExp
   . XV.eval XV.evalPrim xh

  -- Apply an expression to a value.
  -- First we need to evaluate the function to a value,
  -- then we can use XV.applyValues
  applyX fX argV
   = do fV <- evalX fX
        mapLeft RuntimeErrorExp
            $ XV.applyValues XV.evalPrim fV (V.VBase argV)

  -- Apply an expression to a stream value, keeping the bubblegum intact
  applySnd fX (bubble, argV)
   = do v' <- applyX fX argV
        case v' of
         V.VBase v'' -> return (bubble, v'')
         V.VFun{}    -> Left (RuntimeErrorExpNotBaseType v')

