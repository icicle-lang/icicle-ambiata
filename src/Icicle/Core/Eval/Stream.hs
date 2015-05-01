-- | Simple stream evaluation
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Stream (
      StreamValue
    , DatedStreamValue
    , StreamHeap 
    , RuntimeError (..)
    , eval
    ) where

import Icicle.Data
import Icicle.Data.DateTime
import Icicle.Core.Base
import Icicle.Core.Type
import Icicle.Core.Stream
import qualified    Icicle.Core.Eval.Exp    as XV -- eXpression eVal

import              P

import              Data.Either.Combinators
import              Data.List     (take)
import qualified    Data.Map as Map



-- | A stream value is just a list of values
--
-- TODO: attach a cookie to each stream value.
-- the cookie should be passed around unchanged as the stream is mapped, dropped if the value is dropped by a filter etc.
--
-- Reduce should then return list of used cookies; all input for fold and latest n for latest.
-- Actually reduce could create a new cookie of current reduce values so that could be used tomorrow as the new starting point.
-- That would basically be the latest input value's cookie.
-- Talk to Mark to make sure that's feasible.
--
type StreamValue n
 = [XV.Value n]

type DatedStreamValue n
 = [AsAt (XV.Value n)]

-- | A stream heap maps from names to stream values
type StreamHeap n
 = Map.Map (Name n) (StreamValue n)


-- | Things that can go wrong for stream evaluation
data RuntimeError n
 = RuntimeErrorExp          (XV.RuntimeError n)
 | RuntimeErrorNoSuchStream (Name n)
 | RuntimeErrorExpNotOfType (XV.Value n) ValType
 deriving (Show, Eq)


-- | Evaluate a stream.
-- We take the precomputation environment, the stream of concrete values,
-- and the heap with all preceding streams stored.
eval    :: Ord n
        => DateTime     -- Snapshot date for checking against windows
        -> XV.Heap     n
        -> DatedStreamValue n   -- Concrete inputs start with dates attached
        -> StreamHeap  n
        -> Stream n
        -> Either (RuntimeError n) (StreamValue n)
eval window_check xh concreteValues sh s
 = case s of
    -- Raw input is easy
    Source
     -> return $ fmap fact concreteValues

    -- Windowed input.
    -- The dates are assured to be increasing, so we could really use a takeWhile.dropWhile or something
    SourceWindowedDays window
     -> return $ fmap fact
               $ filter (\v -> withinWindow (time v) window_check window)
               $ concreteValues

    -- Transformers are slightly more involved
    STrans st x n
     -> do  -- First get the input source of the transformer
            sv <- maybeToRight  (RuntimeErrorNoSuchStream n)
                                (Map.lookup n sh)

            -- Now execute the transformer over those values
            evalTrans st x sv

 where
  -- Evaluate transform over given values.
  -- We don't need the stream heap any more
  evalTrans st x sv
   = case st of
      SFilter _
       -> filterM (evalFilt x) sv

      SMap _ _
       -> mapM    (applyX  x) sv

      -- Take first N elements
      STake _
       -> do    -- Evaluate worker expression with no arguments
                v <- evalX x
                case v of
                 -- It better be an int
                 -- otherwise, it's a type error
                 XV.VInt i
                  -> return $ take i sv
                 _
                  -> Left (RuntimeErrorExpNotOfType v IntT)


  -- Apply x to arg, if it's a bool we know whether to filter
  evalFilt x arg
   = do v <- applyX x arg
        case v of
         XV.VBool b
          -> return b
         _
          -> Left (RuntimeErrorExpNotOfType v BoolT)

  -- Evaluate expression with environment,
  -- raise to a stream error if it fails
  evalX
   = mapLeft RuntimeErrorExp
   . XV.eval xh

  -- Apply an expression to a value.
  -- First we need to evaluate the function to a value,
  -- then we can use XV.applyValues
  applyX fX argV
   = do fV <- evalX fX
        mapLeft RuntimeErrorExp
            $ XV.applyValues fV argV

