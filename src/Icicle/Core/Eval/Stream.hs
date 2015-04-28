-- | Simple stream evaluation
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Stream (
      StreamValue
    , StreamHeap 
    , RuntimeError (..)
    , eval
    ) where

import Icicle.Core.Base
import Icicle.Core.Type
import Icicle.Core.Stream
import qualified    Icicle.Core.Eval.Exp    as XV -- eXpression eVal

import              P

import              Data.Either.Combinators
import              Data.List     (take)
import qualified    Data.Map as Map



-- | A stream value is just a list of values
type StreamValue n
 = [XV.Value n]

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
        => XV.Heap     n
        -> StreamValue n
        -> StreamHeap  n
        -> Stream n
        -> Either (RuntimeError n) (StreamValue n)
eval xh concreteValues sh s
 = case s of
    -- Raw input is easy
    Source
     -> return concreteValues

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

