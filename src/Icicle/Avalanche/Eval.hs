-- | Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Eval (
    evalProgram
  ) where

import              Icicle.BubbleGum

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Value
import qualified    Icicle.Common.Exp as XV

import              Icicle.Data

import              Icicle.Avalanche.Program

import              P
import qualified    Data.Map    as Map
import              Data.List   (take)

type AccumulatorHeap n
 = Map.Map (Name n) ([BubbleGumFact], AccumulatorValue)

data AccumulatorValue
 = AVFold BaseValue
 | AVLatest Int [BaseValue]

data RuntimeError n p
 = RuntimeErrorNoAccumulator (Name n)

updateOrPush
        :: Ord n
        => AccumulatorHeap n
        -> Name n
        -> BubbleGumFact
        -> BaseValue
        -> Either (RuntimeError n p) (AccumulatorHeap n)

updateOrPush heap n bg v
 = do   v' <- maybeToRight (RuntimeErrorNoAccumulator n)
                           (Map.lookup n heap)
        case v' of
         (bgs, AVFold _)
          -> return
           $ Map.insert n (bg : bgs, AVFold v) heap
         (bgs, AVLatest num vs)
          -> return 
           $ Map.insert n
           ( take num (bg : bgs)
           , AVLatest num (take num (v : vs)) ) heap


evalProgram
        :: Ord n
        => XV.EvalPrim n p
        -> DateTime
        -> [AsAt (BubbleGumFact, BaseValue)]
        -> Program n p
        -> Either (RuntimeError n p) BaseValue
evalProgram evalPrim now values p
 = do   pres <- evalExps Map.empty  (precomps p)
        
        accs <- mapM (initAcc pres) (accums   p)

        accs' <- foldM (evalLoop (loop p)) (accs, pres) values

        posts <- evalExps (pres <> accs') (postcomps p)

        evalExp posts (returns p)

        extractBubbleGum accs'


evalLoop
        :: Ord n
        => EvalPrim n p
        -> Loop n p
        -> (AccumulatorHeap n, XV.Heap n p)
        -> AsAt (BubbleGumFact, BaseValue)
        -> Either (RuntimeError n p) (AccumulatorHeap n, XV.Heap n p)
evalLoop evalPrim  loop (ah, xh) input
 = ...
