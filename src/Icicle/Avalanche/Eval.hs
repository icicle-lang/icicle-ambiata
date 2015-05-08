-- | Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Eval (
    evalProgram
  ) where

import              Icicle.Avalanche.Program

import              Icicle.BubbleGum

import              Icicle.Common.Base
import              Icicle.Common.Value
import qualified    Icicle.Common.Exp as XV

import              Icicle.Data.DateTime
import              Icicle.Data         (AsAt(..))

import              P

import              Data.Either.Combinators
import              Data.List   (take, reverse, sort)
import qualified    Data.Map    as Map


type AccumulatorHeap n
 = Map.Map (Name n) ([BubbleGumFact], AccumulatorValue)

data AccumulatorValue
 = AVFold Bool BaseValue
 | AVLatest Int [BaseValue]
 deriving (Eq, Ord, Show)


data RuntimeError n p
 = RuntimeErrorNoAccumulator (Name n)
 | RuntimeErrorPre           (XV.RuntimeError n p)
 | RuntimeErrorAccumulator   (XV.RuntimeError n p)
 | RuntimeErrorLoop          (XV.RuntimeError n p)
 | RuntimeErrorPost          (XV.RuntimeError n p)
 | RuntimeErrorReturn        (XV.RuntimeError n p)
 | RuntimeErrorNotBaseValue  (Value n p)
 | RuntimeErrorAccumulatorLatestNotInt  BaseValue
 deriving (Eq, Show)


baseValue :: Value n p -> Either (RuntimeError n p) BaseValue
baseValue v
 = getBaseValue (RuntimeErrorNotBaseValue v) v

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
         (bgs, AVFold windowed _)
          -> return
           $ Map.insert n (bg : bgs, AVFold windowed v) heap
         (bgs, AVLatest num vs)
          -> return 
           $ Map.insert n
           ( take num (bg : bgs)
           , AVLatest num (take num (v : vs)) ) heap


updateHeapFromAccs
        :: Ord n
        => Heap n p
        -> AccumulatorHeap n
        -> Heap n p

updateHeapFromAccs env accs
 = Map.foldWithKey upd env accs
 where
  upd n (_,v) e
   = case v of
      AVFold _ v'
       -> Map.insert n (VBase v') e
      AVLatest _ vs
       -> Map.insert n (VBase $ VArray $ reverse vs) e


bubbleGumOutputOfAccumulatorHeap
        :: Ord n
        => AccumulatorHeap n
        -> [BubbleGumOutput n (BaseValue)]

bubbleGumOutputOfAccumulatorHeap acc
 = fmap mk
 $ Map.toList acc
 where
  mk (n, (_, AVFold False v))
   = BubbleGumReduction n v
  mk (_, (bgs, AVFold True _))
   = BubbleGumFacts $ sort $ fmap flav bgs
  mk (_, (bgs, AVLatest _ _))
   = BubbleGumFacts $ sort $ fmap flav bgs
  
  flav (BubbleGumFact f) = f


evalProgram
        :: Ord n
        => XV.EvalPrim n p
        -> DateTime
        -> [AsAt (BubbleGumFact, BaseValue)]
        -> Program n p
        -> Either (RuntimeError n p) ([BubbleGumOutput n BaseValue], BaseValue)

evalProgram evalPrim now values p
 = do   pres  <- mapLeft RuntimeErrorPre
               $ XV.evalExps evalPrim Map.empty  (precomps p)
        
        accs  <- Map.fromList <$> mapM (initAcc evalPrim pres) (accums   p)

        let pres' = updateHeapFromAccs pres accs

        (accs', env')
              <- foldM (evalLoop evalPrim now (loop p)) (accs, pres') values

        posts <- mapLeft RuntimeErrorPost
                $ XV.evalExps evalPrim env' (postcomps p)

        ret   <- mapLeft RuntimeErrorReturn
                (XV.eval evalPrim posts (returns p))
             >>= baseValue

        let bgs = bubbleGumOutputOfAccumulatorHeap accs'

        return (bgs, ret)


initAcc :: Ord n
        => XV.EvalPrim n p
        -> Heap n p
        -> Accumulator n p
        -> Either (RuntimeError n p) (Name n, ([BubbleGumFact], AccumulatorValue))

initAcc evalPrim env (Accumulator n t)
 = do av <- getValue
      return (n, ([], av))
 where
  ev x
   = do v <- mapLeft RuntimeErrorAccumulator
           $ XV.eval evalPrim env x
        baseValue v

  getValue
   = case t of
     Resumable _ x
      -> AVFold False <$> ev x
     Windowed  _ x
      -> AVFold True <$> ev x
     Latest    _ x
      -> do v    <- ev x
            case v of
             VInt i
              -> return $ AVLatest i []
             _
              -> Left (RuntimeErrorAccumulatorLatestNotInt v)


evalLoop
        :: Ord n
        => XV.EvalPrim n p
        -> DateTime
        -> Loop n p
        -> (AccumulatorHeap n, Heap n p)
        -> AsAt (BubbleGumFact, BaseValue)
        -> Either (RuntimeError n p) (AccumulatorHeap n, Heap n p)
evalLoop evalPrim now (Loop _ stmts) (ah, xh) input
 = foldM (evalStmt evalPrim now input) (ah, xh) stmts


evalStmt
        :: Ord n
        => XV.EvalPrim n p
        -> DateTime
        -> AsAt (BubbleGumFact, BaseValue)
        -> (AccumulatorHeap n, Heap n p)
        -> Statement n p
        -> Either (RuntimeError n p) (AccumulatorHeap n, Heap n p)
evalStmt evalPrim now input (ah, xh) stmt
 = case stmt of
    If x stmts
     -> do  v   <- eval x >>= baseValue
            case v of
             VBool True
              -> go stmts
             _
              -> return (ah, xh)

    IfWindowed window stmts
     -> if   withinWindow (time input) now window
        then go stmts
        else return (ah, xh)

    Update n x
     -> do  v   <- eval x >>= baseValue
            ah' <- updateOrPush ah n (fst $ fact input) v
            return (ah', updateHeapFromAccs xh ah')

    Push n x
     -> do  v   <- eval x >>= baseValue
            ah' <- updateOrPush ah n (fst $ fact input) v
            return (ah', updateHeapFromAccs xh ah')

    Let n x
     -> do  v <- eval x
            return (ah, Map.insert n v xh)

    UseSource n
     -> return (ah, Map.insert n (VBase $ snd $ fact input) xh)

 where
  go = foldM (evalStmt evalPrim now input) (ah, xh)

  eval = mapLeft RuntimeErrorLoop
       . XV.eval evalPrim xh

