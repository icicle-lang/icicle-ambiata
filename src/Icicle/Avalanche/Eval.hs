-- | Evaluate Avalanche programs
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


-- | Store history information about the accumulators
type AccumulatorHeap n
 = Map.Map (Name n) ([BubbleGumFact], AccumulatorValue)

-- | The value of an accumulator
data AccumulatorValue
 -- | Whether this fold is windowed or not
 = AVFold Bool BaseValue
 -- | Accumulator storing latest N values
 -- Stored in reverse so we can just cons or take it
 | AVLatest Int [BaseValue]
 deriving (Eq, Ord, Show)


-- | What can go wrong evaluating an Avalanche
data RuntimeError n p
 = RuntimeErrorNoAccumulator (Name n)
 | RuntimeErrorPre           (XV.RuntimeError n p)
 | RuntimeErrorAccumulator   (XV.RuntimeError n p)
 | RuntimeErrorLoop          (XV.RuntimeError n p)
 | RuntimeErrorLoopAccumulatorBad (Name n)
 | RuntimeErrorPost          (XV.RuntimeError n p)
 | RuntimeErrorReturn        (XV.RuntimeError n p)
 | RuntimeErrorNotBaseValue  (Value n p)
 | RuntimeErrorAccumulatorLatestNotInt  BaseValue
 deriving (Eq, Show)


-- | Extract base value; return an error if it's a closure
baseValue :: Value n p -> Either (RuntimeError n p) BaseValue
baseValue v
 = getBaseValue (RuntimeErrorNotBaseValue v) v


-- | Update value or push value to an accumulator, taking care of history
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


-- | For each accumulator value, write its scalar value back into the environment.
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


-- | For each accumulator value, get the history information
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


-- | Evaluate an entire program
-- with given primitive evaluator and values
evalProgram
        :: Ord n
        => XV.EvalPrim n p
        -> DateTime
        -> [AsAt (BubbleGumFact, BaseValue)]
        -> Program n p
        -> Either (RuntimeError n p) ([BubbleGumOutput n BaseValue], BaseValue)

evalProgram evalPrim now values p
 = do   -- Precomputations are just expressions
        pres  <- mapLeft RuntimeErrorPre
               $ XV.evalExps evalPrim Map.empty  (precomps p)
        
        -- Initialise all the accumulators into their own heap
        accs  <- Map.fromList <$> mapM (initAcc evalPrim pres) (accums   p)

        -- Keep evaluating the same loop for every value
        -- with accumulator and scalar heaps threaded through
        accs' <- foldM (evalLoop evalPrim now (loop p) pres) accs values

        -- Push the accumulators back into scalar heap
        let env'  = updateHeapFromAccs pres accs'

        -- Grab the history out of the accumulator heap while we're at it
        let bgs = bubbleGumOutputOfAccumulatorHeap accs'

        -- Use final scalar heap to evaluate postcomputations
        posts <- mapLeft RuntimeErrorPost
                $ XV.evalExps evalPrim env' (postcomps p)

        -- Then use postcomputations to evaluate the return value
        ret   <- mapLeft RuntimeErrorReturn
                (XV.eval evalPrim posts (returns p))
             >>= baseValue

        return (bgs, ret)


-- | Initialise an accumulator
initAcc :: Ord n
        => XV.EvalPrim n p
        -> Heap n p
        -> Accumulator n p
        -> Either (RuntimeError n p) (Name n, ([BubbleGumFact], AccumulatorValue))

initAcc evalPrim env (Accumulator n t)
 = do av <- getValue
      -- There is no history yet, just a value
      return (n, ([], av))
 where
  ev x
   = do v <- mapLeft RuntimeErrorAccumulator
           $ XV.eval evalPrim env x
        baseValue v

  getValue
   = case t of
     -- Start with initial value.
     -- TODO: take list of previously saved resumes, and lookup here
     Resumable _ x
      -> AVFold False <$> ev x
     Windowed  _ x
      -> AVFold True <$> ev x
     Latest    _ x
            -- Figure out how many latest to store,
            -- but nothing is stored yet
      -> do v    <- ev x
            case v of
             VInt i
              -> return $ AVLatest i []
             _
              -> Left (RuntimeErrorAccumulatorLatestNotInt v)


-- | Evaluate an entire loop for a single value
-- Takes accumulator and scalar heaps and value, returns new heaps.
evalLoop
        :: Ord n
        => XV.EvalPrim n p
        -> DateTime
        -> FactLoop n p
        -> Heap n p
        -> AccumulatorHeap n
        -> AsAt (BubbleGumFact, BaseValue)
        -> Either (RuntimeError n p) (AccumulatorHeap n)

evalLoop evalPrim now (FactLoop _ bind stmts) xh ah input
 -- Just go through all the statements
 = foldM (evalStmt evalPrim now xh' input) ah stmts
 where
  xh' = Map.insert bind streamvalue xh
  streamvalue = VBase $ VPair (snd $ fact input) (VDateTime $ time input)


-- | Evaluate a single statement for a single value
evalStmt
        :: Ord n
        => XV.EvalPrim n p
        -> DateTime
        -> Heap n p
        -> AsAt (BubbleGumFact, BaseValue)
        -> AccumulatorHeap n
        -> Statement n p
        -> Either (RuntimeError n p) (AccumulatorHeap n)

evalStmt evalPrim now xh input ah stmt
 = case stmt of
    If x stmts
     -> do  v   <- eval x >>= baseValue
            case v of
             -- Predicate must be true
             VBool True
              -> go' stmts
             -- This is not ideal, but if it is not a boolean,
             -- our type checker will catch it.
             _
              -> return ah

    IfWindowed window stmts
        -- Check the input fact's time against now
     -> if   withinWindow (time input) now window
        then go' stmts
        else return ah

    -- Evaluate and insert the value into the heap.
    Let n x stmts
     -> do  v <- eval x
            go (Map.insert n v xh) ah stmts

    -- Update accumulator
    Update n x
     -> do  vf  <- eval x
            -- Get the current value and apply the function
            v   <- case Map.lookup n ah of
                    Just (_, AVFold _ vacc)
                     ->  mapLeft RuntimeErrorLoop
                        (XV.applyValues evalPrim vf (VBase vacc))
                     >>= baseValue
                    _
                     -> Left (RuntimeErrorLoopAccumulatorBad n)

            updateOrPush ah n (fst $ fact input) v

    -- Push a value to a latest accumulator.
    Push n x
     -> do  v   <- eval x >>= baseValue
            ah' <- updateOrPush ah n (fst $ fact input) v
            return ah'

 where
  -- Go through all the substatements
  go xh' = foldM (evalStmt evalPrim now xh' input)
  go' = go xh ah

  -- Raise Exp error to Avalanche
  eval = mapLeft RuntimeErrorLoop
       . XV.eval evalPrim xh

