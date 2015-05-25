-- | Evaluate Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Eval (
    evalProgram
  ) where

import              Icicle.Avalanche.Statement.Statement
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

 -- | A mutable value with no history attached
 | AVMutable BaseValue
 deriving (Eq, Ord, Show)


-- | What can go wrong evaluating an Avalanche
data RuntimeError n p
 = RuntimeErrorNoAccumulator (Name n)
 | RuntimeErrorAccumulator   (XV.RuntimeError n p)
 | RuntimeErrorLoop          (XV.RuntimeError n p)
 | RuntimeErrorLoopAccumulatorBad (Name n)
 | RuntimeErrorIfNotBool     BaseValue
 | RuntimeErrorForeachNotInt BaseValue BaseValue
 | RuntimeErrorNotBaseValue  (Value n p)
 | RuntimeErrorNoReturn
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
        -> Maybe BubbleGumFact
        -> BaseValue
        -> Either (RuntimeError n p) (AccumulatorHeap n)

updateOrPush heap n bg v
 = do   v' <- maybeToRight (RuntimeErrorNoAccumulator n)
                           (Map.lookup n heap)
        case v' of
         (bgs, AVFold windowed _)
          -> return
           $ Map.insert n
           (insbgs bgs
           , AVFold windowed v) heap
         (bgs, AVLatest num vs)
          -> return 
           $ Map.insert n
           ( take num (insbgs bgs)
           , AVLatest num (take num (v : vs)) ) heap
         (_, AVMutable _)
          -> return
           $ Map.insert n ([], AVMutable v) heap
 where
  insbgs bgs
   = case bg of
      Nothing  -> bgs
      Just bg' -> bg' : bgs


-- | For each accumulator value, get the history information
bubbleGumOutputOfAccumulatorHeap
        :: Ord n
        => AccumulatorHeap n
        -> [BubbleGumOutput n (BaseValue)]

bubbleGumOutputOfAccumulatorHeap acc
 = concatMap  mk
 $ Map.toList acc
 where
  mk (n, (_, AVFold False v))
   = [BubbleGumReduction n v]
  mk (_, (bgs, AVFold True _))
   = [BubbleGumFacts $ sort $ fmap flav bgs]
  mk (_, (bgs, AVLatest _ _))
   = [BubbleGumFacts $ sort $ fmap flav bgs]
  mk (_, (_, AVMutable _))
   = []
  
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

        -- Keep evaluating the same loop for every value
        -- with accumulator and scalar heaps threaded through
        let stmts = statements p
        let xh    = Map.singleton (binddate p) $ VBase $ VDateTime $ now
        let ah    = Map.empty
        (accs',ret) <- evalStmt evalPrim now xh values Nothing ah stmts

        -- Grab the history out of the accumulator heap while we're at it
        let bgs = bubbleGumOutputOfAccumulatorHeap accs'

        case ret of 
         Nothing -> Left RuntimeErrorNoReturn
         Just r  -> return (bgs, r)


-- | Initialise an accumulator
initAcc :: Ord n
        => XV.EvalPrim n p
        -> Heap n p
        -> Accumulator n p
        -> Either (RuntimeError n p) (Name n, ([BubbleGumFact], AccumulatorValue))

initAcc evalPrim env (Accumulator n at _ x)
 = do av <- getValue
      -- There is no history yet, just a value
      return (n, ([], av))
 where
  ev
   = do v <- mapLeft RuntimeErrorAccumulator
           $ XV.eval evalPrim env x
        baseValue v

  getValue
   = case at of
     -- Start with initial value.
     -- TODO: take list of previously saved resumes, and lookup here
     Resumable
      -> AVFold False <$> ev
     Windowed
      -> AVFold True <$> ev
     Mutable
      -> AVMutable <$> ev
     Latest
            -- Figure out how many latest to store,
            -- but nothing is stored yet
      -> do v    <- ev
            case v of
             VInt i
              -> return $ AVLatest i []
             _
              -> Left (RuntimeErrorAccumulatorLatestNotInt v)


-- | Evaluate a single statement for a single value
evalStmt
        :: Ord n
        => XV.EvalPrim n p
        -> DateTime
        -> Heap n p
        -> [AsAt (BubbleGumFact, BaseValue)]
        -> Maybe BubbleGumFact
        -> AccumulatorHeap n
        -> Statement n p
        -> Either (RuntimeError n p) (AccumulatorHeap n, Maybe BaseValue)

evalStmt evalPrim now xh values bubblegum ah stmt
 = case stmt of
    If x stmts elses
     -> do  v   <- eval x >>= baseValue
            case v of
             -- Run "then" or "else"?
             VBool True
              -> go' stmts
             VBool False
              -> go' elses
             _-> Left (RuntimeErrorIfNotBool v)

    -- Evaluate and insert the value into the heap.
    Let n x stmts
     -> do  v <- eval x
            go (Map.insert n v xh) ah stmts

    ForeachInts n from to stmts
     -> do  fromv <- eval from >>= baseValue
            tov   <- eval to   >>= baseValue
            case (fromv, tov) of
             (VInt fromi, VInt toi)
              -> -- Open-closed interval [from,to)
                 -- ie "foreach i in 0 to 0" does not run
                 do ahs <- foldM (\ah' index -> fst <$> go (Map.insert n (VBase $ VInt index) xh) ah' stmts)
                         ah
                         [fromi .. toi-1]
                    return (ahs, Nothing)
             _
              -> Left $ RuntimeErrorForeachNotInt fromv tov

    ForeachFacts n _ stmts
     -> do  let with input = Map.insert n (VBase $ VPair (snd $ fact input) (VDateTime $ time input)) xh
            ahs <- foldM (\ah' input -> fst <$> evalStmt evalPrim now (with input) [] (Just $ fst $ fact input) ah' stmts) ah values
            return (ahs, Nothing)

    Block []
     -> return (ah, Nothing)
    Block [s]
     -> go' s
    Block (s:ss)
     -> do (ah',_) <- go xh ah s
           go xh ah' (Block ss)

    InitAccumulator acc stmts
     -> do (n,av)  <- initAcc evalPrim xh acc
           go xh (Map.insert n av ah) stmts

    -- Read from an accumulator
    Read n acc stmts
     -> do  -- Get the current value and apply the function
            v   <- case Map.lookup acc ah of
                    Just (_, AVFold _ vacc)
                     -> return $ VBase vacc
                    Just (_, AVMutable vacc)
                     -> return $ VBase vacc
                    Just (_, AVLatest _ vals)
                     -> return $ VBase $ VArray $ reverse vals
                    _
                     -> Left (RuntimeErrorLoopAccumulatorBad acc)
            go (Map.insert n v xh) ah stmts

    -- Update accumulator
    Write n x
     -> do  v   <- eval x >>= baseValue
            ah' <- updateOrPush ah n bubblegum v
            return (ah', Nothing)

    -- Push a value to a latest accumulator.
    Push n x
     -> do  v   <- eval x >>= baseValue
            ah' <- updateOrPush ah n bubblegum v
            return (ah', Nothing)

    Return x
     -> do  v  <- eval x >>= baseValue
            return (ah, Just v)

 where
  -- Go through all the substatements
  go xh' = evalStmt evalPrim now xh' values bubblegum
  go' = go xh ah

  -- Raise Exp error to Avalanche
  eval = mapLeft RuntimeErrorLoop
       . XV.eval evalPrim xh

