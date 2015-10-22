-- | Evaluate Avalanche programs
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Avalanche.Eval (
    evalProgram
  , RuntimeError
  ) where

import              Icicle.Avalanche.Statement.Statement
import              Icicle.Avalanche.Statement.Simp.Melt
import              Icicle.Avalanche.Program

import              Icicle.BubbleGum

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Value
import qualified    Icicle.Common.Exp as XV

import              Icicle.Data.DateTime
import              Icicle.Data         (AsAt(..))

import              P

import              Data.Either.Combinators
import              Data.List   (take, sort, zip)
import qualified    Data.Map    as Map

import              Icicle.Internal.Pretty

-- | Store history information about the accumulators
data AccumulatorHeap n
 = AccumulatorHeap
 { accumulatorHeapMap       :: Map.Map (Name n) ([BubbleGumFact], AccumulatorValue)
 , accumulatorHeapMarked    :: [BubbleGumOutput n BaseValue]
 }

-- | The value of an accumulator
data AccumulatorValue
 -- | Accumulator storing latest N values
 -- Stored in reverse so we can just cons or take it
 = AVLatest Int [BaseValue]

 -- | A mutable value with no history attached
 | AVMutable BaseValue
 deriving (Eq, Ord, Show)


-- | What can go wrong evaluating an Avalanche
data RuntimeError a n p
 = RuntimeErrorNoAccumulator (Name n)
 | RuntimeErrorAccumulator   (XV.RuntimeError a n p)
 | RuntimeErrorLoop          (XV.RuntimeError a n p)
 | RuntimeErrorLoopAccumulatorBad (Name n)
 | RuntimeErrorIfNotBool     BaseValue
 | RuntimeErrorForeachNotInt BaseValue BaseValue
 | RuntimeErrorForeachTypeMismatch [(Name n, ValType)] ValType BaseValue
 | RuntimeErrorNotBaseValue  (Value a n p)
 | RuntimeErrorKeepFactNotInFactLoop
 | RuntimeErrorAccumulatorLatestNotInt  BaseValue
 deriving (Eq, Show)

instance (Pretty n, Pretty p) => Pretty (RuntimeError a n p) where
 pretty (RuntimeErrorNoAccumulator n)
  = "No accumulator:" <+> pretty n
 pretty (RuntimeErrorAccumulator p)
  = pretty p
 pretty (RuntimeErrorLoop p)
  = pretty p
 pretty (RuntimeErrorLoopAccumulatorBad n)
  = "Bad loop accumulator:" <+> pretty n
 pretty (RuntimeErrorIfNotBool p)
  = "Value should be a bool but isn't" <+> (pretty p)
 pretty (RuntimeErrorForeachNotInt p p')
  = "Foreach not ints:" <+> pretty p <+> pretty p'
 pretty (RuntimeErrorForeachTypeMismatch ns ty v)
  = "Foreach type error: bindings = " <+> align (vcat (fmap pretty ns)) <> line <>
    "                    type     = " <+> pretty ty <> line <>
    "                    value    = " <+> pretty v
 pretty (RuntimeErrorNotBaseValue p)
  = "Value isn't a base value:" <+> (pretty p)
 pretty (RuntimeErrorKeepFactNotInFactLoop)
  = "Tried to keep a value in the history which doesn't have bubblegum"
 pretty (RuntimeErrorAccumulatorLatestNotInt p)
  = "Accumulator Latest needs an integer, got" <+> pretty p


-- | Extract base value; return an error if it's a closure
baseValue :: Value a n p -> Either (RuntimeError a n p) BaseValue
baseValue v
 = getBaseValue (RuntimeErrorNotBaseValue v) v


-- | Update value or push value to an accumulator, taking care of history
updateOrPush
        :: Ord n
        => AccumulatorHeap n
        -> Name n
        -> Maybe BubbleGumFact
        -> BaseValue
        -> Either (RuntimeError a n p) (AccumulatorHeap n)

updateOrPush heap n bg v
 = do   let map          = accumulatorHeapMap heap

        let replace map' = return
                         $ heap { accumulatorHeapMap = map' }

        v' <- maybeToRight (RuntimeErrorNoAccumulator n)
                           (Map.lookup n map)
        case v' of
         (bgs, AVLatest num vs)
          -> replace
           $ Map.insert n
           ( take num (insbgs bgs)
           , AVLatest num (take num (v : vs)) ) map
         (_, AVMutable _)
          -> replace
           $ Map.insert n ([], AVMutable v) map
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
 = bubbleGumNubOutputs
 (accumulatorHeapMarked acc <> concatMap mk (Map.toList $ accumulatorHeapMap acc))
 where
  mk (_, (bgs, AVLatest _ _))
   = [BubbleGumFacts $ sort $ fmap flav bgs]
  mk (_, (_, AVMutable _))
   = []

  flav (BubbleGumFact f) = f


-- | Evaluate an entire program
-- with given primitive evaluator and values
evalProgram
        :: Ord n
        => XV.EvalPrim a n p
        -> DateTime
        -> [AsAt (BubbleGumFact, BaseValue)]
        -> Program a n p
        -> Either (RuntimeError a n p) ([BubbleGumOutput n BaseValue], [(OutputName,BaseValue)])

evalProgram evalPrim now values p
 = do   -- Precomputations are just expressions

        -- Keep evaluating the same loop for every value
        -- with accumulator and scalar heaps threaded through
        let stmts = statements p
        let xh    = Map.singleton (binddate p) $ VBase $ VDateTime $ now
        let ah    = AccumulatorHeap Map.empty []
        (accs',ret) <- evalStmt evalPrim now xh values Nothing ah stmts

        -- Grab the history out of the accumulator heap while we're at it
        let bgs = bubbleGumNubOutputs $ bubbleGumOutputOfAccumulatorHeap accs'

        return (bgs, ret)


-- | Initialise an accumulator
initAcc :: Ord n
        => XV.EvalPrim a n p
        -> Heap a n p
        -> Accumulator a n p
        -> Either (RuntimeError a n p) (Name n, ([BubbleGumFact], AccumulatorValue))

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
        => XV.EvalPrim a n p
        -> DateTime
        -> Heap a n p
        -> [AsAt (BubbleGumFact, BaseValue)]
        -> Maybe BubbleGumFact
        -> AccumulatorHeap n
        -> Statement a n p
        -> Either (RuntimeError a n p) (AccumulatorHeap n, [(OutputName, BaseValue)])

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
                    return (ahs, [])
             _
              -> Left $ RuntimeErrorForeachNotInt fromv tov

    -- TODO: evaluation ignores history/bubblegum.
    -- All inputs are new, so history loop does nothing.
    ForeachFacts _ _ FactLoopHistory _
     -> return (ah, [])

    -- TODO: ignoring outputs inside loops.

    -- Allow unmelted foreach
    -- (i.e. where ty == ty' and we only have a singleton list of bindings)
    ForeachFacts [(n, ty)] ty' FactLoopNew stmts
     | ty == ty'
     -> do  let evalInput ah' input = do
                  let v0     = snd (fact input)
                      v1     = VDateTime (time input)
                      vv     = VPair v0 v1
                      input' = Map.insert n (VBase vv) xh
                      bgf    = Just $ fst $ fact input

                  fst <$> evalStmt evalPrim now input' [] bgf ah' stmts

            ahs <- foldM evalInput ah values
            return (ahs, [])

    ForeachFacts ns ty FactLoopNew stmts
     -> do  let evalInput ah' input = do
                  let v0  = snd (fact input)
                      v1  = VDateTime (time input)
                      vv  = VPair v0 v1
                      mvs = meltValue vv ty

                  case mvs of
                    Nothing
                     -> Left (RuntimeErrorForeachTypeMismatch ns ty vv)

                    Just vs
                     | length vs /= length ns
                     -> Left (RuntimeErrorForeachTypeMismatch ns ty vv)

                     | otherwise
                     , nvs    <- zip (fmap fst ns) vs
                     , input' <- foldr (\(n, v) -> Map.insert n (VBase v)) xh nvs
                     , bgf    <- Just $ fst $ fact input
                     -> fst <$> evalStmt evalPrim now input' [] bgf ah' stmts

            ahs <- foldM evalInput ah values
            return (ahs, [])

    Block []
     -> return (ah, [])
    Block [s]
     -> go' s
    Block (s:ss)
     -> do (ah',vs) <- go xh ah s
           (ah'', vs') <- go xh ah' (Block ss)
           return (ah'', vs <> vs')

    InitAccumulator acc stmts
     -> do (n,av)  <- initAcc evalPrim xh acc
           let map' = Map.insert n av $ accumulatorHeapMap ah
           go xh (ah { accumulatorHeapMap = map' }) stmts

    -- Read from an accumulator
    Read n acc _ _ stmts
     -> do  -- Get the current value and apply the function
            v   <- case Map.lookup acc $ accumulatorHeapMap ah of
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
            return (ah', [])

    -- Push a value to a latest accumulator.
    Push n x
     -> do  v   <- eval x >>= baseValue
            ah' <- updateOrPush ah n bubblegum v
            return (ah', [])

    Output n x
     -> do  v  <- eval x >>= baseValue
            return (ah, [(n, v)])

    -- Keep this fact in history
    KeepFactInHistory
     | Just (BubbleGumFact bg) <- bubblegum
     -> return (ah { accumulatorHeapMarked = BubbleGumFacts [bg] : accumulatorHeapMarked ah }, [])
     | otherwise
     -> Left $ RuntimeErrorKeepFactNotInFactLoop

    LoadResumable _ _
     -> return (ah, [])

    SaveResumable acc _
     -> do  v   <- case Map.lookup acc $ accumulatorHeapMap ah of
                    Just (_, AVMutable vacc)
                     -> return $ vacc
                    Just (_, AVLatest _ vals)
                     -> return $ VArray $ reverse vals
                    _
                     -> Left (RuntimeErrorLoopAccumulatorBad acc)
            return (ah { accumulatorHeapMarked = BubbleGumReduction acc v : accumulatorHeapMarked ah }, [])

 where
  -- Go through all the substatements
  go xh' = evalStmt evalPrim now xh' values bubblegum
  go' = go xh ah

  -- Raise Exp error to Avalanche
  eval = mapLeft RuntimeErrorLoop
       . XV.eval evalPrim xh

