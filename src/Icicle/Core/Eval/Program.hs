-- | Evaluate an entire program
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Eval.Program (
      RuntimeError (..)
    , ProgramValue (..)
    , eval
    ) where

import              Icicle.BubbleGum

import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp
import              Icicle.Core.Stream
import              Icicle.Core.Reduce

import qualified    Icicle.Core.Program.Program as P
import qualified    Icicle.Core.Eval.Exp    as XV
import qualified    Icicle.Core.Eval.Stream as SV
import qualified    Icicle.Core.Eval.Reduce as RV

import              Icicle.Data.DateTime

import              P

import              Data.Either.Combinators
import qualified    Data.Map as Map


-- | Things that can go wrong for program evaluation
data RuntimeError n
 = RuntimeErrorPre      (XV.RuntimeError n)
 | RuntimeErrorStream   (SV.RuntimeError n)
 | RuntimeErrorReduce   (RV.RuntimeError n)
 | RuntimeErrorPost     (XV.RuntimeError n)
 | RuntimeErrorReturn   (XV.RuntimeError n)
 | RuntimeErrorVarNotUnique (Name n)
 deriving (Show, Eq)


-- | The result of evaluating a whole program:
-- some value, and the minimum facts needed to compute next value.
data ProgramValue n =
 ProgramValue {
    value   :: XV.Value n
 ,  history :: [BubbleGumOutput n (XV.Value n)]
 }

-- | Evaluate a program.
-- We take no environments, but do take the concrete feature values.
eval    :: Ord n
        => DateTime
        -> SV.DatedStreamValue n
        -> P.Program n
        -> Either (RuntimeError n) (ProgramValue n)
eval d sv p
 = do   pres    <- evalExps RuntimeErrorPre Map.empty   (P.precomps     p)
        stms    <- evalStms pres d sv       Map.empty   (P.streams      p)

        -- Get the history and results of reductions.
        -- The history is returned as-is but values are used in further computations
        (bgs,reds)
                <- evalReds pres            stms        (P.reduces      p)
        post    <- evalExps RuntimeErrorPost reds       (P.postcomps    p)

        ret     <- mapLeft RuntimeErrorReturn
                 $ XV.eval post
                 $ P.returns p
        return $ ProgramValue ret bgs


-- | Evaluate all expression bindings, collecting up expression heap as we go
evalExps
        :: Ord n
        => (XV.RuntimeError n -> RuntimeError n)
        -> XV.Heap n
        -> [(Name n, Exp n)]
        -> Either (RuntimeError n) (XV.Heap n)

evalExps _ env []
 = return env

evalExps err env ((n,x):bs)
 = do   v   <- mapLeft err
             $ XV.eval env x
        env' <- insertUnique env n v
        evalExps err env' bs


-- | Evaluate all stream bindings, collecting up stream heap as we go
evalStms
        :: Ord n
        => XV.Heap n
        -> DateTime
        -> SV.DatedStreamValue n
        -> SV.StreamHeap  n
        -> [(Name n, Stream n)]
        -> Either (RuntimeError n) (SV.StreamHeap n)

evalStms _ _ _ sh []
 = return sh

evalStms xh d svs sh ((n,strm):bs)
 = do   v   <- mapLeft RuntimeErrorStream
             $ SV.eval d xh svs sh strm
        sh' <- insertUnique sh n v
        evalStms xh d svs sh' bs
        

-- | Evaluate all reduction bindings, inserting to expression heap as we go
-- return expression heap at the end, throwing away streams
evalReds
        :: Ord n
        => XV.Heap n
        -> SV.StreamHeap  n
        -> [(Name n, Reduce n)]
        -> Either (RuntimeError n) ([BubbleGumOutput n (XV.Value n)], XV.Heap n)

evalReds xh _ []
 = return ([], xh)

evalReds xh sh ((n,red):bs)
 = do   (bg,v)
            <- mapLeft RuntimeErrorReduce
             $ RV.eval n xh sh red
        -- Evaluate the remaining reductions before inserting into heap
        -- This shouldn't affect the semantics since names are unique.
        (bgs, xh')
            <- evalReds xh sh bs
        xh''
            <- insertUnique xh' n v
        return (bg:bgs, xh'')


-- Return an error if name already has a value
insertUnique
        :: Ord n
        => Map.Map (Name n) v
        -> Name n
        -> v
        -> Either (RuntimeError n) (Map.Map (Name n) v)

insertUnique 
 = insertOrDie RuntimeErrorVarNotUnique

