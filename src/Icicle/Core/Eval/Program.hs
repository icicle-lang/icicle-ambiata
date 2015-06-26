-- | Evaluate an entire program
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Core.Eval.Program (
      RuntimeError (..)
    , ProgramValue (..)
    , eval
    ) where

import              Icicle.BubbleGum

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Common.Value as V
import              Icicle.Core.Exp
import              Icicle.Core.Stream
import              Icicle.Core.Reduce

import qualified    Icicle.Core.Program.Program as P
import qualified    Icicle.Common.Exp.Eval  as XV
import qualified    Icicle.Core.Eval.Exp    as XV
import qualified    Icicle.Core.Eval.Stream as SV
import qualified    Icicle.Core.Eval.Reduce as RV

import              Icicle.Data.DateTime

import              Icicle.Internal.Pretty

import              P

import              Data.Either.Combinators
import qualified    Data.Map as Map


-- | Things that can go wrong for program evaluation
data RuntimeError n
 = RuntimeErrorPre      (XV.RuntimeError n Prim)
 | RuntimeErrorStream   (SV.RuntimeError n)
 | RuntimeErrorReduce   (RV.RuntimeError n)
 | RuntimeErrorPost     (XV.RuntimeError n Prim)
 | RuntimeErrorReturn   (XV.RuntimeError n Prim)
 | RuntimeErrorVarNotUnique (Name n)
 | RuntimeErrorReturnNotBaseType (V.Value n Prim)
 deriving (Show, Eq)

instance (Pretty n) => Pretty (RuntimeError n) where
 pretty (RuntimeErrorPre p)
  = "Precomputation error:" <> line
  <> indent 2 (pretty p)
 pretty (RuntimeErrorStream p)
  = "Stream error:" <> line
  <> indent 2 (pretty p)
 pretty (RuntimeErrorReduce p)
  = "Reduce error:" <> line
  <> indent 2 (pretty p)
 pretty (RuntimeErrorPost p)
  = "Postcomputation error:" <> line
  <> indent 2 (pretty p)
 pretty (RuntimeErrorReturn p)
  = "Return error:" <> line
  <> indent 2 (pretty p)
 pretty (RuntimeErrorVarNotUnique n)
  = "Variable name not unique: " <> pretty n
 pretty (RuntimeErrorReturnNotBaseType n)
  = "Return has function type, but should be simple value: " <> line
  <> "  Expression: " <> pretty n



-- | The result of evaluating a whole program:
-- some value, and the minimum facts needed to compute next value.
data ProgramValue n =
 ProgramValue {
    value   :: BaseValue
 ,  history :: [BubbleGumOutput n BaseValue]
 }
 deriving (Show, Eq)

-- | Evaluate a program.
-- We take no environments, but do take the concrete feature values.
eval    :: Ord n
        => DateTime
        -> SV.InitialStreamValue
        -> P.Program n
        -> Either (RuntimeError n) (ProgramValue n)
eval d sv p
 = do   pres    <- mapLeft RuntimeErrorPre
                 $ XV.evalExps XV.evalPrim  Map.empty   (P.precomps     p)
        (stms,bgleftovers) <- evalStms pres d sv       Map.empty   (P.streams      p)

        -- Get the history and results of reductions.
        -- The history is returned as-is but values are used in further computations
        (bgs,reds)
                <- evalReds pres            stms        (P.reduces      p)

        -- Insert date into environment if necessary
        let reds' = case P.postdate p of
                    Nothing -> reds
                    Just nm -> Map.insert nm (VBase $ VDateTime d) reds

        post    <- mapLeft RuntimeErrorPost
                 $ XV.evalExps XV.evalPrim  reds'       (P.postcomps    p)

        ret     <- mapLeft RuntimeErrorReturn
                 $ XV.eval XV.evalPrim post
                 $ P.returns p
        ret'    <- V.getBaseValue (RuntimeErrorReturnNotBaseType ret) ret

        return $ ProgramValue ret' $ bubbleGumNubOutputs (bgs <> bgleftovers)


-- | Evaluate all stream bindings, collecting up stream heap as we go
evalStms
        :: Ord n
        => V.Heap n Prim
        -> DateTime
        -> SV.InitialStreamValue
        -> SV.StreamHeap  n
        -> [(Name n, Stream n)]
        -> Either (RuntimeError n) (SV.StreamHeap n, [BubbleGumOutput n BaseValue])

evalStms _ _ _ sh []
 = return (sh, [])

evalStms xh d svs sh ((n,strm):bs)
 = do   v   <- mapLeft RuntimeErrorStream
             $ SV.eval d xh svs sh strm
        sh' <- insertUnique sh n $ SV.evalStreamValue v

        let out = bubbleGumOutputOfFacts
                $ SV.evalMarkAsRequired v

        (sh'', outs) <- evalStms xh d svs sh' bs
        return (sh'', out : outs)
        

-- | Evaluate all reduction bindings, inserting to expression heap as we go
-- return expression heap at the end, throwing away streams
evalReds
        :: Ord n
        => V.Heap n Prim
        -> SV.StreamHeap  n
        -> [(Name n, Reduce n)]
        -> Either (RuntimeError n) ([BubbleGumOutput n BaseValue], V.Heap n Prim)

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
            <- insertUnique xh' n (V.VBase v)
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

