-- | Evaluate an entire program
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
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

import qualified    Icicle.Core.Program.Program as P
import qualified    Icicle.Common.Exp.Eval  as XV
import qualified    Icicle.Core.Eval.Exp    as XV
import qualified    Icicle.Core.Eval.Stream as SV

import              Icicle.Data

import              Icicle.Internal.Pretty

import              P

import qualified    Data.Map as Map
import              Data.Hashable (Hashable)
import qualified    Data.Set as Set
import              Data.List (zipWith)


-- | Things that can go wrong for program evaluation
data RuntimeError a n
 = RuntimeErrorPre      (XV.RuntimeError a n Prim)
 | RuntimeErrorStream   (SV.RuntimeError a n)
 | RuntimeErrorPost     (XV.RuntimeError a n Prim)
 | RuntimeErrorReturn   (XV.RuntimeError a n Prim)
 | RuntimeErrorVarNotUnique (Name n)
 | RuntimeErrorReturnNotBaseType (V.Value a n Prim)
 deriving (Show, Eq)

instance (Pretty n) => Pretty (RuntimeError a n) where
 pretty (RuntimeErrorPre p)
  = "Precomputation error:" <> line
  <> indent 2 (pretty p)
 pretty (RuntimeErrorStream p)
  = "Stream error:" <> line
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
    value   :: [(OutputName, BaseValue)]
 ,  history :: Set.Set FactIdentifier
 }
 deriving (Show, Eq)

-- | Right at the start, we need dates on the stream values.
-- These can be used by windowing functions or ignored.
-- Afterwards they are thrown away, but could still be included in the value itself.
type InitialStreamValue
 = [AsAt (BubbleGumFact, BaseValue)]


-- | Evaluate a program.
-- We take no environments, but do take the concrete feature values.
eval    :: (Hashable n, Eq n)
        => Time
        -> InitialStreamValue
        -> P.Program a n
        -> Either (RuntimeError a n) (ProgramValue n)
eval d sv p
 = do   let env0 = Map.singleton (P.snaptimeName p) (VBase $ VTime d)
        pres    <- first RuntimeErrorPre
                 $ XV.evalExps XV.evalPrim  env0   (P.precomps     p)

        let mkstream f t = SV.StreamValue (fmap f sv) (defaultOfType t)
        let idStream = SV.StreamValue (zipWith (\_ i -> VFactIdentifier $ FactIdentifier i) sv [0..]) (defaultOfType FactIdentifierT)
        let valueOfInput at = VPair (snd $ atFact at) (VTime $ atTime at)

        let inputHeap = Map.fromList 
                      [(P.factValName  p, mkstream valueOfInput (PairT (P.inputType p) TimeT))
                      ,(P.factIdName   p, idStream)
                      ,(P.factTimeName p, mkstream (VTime . atTime) TimeT)]
        (stms,facts) <- evalStms pres inputHeap (P.streams      p)

        let lastSV svals | (v:_) <- reverse $ SV.streamValues svals
                         = v
                         | otherwise
                         = SV.streamZero svals

        -- Get the final value of each stream, or any precomputations. Streams take precedence
        let stms' = Map.union (Map.map (VBase . lastSV) stms) pres

        post    <- first RuntimeErrorPost
                 $ XV.evalExps XV.evalPrim  stms'       (P.postcomps    p)

        let evalReturn (n,x)
                 = do ret   <- first RuntimeErrorReturn
                             $ XV.eval XV.evalPrim post x
                      ret'  <- V.getBaseValue (RuntimeErrorReturnNotBaseType ret) ret
                      return (n, ret')

        rets <- mapM evalReturn (P.returns p)

        let facts' = Set.unions
                   $ fmap V.collectFactIdentifiersFromBufs
                   $ Map.elems stms'

        return $ ProgramValue rets (Set.union facts facts')


-- | Evaluate all stream bindings, collecting up stream heap as we go
evalStms
        :: (Hashable n, Eq n)
        => V.Heap a n Prim
        -> SV.StreamHeap  n
        -> [Stream a n]
        -> Either (RuntimeError a n) (SV.StreamHeap n, Set.Set FactIdentifier)

evalStms _ sh []
 = return (sh, Set.empty)

evalStms xh sh (strm:bs)
 = do   (sh',facts)     <- first RuntimeErrorStream
                         $ SV.eval xh sh strm

        (sh'', facts')  <- evalStms xh sh' bs
        return (sh'', Set.union facts facts')

