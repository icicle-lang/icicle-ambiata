-- | This is a very simple expression evaluator, the idea being to serve as a spec
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Eval (
      RuntimeError(..)
    , EvalPrim
    , eval0
    , eval
    , applyValues
    ) where

import Icicle.Common.Base
import Icicle.Common.Value
import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds

import              P

import qualified    Data.Map as Map


-- | Things that can go wrong (but shouldn't!)
data RuntimeError n p
 = RuntimeErrorBadApplication (Value n p) (Value n p)
 | RuntimeErrorVarNotInHeap (Name n)
 | RuntimeErrorPrimBadArgs p [Value n p]
 -- | Variables must be unique.
 -- This lets us be lazy with capture-avoiding substitution.
 | RuntimeErrorVarNotUnique (Name n) (Value n p) (Value n p)
 deriving (Show, Eq)

type EvalPrim n p = p -> [Value n p] -> Either (RuntimeError n p) (Value n p)

-- | Big step evaluation of a closed expression
-- Start with an empty heap.
eval0 :: Ord n => EvalPrim n p -> Exp n p -> Either (RuntimeError n p) (Value n p)
eval0 evalPrim = eval evalPrim Map.empty

-- | Big step evaluation with given heap
eval :: Ord n
     => EvalPrim n p
     -> Heap n p
     -> Exp n p
     -> Either (RuntimeError n p) (Value n p)

eval evalPrim h xx
 = case xx of
    -- Try to look up variable in heap
    XVar n
     -> maybeToRight (RuntimeErrorVarNotInHeap n)
                     (Map.lookup n h)

    -- Application of primitive.
    -- Primitives must be fully applied, so evalPrim will eat all the arguments.
    XApp{}
     | Just (p, args) <- takePrimApps xx
     -> do  vs <- mapM (go h) args
            evalPrim p vs

    -- If the left-hand side isn't a primitive, it must evaluate to a function.
    XApp p q
     -> do  p' <- go h p
            q' <- go h q
            -- Perform application
            applyValues evalPrim p' q'

    -- Primitive with no arguments - probably a constant.
    XPrim p
     -> evalPrim p []

    -- Lambdas cannot be evaluated any further;
    -- throw away the type and keep the current heap
    XLam n _ x
     -> return (VFun h n x)

    -- Evaluate definition, put it into heap, then evaluate "in" part
    XLet n d i
     -> do  d' <- go h d
            h' <- insertUnique n d' h
            go h' i
 where
  go = eval evalPrim







-- | Apply two values together
--
-- It is a bit annoying that we can't just use XApps,
-- as the expression language has no construct for Values.
--
-- I could add a Value term to the language, but values
-- can be closures which we don't want in the language.
--
-- This is exposed because Stream needs has values
-- and needs to apply them to Exps.
--
applyValues
        :: Ord n
        => EvalPrim n p
        -> Value n p
        -> Value n p
        -> Either (RuntimeError n p) (Value n p)
applyValues evalPrim f arg
 = case f of
    VFun hh nm x
           -- Evaluate expression with argument added to heap
     -> do hh' <- insertUnique nm arg hh
           eval evalPrim hh' x
    _
     -> Left (RuntimeErrorBadApplication f arg)



-- By requiring unique variable names and not having general lambdas,
-- we don't need capture avoiding substitution.
insertUnique
        :: Ord n
        => Name n
        -> Value n p
        -> Heap n p
        -> Either (RuntimeError n p) (Heap n p)

insertUnique n v hh
 | Just v' <- Map.lookup n hh
 = Left (RuntimeErrorVarNotUnique n v v')
 | otherwise
 = return (Map.insert n v hh)

