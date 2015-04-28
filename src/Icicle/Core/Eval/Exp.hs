-- | This is a very simple expression evaluator, the idea being to serve as a spec
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Exp (
      Value (..)
    , RuntimeError(..)
    , Heap
    , eval0
    , eval
    , applyValues
    ) where

import Icicle.Core.Base
import Icicle.Core.Exp

import              P

import qualified    Data.Map as Map


-- | A heap is just a mapping from names to values.
type Heap n
 = Map.Map (Name n) (Value n)


-- | Values are relatively simple, except for functions.
data Value n
 = VInt   Int
 | VBool  Bool
 | VArray [Value n]
 | VPair  (Value n) (Value n)
 -- | A function carries its own heap, the name of its argument, and the expression to apply.
 | VFun   (Heap n)  (Name n)  (Exp n)
 deriving (Show, Eq)


-- | Things that can go wrong (but shouldn't!)
data RuntimeError n
 = RuntimeErrorBadApplication (Value n) (Value n)
 | RuntimeErrorVarNotInHeap (Name n)
 | RuntimeErrorPrimBadArgs Prim [Value n]
 -- | Variables must be unique.
 -- This lets us be lazy with capture-avoiding substitution.
 | RuntimeErrorVarNotUnique (Name n) (Value n) (Value n)
 deriving (Show, Eq)


-- | Big step evaluation of a closed expression
-- Start with an empty heap.
eval0 :: Ord n => Exp n -> Either (RuntimeError n) (Value n)
eval0 = eval Map.empty

-- | Big step evaluation with given heap
eval :: Ord n
     => Heap n
     -> Exp n
     -> Either (RuntimeError n) (Value n)

eval h xx
 = case xx of
    -- Try to look up variable in heap
    XVar n
     -> maybeToRight (RuntimeErrorVarNotInHeap n)
                     (Map.lookup n h)

    -- Application of primitive.
    -- Primitives must be fully applied, so evalPrim will eat all the arguments.
    XApp{}
     | Just (p, args) <- takePrimApps xx
     -> do  vs <- mapM (eval h) args
            evalPrim p vs

    -- If the left-hand side isn't a primitive, it must evaluate to a function.
    XApp p q
     -> do  p' <- eval h p
            q' <- eval h q
            -- Perform application
            applyValues p' q'

    -- Primitive with no arguments - probably a constant.
    XPrim p
     -> evalPrim p []

    -- Lambdas cannot be evaluated any further;
    -- throw away the type and keep the current heap
    XLam n _ x
     -> return (VFun h n x)

    -- Evaluate definition, put it into heap, then evaluate "in" part
    XLet n d i
     -> do  d' <- eval h d
            h' <- insertUnique n d' h
            eval h' i


 where
  -- Evaluate a primitive, given list of argument values
  evalPrim p vs
   = case (p,vs) of

     (PrimArith PrimArithPlus,      [VInt i, VInt j])
      -> return $ VInt (i+j)

     (PrimArith PrimArithMinus,     [VInt i, VInt j])
      -> return $ VInt (i-j)

     (PrimConst (PrimConstInt i),   [])
      -> return $ VInt i

     -- Something went wrong
     _
      -> Left (RuntimeErrorPrimBadArgs p vs) 



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
        => Value n
        -> Value n
        -> Either (RuntimeError n) (Value n)
applyValues f arg
 = case f of
    VFun hh nm x
           -- Evaluate expression with argument added to heap
     -> do hh' <- insertUnique nm arg hh
           eval hh' x
    _
     -> Left (RuntimeErrorBadApplication f arg)



-- By requiring unique variable names and not having general lambdas,
-- we don't need capture avoiding substitution.
insertUnique
        :: Ord n
        => Name n
        -> Value n
        -> Heap n
        -> Either (RuntimeError n) (Heap n)

insertUnique n v hh
 | Just v' <- Map.lookup n hh
 = Left (RuntimeErrorVarNotUnique n v v')
 | otherwise
 = return (Map.insert n v hh)

