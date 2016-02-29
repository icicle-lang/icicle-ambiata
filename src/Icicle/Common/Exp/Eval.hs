-- | This is a very simple expression evaluator, the idea being to serve as a spec
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Exp.Eval (
      RuntimeError(..)
    , EvalPrim
    , eval0
    , eval
    , evalExps
    , applyValues
    , applies
    ) where

import Icicle.Common.Base
import Icicle.Common.Value
import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Compounds

import Icicle.Internal.Pretty

import              P

import qualified    Data.Map as Map
import              Data.Hashable (Hashable)


-- | Things that can go wrong (but shouldn't!)
data RuntimeError a n p
 = RuntimeErrorBadApplication (Value a n p) (Value a n p)
 | RuntimeErrorVarNotInHeap (Name n)
 | RuntimeErrorPrimBadArgs p [Value a n p]
 deriving (Show, Eq)

instance (Pretty n, Pretty p) => Pretty (RuntimeError a n p) where
 pretty (RuntimeErrorBadApplication x y)
  = "Bad application:" <> line
  <> "  Function: " <> pretty x   <> line
  <> "  Argument: " <> pretty y
 pretty (RuntimeErrorVarNotInHeap n)
  = "No such expression variable: " <> pretty n
 pretty (RuntimeErrorPrimBadArgs p vs)
  = "Bad arguments to primitive: " <> line
  <> "  Primitive: " <> pretty p <> line
  <> "  Arguments: " <> pretty vs


type EvalPrim a n p = p -> [Value a n p] -> Either (RuntimeError a n p) (Value a n p)

-- | Big step evaluation of a closed expression
-- Start with an empty heap.
eval0 :: (Hashable n, Eq n)
      => EvalPrim a n p -> Exp a n p -> Either (RuntimeError a n p) (Value a n p)
eval0 evalPrim = eval evalPrim Map.empty

-- | Big step evaluation with given heap
eval :: (Hashable n, Eq n)
     => EvalPrim a n p
     -> Heap a n p
     -> Exp a n p
     -> Either (RuntimeError a n p) (Value a n p)

eval evalPrim h xx
 = case xx of
    -- Try to look up variable in heap
    XVar _ n
     -> maybeToRight (RuntimeErrorVarNotInHeap n)
                     (Map.lookup n h)

    XValue _ _ bv
     -> return $ VBase bv

    -- Application of primitive.
    -- Primitives must be fully applied, so evalPrim will eat all the arguments.
    XApp{}
     | Just (p, args) <- takePrimApps xx
     -> do  vs <- mapM (go h) args
            evalPrim' p vs

    -- If the left-hand side isn't a primitive, it must evaluate to a function.
    XApp _ p q
     -> do  p' <- go h p
            q' <- go h q
            -- Perform application
            wrapException [p', q'] (applyValues evalPrim p' q')

    -- Primitive with no arguments - probably a constant.
    XPrim _ p
     -> evalPrim' p []

    -- Lambdas cannot be evaluated any further;
    -- throw away the type and keep the current heap
    XLam _ n _ x
     -> return (VFun h n x)

    -- Evaluate definition, put it into heap, then evaluate "in" part
    XLet _ n d i
     -> do  d' <- go h d
            let h' = Map.insert n d' h
            go h' i
 where
  go = eval evalPrim


  evalPrim' p vs
   = wrapException vs $ evalPrim p vs

  wrapException vs ret
   = case ret of
      Left err
       | ev : _ <- filter isException vs
       -> return ev
       | otherwise
       -> Left err
      Right v
       -> return v

  isException (VBase (VError _)) = True
  isException _                  = False


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
        :: (Hashable n, Eq n)
        => EvalPrim a n p
        -> Value a n p
        -> Value a n p
        -> Either (RuntimeError a n p) (Value a n p)
applyValues evalPrim f arg
 = case f of
    VFun hh nm x
           -- Evaluate expression with argument added to heap
     -> eval evalPrim (Map.insert nm arg hh) x
    _
     -> Left (RuntimeErrorBadApplication f arg)


-- | Apply a value to a bunch of arguments
applies :: (Hashable n, Eq n)
        => EvalPrim a n p
        -> Value a n p
        -> [Value a n p]
        -> Either (RuntimeError a n p) (Value a n p)
applies evalPrim = foldM (applyValues evalPrim)

-- | Evaluate all expression bindings, collecting up expression heap as we go
evalExps
        :: (Hashable n, Eq n)
        => EvalPrim a n p
        -> Heap     a n p
        -> [(Name n, Exp a n p)]
        -> Either (RuntimeError a n p) (Heap a n p)

evalExps _ env []
 = return env

evalExps evalPrim env ((n,x):bs)
 = do   v    <- eval evalPrim env x
        evalExps evalPrim (Map.insert n v env) bs


