{-# LANGUAGE NoImplicitPrelude #-}

module Icicle.Common.Exp.Eval.Small

where

import qualified Data.Map as M

import Icicle.Internal.Pretty
import Icicle.Common.Base
import Icicle.Common.Type
import Icicle.Common.Value
import Icicle.Common.Exp.Exp
import Icicle.Common.Exp.Eval (EvalPrim)
import qualified Icicle.Common.Exp.Eval as Big

import P

import Data.Either.Combinators (mapLeft)


data RuntimeError n p
  = RuntimeErrorPrimBadArgs p [Value n p]
  | RuntimeErrorUnboundVar  (Name n)
  | RuntimeErrorPrimBad p
  | RuntimeErrorPrimEval (Big.RuntimeError n p)
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- | Small step evaluation. Call-by-value for simplicity.
--
eval :: Eq n
     => EvalPrim n p -> Exp n p
     -> Either (RuntimeError n p) (Exp n p)
eval evalPrim xx = case xx of
  -- * Variables must have been substituted away.
  XVar n
    -> Left (RuntimeErrorUnboundVar n)

  -- * Values are fully evaluated.
  XValue{}
    -> return xx

  -- * App where left is lambda and right is value: substitute.
  XApp (XLam n t1 e) (XValue t2 v)
    | t1 == t2
    -> subst n v t1 e

  -- * App where left is lambda: evals right.
  XApp e1@XLam{} e2
    -> fmap (XApp e1) (go e2)

  -- * App where left is other exps: evals left.
  XApp e1 e2
    -> fmap (flip XApp e2) (go e1)

  -- * Primitives with no arguments.
  XPrim p
    -> do x <- mapLeft RuntimeErrorPrimEval $ evalPrim p []
          v <- getBaseValue (RuntimeErrorPrimBad p) x
          t <- typeOfValue  (RuntimeErrorPrimBad p) x
          return (XValue t v)

  -- * Lambdas cannot go any further.
  XLam{}
    -> return xx

  -- * Let where definition is a value: substitute.
  XLet n (XValue t v) e2
    -> subst n v t e2

  -- * Let where definition is something else.
  XLet n e1 e2
    -> do e <- go e1
          return (XLet n e e2)

  where go = eval evalPrim

subst :: Eq n
      => Name n -> BaseValue -> ValType -> Exp n p
      -> Either (RuntimeError n p) (Exp n p)
subst x val t body = case body of
  XVar n
    | x == n
    -> return (XValue t val)

  _ -> return body


