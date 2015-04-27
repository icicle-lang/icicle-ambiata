-- This is a very simple expression evaluator, the idea being to serve as a spec
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Core.Eval.Exp (
      Value (..)
    , RuntimeError(..)
    , eval0
    , eval
    ) where

import Icicle.Core.Base
import Icicle.Core.Exp

import              P

import qualified    Data.Map as Map


type Heap n
 = Map.Map (Name n) (Value n)


data Value n
 = VInt   Int
 | VArray [Value n]
 | VPair  (Value n) (Value n)
 | VFun   (Heap n)  (Name n)  (Exp n)
 deriving (Show, Eq)

data RuntimeError n
 = RuntimeErrorBadApplication (Value n) (Value n)
 | RuntimeErrorVarNotInHeap (Name n)
 | RuntimeErrorPrimBadArgs Prim [Value n]
 deriving (Show, Eq)

-- Big step evaluation
eval0 :: Ord n => Exp n -> Either (RuntimeError n) (Value n)
eval0 = eval Map.empty

-- Big step evaluation
eval :: Ord n
     => Heap n
     -> Exp n
     -> Either (RuntimeError n) (Value n)

eval h xx
 = case xx of
    XVar n
     -> maybeToRight (RuntimeErrorVarNotInHeap n)
                     (Map.lookup n h)

    XApp{}
     | Just (p, args) <- takePrimApps xx
     -> do  vs <- mapM (eval h) args
            evalPrim p vs

    XApp p q
     -> do  p' <- eval h p
            q' <- eval h q
            case p' of
             VFun hh nm x
              -> eval (Map.insert nm q' hh) x
             _
              -> Left (RuntimeErrorBadApplication p' q')

    XPrim p
     -> evalPrim p []

    XLam n _ x
     -> return (VFun h n x)

    XLet n d i
     -> do  d' <- eval h d
            eval (Map.insert n d' h) i


 where
  evalPrim p vs
   = case (p,vs) of

     (PrimArith PrimArithPlus,      [VInt i, VInt j])
      -> return $ VInt (i+j)

     (PrimArith PrimArithMinus,     [VInt i, VInt j])
      -> return $ VInt (i-j)

     (PrimConst (PrimConstInt i),   [])
      -> return $ VInt i

     _
      -> Left (RuntimeErrorPrimBadArgs p vs) 
     
