-- | Helper combinators for constructing core expressions
-- These are quite useful until we have a real source language and parser.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Core.Exp.Combinators where

import              Icicle.Core.Base
import              Icicle.Core.Type
import              Icicle.Core.Exp.Prim
import              Icicle.Core.Exp.Exp

import              System.IO.Unsafe
import              System.IO
import              Data.IORef
import              P
import              Data.Text   as T

-- | Right-associative application
($~) :: Exp n -> Exp n -> Exp n
($~) x y = XApp x y
infixr 0 $~

-- | Left-associative application
(@~) :: Exp n -> Exp n -> Exp n
(@~) x y = XApp x y
infixl 0 @~


var  :: n -> Exp n
var = XVar . Name


constI :: Int -> Exp n
constI = XPrim . PrimConst . PrimConstInt

constB :: Bool -> Exp n
constB = XPrim . PrimConst . PrimConstBool


prim2 :: Prim -> Exp n -> Exp n -> Exp n
prim2 p x y = XPrim p @~ x @~ y

(+~) :: Exp n -> Exp n -> Exp n
(+~) = prim2 (PrimArith PrimArithPlus)
infixl 6 +~

(-~) :: Exp n -> Exp n -> Exp n
(-~) = prim2 (PrimArith PrimArithMinus)
infixl 6 -~

(/~) :: Exp n -> Exp n -> Exp n
(/~) = prim2 (PrimArith PrimArithDiv)
infixl 7 /~

(>~) :: Exp n -> Exp n -> Exp n
(>~) = prim2 (PrimRelation PrimRelationGt)
infix 4 >~

(>=~) :: Exp n -> Exp n -> Exp n
(>=~) = prim2 (PrimRelation PrimRelationGe)
infix 4 >=~

(<~) :: Exp n -> Exp n -> Exp n
(<~) = prim2 (PrimRelation PrimRelationLt)
infix 4 <~

(<=~) :: Exp n -> Exp n -> Exp n
(<=~) = prim2 (PrimRelation PrimRelationLe)
infix 4 <=~

(==~) :: Exp n -> Exp n -> Exp n
(==~) = prim2 (PrimRelation PrimRelationEq)
infix 4 ==~

(/=~) :: Exp n -> Exp n -> Exp n
(/=~) = prim2 (PrimRelation PrimRelationNe)
infix 4 /=~


lam :: ValType -> (Exp Text -> Exp Text) -> Exp Text
lam t f
 = unsafePerformIO
 $ do   n <- lam_get_counter
        let v = NameMod "_" $ Name $ T.pack $ show n
        return $  XLam v t $ f $ XVar v
 

-- | Unsafe fresh name generation for lambdas
-- !!!
-- I think there's a better way to do this with
-- tying some knots and getting free variables
-- but free variables isn't implemented yet.
lam_get_counter :: IO Int
lam_get_counter
 = do   n <- readIORef lam_free_counter
        modifyIORef lam_free_counter (+1)
        return n

{-# NOINLINE lam_free_counter #-}
lam_free_counter :: IORef Int
lam_free_counter = unsafePerformIO $ newIORef 0

