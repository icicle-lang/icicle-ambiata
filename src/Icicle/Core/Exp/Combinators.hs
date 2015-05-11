-- | Helper combinators for constructing core expressions
-- These are quite useful until we have a real source language and parser.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Core.Exp.Combinators where

import              Icicle.Common.Base
import              Icicle.Common.Type
import              Icicle.Core.Exp.Prim
import qualified    Icicle.Core.Exp.Exp     as X
import              Icicle.Common.Exp.Exp

import              System.IO.Unsafe
import              System.IO
import              Data.IORef
import              P
import              Data.Text   as T

-- | Right-associative application
($~) :: X.Exp n -> X.Exp n -> X.Exp n
($~) x y = XApp x y
infixr 0 $~

-- | Left-associative application
(@~) :: X.Exp n -> X.Exp n -> X.Exp n
(@~) x y = XApp x y
infixl 0 @~


var  :: n -> X.Exp n
var = XVar . Name


constI :: Int -> X.Exp n
constI = XValue IntT  . VInt

constB :: Bool -> X.Exp n
constB = XValue BoolT . VBool

fstOfSource :: ValType -> X.Exp Text -> X.Exp Text
fstOfSource ty p
 = XPrim (PrimFold (PrimFoldPair ty DateTimeT) ty)
    @~ (lam ty $ \x -> lam DateTimeT $ \_ -> x) @~ p


prim2 :: Prim -> X.Exp n -> X.Exp n -> X.Exp n
prim2 p x y = XPrim p @~ x @~ y

(+~) :: X.Exp n -> X.Exp n -> X.Exp n
(+~) = prim2 (PrimArith PrimArithPlus)
infixl 6 +~

(-~) :: X.Exp n -> X.Exp n -> X.Exp n
(-~) = prim2 (PrimArith PrimArithMinus)
infixl 6 -~

(/~) :: X.Exp n -> X.Exp n -> X.Exp n
(/~) = prim2 (PrimArith PrimArithDiv)
infixl 7 /~

(>~) :: X.Exp n -> X.Exp n -> X.Exp n
(>~) = prim2 (PrimRelation PrimRelationGt)
infix 4 >~

(>=~) :: X.Exp n -> X.Exp n -> X.Exp n
(>=~) = prim2 (PrimRelation PrimRelationGe)
infix 4 >=~

(<~) :: X.Exp n -> X.Exp n -> X.Exp n
(<~) = prim2 (PrimRelation PrimRelationLt)
infix 4 <~

(<=~) :: X.Exp n -> X.Exp n -> X.Exp n
(<=~) = prim2 (PrimRelation PrimRelationLe)
infix 4 <=~

(==~) :: X.Exp n -> X.Exp n -> X.Exp n
(==~) = prim2 (PrimRelation PrimRelationEq)
infix 4 ==~

(/=~) :: X.Exp n -> X.Exp n -> X.Exp n
(/=~) = prim2 (PrimRelation PrimRelationNe)
infix 4 /=~


lam :: ValType -> (X.Exp Text -> X.Exp Text) -> X.Exp Text
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

