{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Eval where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Exp
import           Icicle.Core.Eval.Exp

import           P

import           System.IO

import           Test.QuickCheck


-- Well typed programs don't go wrong
-- =====================

prop_progress =
 withTypedExp $ \x _ -> isRight (eval0 x)

-- Inverse: if a program goes wrong, it can't be well typed
prop_progress_inverse x =
     isLeft (eval0 x)
 ==> isLeft (checkExp0 x)


-- Prefixing a let with a fresh name doesn't change the semantics,
-- except for functions where the heap is affected.
-- It wouldn't actually change the *execution* of the function, but the
-- heap still looks different. 
-- =====================

prop_prefixlet =
 withTypedExp $ \x _ -> eval0 x `equalExceptFunctionsE` eval0 (XLet (fresh 0) x x)


-- Constant evaluates to constant. How quaint.
-- =====================
prop_const i =
 eval0 ((XPrim $ PrimConst $ PrimConstInt i) :: Exp Var) == Right (VInt i)

-- And likewise, putting anything that typechecks before the constant still evalutes fine
-- =====================
prop_constprefix i =
 withTypedExp $ \x _ -> eval0 (XLet (fresh 0) x (XPrim $ PrimConst $ PrimConstInt i)) == Right (VInt i)


return []
tests :: IO Bool
tests = $quickCheckAll
