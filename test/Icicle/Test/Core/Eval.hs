{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Eval where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Exp
import           Icicle.Core.Eval.Exp

import           P

import           System.IO

import           Test.QuickCheck


-- Well typed programs don't go wrong
-- =====================

prop_progress x =
 isRight     (checkExp0 x)
 ==> isRight (eval0 x)

-- Prefixing a let with a fresh name doesn't change the semantics,
-- except for functions where the heap is affected.
-- It wouldn't actually change the *execution* of the function, but the
-- heap still looks different. 
-- =====================

prop_prefixlet x =
 eval0 x `equalExceptFunctionsE` eval0 (XLet (fresh 0) x x)


-- Constant evaluates to constant. How quaint.
-- =====================
prop_const i =
 eval0 ((XPrim $ PrimConst $ PrimConstInt i) :: Exp Var) == Right (VInt i)

-- And likewise, putting anything that typechecks before the constant still evalutes fine
-- =====================
prop_constprefix x i =
 isRight (checkExp0 x)
 ==> eval0 (XLet (fresh 0) x (XPrim $ PrimConst $ PrimConstInt i)) == Right (VInt i)



return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000})
