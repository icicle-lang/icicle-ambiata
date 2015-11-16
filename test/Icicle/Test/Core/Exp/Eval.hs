{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Eval where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Test.Arbitrary

import qualified Icicle.Core.Exp    as X
import           Icicle.Core.Exp
import           Icicle.Core.Exp.Combinators
import           Icicle.Core.Eval.Exp
import           Icicle.Common.Exp
import           Icicle.Common.Base
import           Icicle.Common.Value

import           P

import           System.IO

import           Test.QuickCheck


-- Well typed programs don't go wrong
-- =====================

prop_progress =
 withTypedExp $ \x _ -> isRight (eval0 evalPrim x)

-- Inverse: if a program goes wrong, it can't be well typed
prop_progress_inverse x =
     isLeft (eval0 evalPrim x)
 ==> isLeft (typeExp0 coreFragment x)


-- Prefixing a let with a fresh name doesn't change the semantics,
-- except for functions where the heap is affected.
-- It wouldn't actually change the *execution* of the function, but the
-- heap still looks different. 
-- =====================

prop_prefixlet =
 withTypedExp $ \x _ -> eval0 evalPrim x `equalExceptFunctionsE` eval0 evalPrim (xLet (fresh 0) x x)


-- Constant evaluates to constant. How quaint.
-- =====================
prop_const i =
 eval0 evalPrim (constI i :: X.Exp () Var) == Right (VBase $ VInt i)

-- And likewise, putting anything that typechecks before the constant still evalutes fine
-- =====================
prop_constprefix i =
 withTypedExp $ \x _ -> eval0 evalPrim (xLet (fresh 0) x (constI i)) == Right (VBase $ VInt i)


return []
tests :: IO Bool
tests = $quickCheckAll
