{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Eval where

import Icicle.Test.Gen.Core.Program
import Icicle.Test.Arbitrary.Data

import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Icicle.Test.Arbitrary

import qualified Icicle.Core.Exp    as X
import           Icicle.Core.Exp.Combinators
import           Icicle.Core.Eval.Exp
import           Icicle.Common.Exp
import           Icicle.Common.Base
import           Icicle.Common.Value

import           P

import           System.IO


-- Well typed programs don't go wrong
-- =====================

prop_progress = property $ do
  x <- forAll (fst <$> genExpTop)
  assert $ isRight (eval0 evalPrim x)

-- Prefixing a let with a fresh name doesn't change the semantics,
-- except for functions where the heap is affected.
-- It wouldn't actually change the *execution* of the function, but the
-- heap still looks different. 
-- =====================

prop_prefixlet = property $ do
  x <- forAll (fst <$> genExpTop)
  assert (eval0 evalPrim x `equalExceptFunctionsE` eval0 evalPrim (xLet (fresh 0) x x))


-- Constant evaluates to constant. How quaint.
-- =====================
prop_const = property $ do
  i <- forAll (Gen.integral $ Range.linear (-100) 100)
  eval0 evalPrim (constI i :: X.Exp () Var) === Right (VBase $ VInt i)

-- And likewise, putting anything that typechecks before the constant still evalutes fine
-- =====================
prop_constprefix = property $ do
  x <- forAll (fst <$> genExpTop)
  i <- forAll (Gen.integral $ Range.linear (-100) 100)
  eval0 evalPrim (xLet (fresh 0) x (constI i)) === Right (VBase $ VInt i)


return []
tests :: IO Bool
tests = checkParallel $$(discover)
