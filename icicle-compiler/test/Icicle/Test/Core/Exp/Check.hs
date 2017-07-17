{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Check where

import Icicle.Internal.Pretty
import Icicle.Test.Gen.Core.Type
import Icicle.Test.Gen.Core.Program
import Icicle.Test.Arbitrary.Data
import Hedgehog

import           Icicle.Common.Exp
import           Icicle.Common.Type
import           Icicle.Core.Exp
import           Icicle.Core.Exp.Combinators

import           P

import           System.IO


-- Generation is well-typed
prop_generation_welltyped = property $ do
  (x,t) <- forAllWith (show . pretty) genExpTop
  typeExp0 coreFragment x === Right (FunT [] t)

-- Prefixing an expression with its own let doesn't affect type checking
-- =====================

prop_prefixlet = property $ do
  x <- forAll (fst <$> genExpTop)
  typeExp0 coreFragment x === typeExp0 coreFragment (xLet (fresh 0) x x)

-- Prefixing a let with a known good expression doesn't affect
prop_prefixletconst = property $ do
  x <- forAll (fst <$> genExpTop)
  typeExp0 coreFragment x === typeExp0 coreFragment (xLet (fresh 0) (constI 0) x)


-- Wrapping in a lambda does affect typechecking, but not *whether* type exists
prop_lamwrap = property $ do
  x <- forAll (fst <$> genExpTop)
  isRight (typeExp0 coreFragmentWorkerFun x) === isRight (typeExp0 coreFragmentWorkerFun (xLam (fresh 0) IntT x))


-- Try to build an expression for type.
-- This is only testing that the generator succeeds with relatively few missed cases.
prop_genExpForType = property $ do
  t <- forAll genValType
  x <- forAll (genExpForTypeTop t)
  typeExp0 coreFragment x === Right (FunT [] t)




return []
tests :: IO Bool
tests = checkParallel $$(discover)
