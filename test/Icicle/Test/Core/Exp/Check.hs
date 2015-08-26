{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Check where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Common.Exp
import           Icicle.Common.Type
import           Icicle.Core.Exp
import           Icicle.Core.Exp.Combinators

import           P

import qualified    Data.Map as Map

import           System.IO

import           Test.QuickCheck


-- Prefixing an expression with its own let doesn't affect type checking
-- =====================

prop_prefixlet x =
 typeExp0 coreFragment x == typeExp0 coreFragment (xLet (fresh 0) x x)

-- Prefixing a let with a known good expression doesn't affect
prop_prefixletconst x =
 typeExp0 coreFragment x == typeExp0 coreFragment (xLet (fresh 0) (constI 0) x)


-- Wrapping in a lambda does affect typechecking, but not *whether* type exists
prop_lamwrap x =
 isRight (typeExp0 coreFragmentWorkerFun x) == isRight (typeExp0 coreFragmentWorkerFun (xLam (fresh 0) IntT x))


-- Try to build an expression for type.
-- This is only testing that the generator succeeds with relatively few missed cases.
prop_genExpForType t =
 forAll (tryExpForType (FunT [] t) Map.empty)
 $ \x -> typeExp0 coreFragment x == Right (FunT [] t) ==> True

-- Again, try generating two with the same type.
prop_genExpForType2 t =
 forAll (tryExpForType (FunT [] t) Map.empty) $ \x ->
 forAll (tryExpForType (FunT [] t) Map.empty) $ \y ->
 typeExp0 coreFragment x == Right (FunT [] t) && typeExp0 coreFragment y == Right (FunT [] t) ==> True





return []
tests :: IO Bool
tests = $quickCheckAll
