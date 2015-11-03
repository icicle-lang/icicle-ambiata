{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.Melt where

import           Icicle.Test.Core.Arbitrary

import           Icicle.Avalanche.Statement.Simp.Melt
import           Icicle.Common.Base
import           Icicle.Common.Type

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property


prop_roundtrip_meltValue t =
  forAll (baseValueForType t) $ \v0 ->
    checkMelt   v0 t $ \vs ->
    checkUnmelt vs t $ \v1 -> v1 === v0

checkMelt :: BaseValue -> ValType -> ([BaseValue] -> Property) -> Property
checkMelt v t go =
  case meltValue v t of
    Nothing -> counterexample ("failed to melt:")
             $ counterexample ("  type  = " <> show t)
             $ counterexample ("  value = " <> show v)
             $ failed
    Just vs -> go vs

checkUnmelt :: [BaseValue] -> ValType -> (BaseValue -> Property) -> Property
checkUnmelt vs t go =
  case unmeltValue vs t of
    Nothing -> counterexample ("failed to unmelt:")
             $ counterexample ("  type   = " <> show t)
             $ counterexample ("  values = " <> show vs)
             $ failed
    Just v' -> go v'

return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10})
