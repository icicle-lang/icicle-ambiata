{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Program.Condense where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Program.Check
import           Icicle.Core.Program.Condense
import qualified Icicle.Core.Eval.Program   as PV


import           P

import           System.IO

import           Test.QuickCheck


-- Condensing might affect the contents of the type error, but not whether there's an error
-- =====================
prop_condense_type t =
 forAll (programForStreamType t)
 $ \p ->
 isRight (checkProgram p) == isRight (checkProgram $ condenseProgram p)

-- Condensing doesn't affect evaluation
-- =====================
prop_condense_eval t =
 forAll (programForStreamType t)
 $ \p1 ->
 forAll (inputsForType t)
 $ \(vs,d) ->
 PV.eval d vs p1 == PV.eval d vs (condenseProgram p1)




return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 5, maxDiscardRatio = 10000})
