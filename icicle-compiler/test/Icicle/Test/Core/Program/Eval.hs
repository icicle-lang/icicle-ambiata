{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Program.Eval where

import           Icicle.Test.Arbitrary
-- import           Icicle.Core.Program.Program
import           Icicle.Core.Program.Check
-- import qualified Icicle.Core.Eval.Exp       as XV
import qualified Icicle.Core.Eval.Program   as PV
import           Icicle.Internal.Pretty

import           Icicle.Common.Eval

import           Icicle.Data.Time

import           P

import           System.IO

import           Test.QuickCheck

-- Just choose some date; it doesn't matter
someTime = unsafeTimeOfYMD 2015 1 1
evalContext = EvalContext someTime 100

-- Well typed programs don't go wrong
-- =====================
--
-- Restrict it to only good programs.
prop_progress t =
 forAll (programForStreamType t)
 $ \p ->
    let ev = PV.eval evalContext [] p
    in counterexample (show $ pretty p)
     $ counterexample (show ev)
     $ isRight     (checkProgram p) ==> isRight ev


-- Evaluate on actual input
prop_progress_values t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,ctx) ->
    let ev = PV.eval ctx vs p
    in counterexample (show $ pretty p)
     $ counterexample (show ev)
     $ isRight     (checkProgram p) ==> isRight ev



return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal checkArgs
