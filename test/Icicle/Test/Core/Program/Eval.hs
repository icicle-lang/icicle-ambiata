{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Program.Eval where

import           Icicle.Test.Core.Arbitrary ()
-- import           Icicle.Core.Program.Program
import           Icicle.Core.Program.Check
-- import qualified Icicle.Core.Eval.Exp       as XV
import qualified Icicle.Core.Eval.Program   as PV

import           Icicle.Data.DateTime

import           P

import           System.IO

import           Test.QuickCheck


-- Just choose some date; it doesn't matter
someDate = dateOfYMD 2015 1 1

-- Well typed programs don't go wrong
-- =====================

-- This is nice to know, but generating all programs and restricting to only those
-- that type check is just infeasible.
--
-- We need another generator that only makes valid programs
zprop_progress x =
 isRight     (checkProgram x)
 ==> isRight (PV.eval someDate [] x)

-- Instead, try saying if it has a runtime error, it can't be type safe
-- Most randomly generated programs will have runtime errors, and won't type check
prop_progress x =
 isLeft      (PV.eval someDate [] x)
 ==> isLeft  (checkProgram x)

-- It would be nice to say something about inputs,
-- but we would need to be able to generate well typed values.
-- A well typed program can still error if its inputs are of the wrong type.



return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})
