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
import           Icicle.Internal.Pretty


import           P

import           System.IO

import           Test.QuickCheck


-- Condensing might affect the contents of the type error, but not whether there's an error
-- =====================
prop_condense_type t =
 forAll (programForStreamType t)
 $ \p ->
   counterexample (show $ pretty p)
 $ counterexample (show $ pretty (condenseProgram () p))
 (isRight (checkProgram p) == isRight (checkProgram $ condenseProgram () p))

-- Condensing doesn't affect evaluation value.
-- It does affect the history, but not the value
-- =====================
prop_condense_eval t =
 forAll (programForStreamType t)
 $ \p1 ->
 forAll (inputsForType t)
 $ \(vs,d) ->
   counterexample (show $ pretty p1)
 $ counterexample (show $ pretty (condenseProgram () p1))
 $ case (PV.eval d vs p1, PV.eval d vs (condenseProgram () p1)) of
    (Left _, Left _)
     -- it is tempting to try "e1 === e2" here, but some of the errors have names inside them.
     -- they should be the same error modulo renaming..
     -> property True
    (Right v1, Right v2) -> PV.value v1 === PV.value v2
    (_,_)                -> property False




return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 10, maxDiscardRatio = 10000})
