{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.SimpCommutes where

import           Icicle.Test.Arbitrary

import           Icicle.Core.Program.Check
import qualified Icicle.Core.Eval.Exp       as XV

import qualified Icicle.Avalanche.FromCore  as AC
import qualified Icicle.Avalanche.Eval      as AE
import qualified Icicle.Avalanche.Simp      as AS

import           Icicle.Internal.Pretty

import           P

import           System.IO

import           Test.QuickCheck

-- We need a way to differentiate stream variables from scalars
namer = AC.namerText (flip Var 0)


-- Simplifying the Avalanche doesn't affect the value
prop_simp_commutes_value t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForTypeRaw t)
 $ \(vs,d) ->
    isRight     (checkProgram p) ==>
     let p' = testFresh "fromCore" $ AC.programFromCore namer p

         simp = testFresh "anf" $ AS.simpAvalanche () p'
         eval = AE.evalProgram XV.evalPrim d vs
     in counterexample (show $ pretty p')
      $ counterexample (show $ pretty simp)
       (eval p' === eval simp)


return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
