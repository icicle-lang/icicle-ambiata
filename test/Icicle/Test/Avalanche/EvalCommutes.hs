{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.EvalCommutes where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Program.Check
import qualified Icicle.Core.Eval.Exp       as XV
import qualified Icicle.Core.Eval.Program   as PV

import qualified Icicle.Avalanche.FromCore  as AC
import qualified Icicle.Avalanche.Eval      as AE

import           Icicle.Internal.Pretty

import           P

import           System.IO

import           Test.QuickCheck
import           Data.List (sort)

-- We need a way to differentiate stream variables from scalars
namer = AC.namerText (flip Var 0)


-- A well typed core program evaluates to a value in avalanche
prop_eval_right t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,d) ->
    isRight     (checkProgram p) ==>
     isRight $ AE.evalProgram XV.evalPrim d vs $ AC.programFromCore namer p


-- going to core doesn't affect value
prop_eval_commutes_value t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,d) -> counterexample (show $ pretty p) $
    isRight     (checkProgram p) ==>
     case (AE.evalProgram XV.evalPrim d vs $ AC.programFromCore namer p, PV.eval d vs p) of
      (Right (_, aval), Right cres)
       ->   aval === PV.value   cres
      (_, Left _)
       -> counterexample "Impossible: Core evaluation or type check must be wrong" False
      (Left err, _)
       -> counterexample ("Avalanche runtime error " <> show err) False


-- going to core doesn't affect history
prop_eval_commutes_history t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,d) ->
    isRight     (checkProgram p) ==>
     case (AE.evalProgram XV.evalPrim d vs $ AC.programFromCore namer p, PV.eval d vs p) of
      (Right (abg, _), Right cres)
       ->  (sort abg)  === (sort $ PV.history cres)
      _
       -> property False




return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})

