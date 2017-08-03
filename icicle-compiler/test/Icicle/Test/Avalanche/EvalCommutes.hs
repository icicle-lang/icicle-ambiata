{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.EvalCommutes where

import           Icicle.Test.Arbitrary
import           Icicle.Test.Arbitrary.NanEq ((=~=))

import           Icicle.Core.Program.Check
import qualified Icicle.Core.Eval.Exp       as XV
import qualified Icicle.Core.Eval.Program   as PV

import qualified Icicle.Avalanche.FromCore  as AC
import qualified Icicle.Avalanche.Eval      as AE

import           Icicle.Internal.Pretty

import           P

import           System.IO

import           Test.QuickCheck

-- We need a way to differentiate stream variables from scalars
namer = AC.namerText (flip Var 0)


-- A well typed core program evaluates to a value in avalanche
prop_eval_right t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,d) ->
    isRight     (checkProgram p) ==>
     isRight $ AE.evalProgram XV.evalPrim d vs $ testFresh "fromCore" $ AC.programFromCore namer p


-- going to core doesn't affect value
prop_eval_commutes_value t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,d) -> counterexample (show $ pretty p) $
    isRight     (checkProgram p) ==>
     case (AE.evalProgram XV.evalPrim d vs $ testFresh "fromCore" $ AC.programFromCore namer p, PV.eval d vs p) of
      (Right aval, Right cres)
       ->   aval =~= cres
      (_, Left _)
       -> counterexample "Impossible: Core evaluation or type check must be wrong" False
      (Left err, _)
       -> counterexample ("Avalanche runtime error " <> show err) False



return []
tests :: IO Bool
tests = $checkAllWith TestRunMore checkArgs
