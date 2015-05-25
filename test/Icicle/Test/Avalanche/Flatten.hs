{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.Flatten where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Program.Check
import qualified Icicle.Core.Eval.Exp       as XV

import qualified Icicle.Avalanche.Program   as AP
import qualified Icicle.Avalanche.FromCore  as AC
import qualified Icicle.Avalanche.Eval      as AE
import qualified Icicle.Avalanche.Statement.Flatten   as AF
import qualified Icicle.Avalanche.Prim.Eval as AE

import           Icicle.Common.Base
import qualified Icicle.Common.Fresh                as Fresh

import           Icicle.Internal.Pretty
import           Data.Either.Combinators

import           P

import           System.IO

import           Test.QuickCheck

-- We need a way to differentiate stream variables from scalars
namer = AC.namerText (flip Var 0)


-- Flattening - removing all folds keeps value same
prop_flatten_commutes_value t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,d) ->
    P.isRight     (checkProgram p) ==>
     let p' = AC.programFromCore namer p

         eval xp = AE.evalProgram xp d vs

         simp = Fresh.runFreshT
                        (AF.flatten $ AP.statements p')
                        (Fresh.counterNameState (Name . Var "anf") 0)
     in case simp of
         Left e
          -> counterexample (show e)
           $ counterexample (show $ pretty p')
             False
         Right (_, s')
          -> counterexample (show $ pretty p')
           $ counterexample (show $ pretty s')
             (mapLeft show (eval XV.evalPrim p') === mapLeft show (eval AE.evalPrim p' { AP.statements = s'}))

{-
-- A well typed core program is well typed under Avalanche
zprop_flatten_commutes_check t =
 forAll (programForStreamType t)
 $ \p ->
    isRight     (checkProgram p) ==>
     let conv = AC.programFromCore namer p in
     case AC.checkProgram coreFragment conv of
      Right _
       -> property True
      Left err
       -> counterexample (show err)
        $ counterexample (show $ pretty p)
        $ counterexample (show $ pretty conv) False
     
-}

return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10000})

