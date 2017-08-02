{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.Flatten where

import           Icicle.Test.Gen.Core.Value
import           Icicle.Test.Gen.Core.Type
import           Icicle.Test.Gen.Core.Program
import           Icicle.Test.Arbitrary.Data
import           Icicle.Test.Arbitrary.Core (testFresh, testFreshT)

import           Hedgehog hiding (Var, eval)

import qualified Icicle.Core.Eval.Exp       as XV

import qualified Icicle.Core.Program.Check as Core
import qualified Icicle.Avalanche.Check     as Check
import qualified Icicle.Avalanche.Program   as AP
import qualified Icicle.Avalanche.FromCore  as AC
import qualified Icicle.Avalanche.Eval      as AE
import qualified Icicle.Avalanche.Prim.Eval as AE
import qualified Icicle.Avalanche.Prim.Flat as AF
import qualified Icicle.Avalanche.Statement.Flatten   as AF
import qualified Icicle.Avalanche.Simp                    as Avalanche

import           Icicle.Internal.Pretty (pretty)

import qualified Icicle.Compiler as P


import           P

import           System.IO

-- We need a way to differentiate stream variables from scalars
namer = AC.namerText (flip Var 0)


prop_flatten_commutes_check = property $ do
 t      <- forAll genInputType
 o      <- forAll genOutputType
 core   <- forAll (programForStreamType t o)
 annotate (show $ pretty core)
 _      <- evalEither $ Core.checkProgram core
 let aval = P.coreAvalanche core
 annotate (show $ pretty aval)
 flat <- evalEither $ P.flattenAvalancheUntyped aval
 annotate (show $ pretty flat)
 flatT <- evalEither $ P.checkAvalanche flat
 annotate (show $ pretty flatT)
 simp <- evalEither $ first (show . pretty) $ P.simpFlattened Avalanche.defaultSimpOpts flatT
 annotate (show $ pretty simp)
 _ <- evalEither $ Check.checkProgram AF.flatFragment simp
 return ()


-- Flattening - removing all folds keeps value same
prop_flatten_commutes_value = property $ do
 t      <- forAll genInputType
 o      <- forAll genOutputType
 p      <- forAll (programForStreamType t o)
 (vs,d) <- forAll (inputsForType t)

 let p' = testFresh "fromCore" $ AC.programFromCore namer p
 let eval xp = AE.evalProgram xp d vs
 let simp = testFreshT "anf" (AF.flatten () $ AP.statements p')

 case simp of
  Left e -> do
   annotate (show e)
   annotate (show $ pretty p')
   failure
  Right s' -> do
   annotate ("Avalanche:\n" <> show (pretty p'))
   annotate ("Flat:\n" <> show (pretty s'))
   let xv' = eval XV.evalPrim p'
   let fv' = eval AE.evalPrim p' { AP.statements = s'}
   first show xv' === first show fv'



prop_flatten_simp_commutes_value = property $ do
 t <- forAll genInputType
 o <- forAll genOutputType
 p <- forAll (programForStreamType t o)
 x <- forAll (inputsForType t)
 flatten_simp_commutes_value p x

flatten_simp_commutes_value p (vs, d) = do
  let aval = P.coreAvalanche p
  let flat = P.coreFlatten p
  case flat of
   Left e -> do
    annotate (show e)
    annotate (show $ pretty aval)
    failure
   Right flat' -> do
    annotate (show $ pretty aval)
    annotate (show $ pretty flat')
    eval XV.evalPrim aval `compareEvalResult` eval AE.evalPrim flat'
 where
  eval xp  = AE.evalProgram xp d vs
  compareEvalResult xv yv = do
    let xv' = first show xv
    let yv' = first show yv
    xv' === yv'

return []
tests :: IO Bool
tests = checkParallel $$(discover)

