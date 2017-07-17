{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Simp where

import Icicle.Internal.Pretty (pretty)
import Icicle.Test.Gen.Core.Program
import Icicle.Test.Arbitrary.Data
import Hedgehog hiding (Var)

import           Icicle.Core.Eval.Exp
import qualified Icicle.Core.Exp.Simp               as CoreSimp
import           Icicle.Core.Exp (coreFragment)
import           Icicle.Common.Exp
import qualified Icicle.Common.Exp.Simp.Beta        as Beta
import qualified Icicle.Common.Exp.Simp.ANormal     as ANormal
import qualified Icicle.Common.Fresh                as Fresh
import           Icicle.Common.Base

import           P

import           System.IO

-- TODO XXX FIXME: argh! beta isn't being tested properly because we aren't generating applied lambdas!

-- Performing beta reduction
prop_beta_evaluation = property $ do
  x <- forAll (fst <$> genExpTop)
  let x' = Beta.beta Beta.isSimpleValue x
  annotate (show $ pretty x)
  annotate (show $ pretty x')
  eval0 evalPrim x === eval0 evalPrim x'

-- Beta reduction preserves type
prop_beta_type = property $ do
  x <- forAll (fst <$> genExpTop)
  typeExp0 coreFragment x === typeExp0 coreFragment ( Beta.beta Beta.isSimpleValue x)

-- Reduce regardless of whether it's a value
prop_beta_always_evaluation = property $ do
  x <- forAll (fst <$> genExpTop)
  let x' = Beta.beta (const True) x
  annotate (show $ pretty x)
  annotate (show $ pretty x')
  eval0 evalPrim x === eval0 evalPrim x'

-- Converting all beta reductions to lets
prop_betaToLets_evaluation = property $ do
  x <- forAll (fst <$> genExpTop)
  let x' = Beta.betaToLets () x
  annotate (show $ pretty x)
  annotate (show $ pretty x')
  eval0 evalPrim x === eval0 evalPrim x'

-- Beta reduction preserves type
prop_betaToLets_type = property $ do
  x <- forAll (fst <$> genExpTop)
  typeExp0 coreFragment x === typeExp0 coreFragment ( Beta.betaToLets () x)





-- Converting to a-normal form
prop_anormal_form_evaluation = property $ do
  x <- forAll (fst <$> genExpTop)
  eval0 evalPrim x === eval0 evalPrim
   ( snd
   $ Fresh.runFresh (ANormal.anormal () x)
                    (Fresh.counterNameState (NameBase . Var "anf") 0))


-- Converting to a-normal form preserves type
prop_anormal_form_type = property $ do
  x <- forAll (fst <$> genExpTop)
  let x' = snd
         $ Fresh.runFresh (ANormal.anormal () x)
                          (Fresh.counterNameState (NameBase . Var "anf") 0)
  annotate (show $ pretty x)
  annotate (show $ pretty x')
  typeExp0 coreFragment x === typeExp0 coreFragment x'

-- Core simplification preserves type
prop_core_simp_type = property $ do
  x <- forAll (fst <$> genExpTop)
  let simple = snd
             $ Fresh.runFresh (CoreSimp.simp () Beta.isSimpleValue x)
                              (Fresh.counterNameState (NameBase . Var "anf") 0)
  annotate (show . pretty $ x)
  annotate (show . pretty $ simple)
  typeExp0 coreFragment x === typeExp0 coreFragment simple

-- Core simplification preserves result
prop_core_simp_eval = property $ do
  x <- forAll (fst <$> genExpTop)
  eval0 evalPrim x === eval0 evalPrim
   ( snd
   $ Fresh.runFresh (CoreSimp.simp () x)
                    (Fresh.counterNameState (NameBase . Var "anf") 0))


return []
tests :: IO Bool
tests = checkParallel $$(discover)
