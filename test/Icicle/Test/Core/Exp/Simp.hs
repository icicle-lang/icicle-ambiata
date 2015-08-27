{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Simp where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Eval.Exp
import qualified Icicle.Core.Exp.Simp               as CoreSimp
import           Icicle.Core.Exp (coreFragment)
import           Icicle.Common.Exp
import qualified Icicle.Common.Exp.Simp.Beta        as Beta
import qualified Icicle.Common.Exp.Simp.ANormal     as ANormal
import qualified Icicle.Common.Fresh                as Fresh
import           Icicle.Common.Base

import           Icicle.Internal.Pretty

import           P

import           System.IO

import           Test.QuickCheck

-- TODO XXX FIXME: argh! beta isn't being tested properly because we aren't generating applied lambdas!

-- Performing beta reduction
prop_beta_evaluation
 = withTypedExp
 $ \x _
 -> let x' = Beta.beta Beta.isSimpleValue x
    in  counterexample (show $ pretty x)
      $ counterexample (show $ pretty x')
       (eval0 evalPrim x === eval0 evalPrim x')

-- Beta reduction preserves type
prop_beta_type
 = withTypedExp
 $ \x _
 -> typeExp0 coreFragment x == typeExp0 coreFragment
   ( Beta.beta Beta.isSimpleValue x)

-- Reduce regardless of whether it's a value
prop_beta_always_evaluation
 = withTypedExp
 $ \x _
 -> let x' = Beta.beta (const True) x
    in  counterexample (show $ pretty x)
      $ counterexample (show $ pretty x')
       (eval0 evalPrim x === eval0 evalPrim x')


-- Converting all beta reductions to lets
prop_betaToLets_evaluation
 = withTypedExp
 $ \x _
 -> let x' = Beta.betaToLets () x
    in  counterexample (show $ pretty x)
      $ counterexample (show $ pretty x')
       (eval0 evalPrim x === eval0 evalPrim x')

-- Beta reduction preserves type
prop_betaToLets_type
 = withTypedExp
 $ \x _
 -> typeExp0 coreFragment x == typeExp0 coreFragment
   ( Beta.betaToLets () x)





-- Converting to a-normal form
prop_anormal_form_evaluation
 = withTypedExp
 $ \x _
 -> eval0 evalPrim x === eval0 evalPrim
   ( snd
   $ Fresh.runFresh (ANormal.anormal () x)
                    (Fresh.counterNameState (Name . Var "anf") 0))


-- Converting to a-normal form preserves type
prop_anormal_form_type
 = withTypedExp
 $ \x _
 -> let x' = snd
           $ Fresh.runFresh (ANormal.anormal () x)
                            (Fresh.counterNameState (Name . Var "anf") 0)
    in  counterexample (show $ pretty x)
      $ counterexample (show $ pretty x')
      ( typeExp0 coreFragment x === typeExp0 coreFragment x')

-- Core simplification preserves type
prop_core_simp_type
  = withTypedExp
  $ \x _
  -> let simple = snd
                $ Fresh.runFresh (CoreSimp.simp () Beta.isSimpleValue x)
                                 (Fresh.counterNameState (Name . Var "anf") 0)
     in  counterexample (show . pretty $ x)
       $ counterexample (show . pretty $ simple)
       ( typeExp0 coreFragment x == typeExp0 coreFragment simple)

-- Core simplification preserves result
prop_core_simp_eval
 = withTypedExp
 $ \x _
 -> eval0 evalPrim x === eval0 evalPrim
   ( snd
   $ Fresh.runFresh (CoreSimp.simp () Beta.isSimpleValue x)
                    (Fresh.counterNameState (Name . Var "anf") 0))


return []
tests :: IO Bool
tests = $quickCheckAll
