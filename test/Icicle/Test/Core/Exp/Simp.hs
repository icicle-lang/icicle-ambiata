{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Exp.Simp where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Eval.Exp
import           Icicle.Common.Exp
import qualified Icicle.Common.Exp.Simp.Beta        as Beta
import qualified Icicle.Common.Exp.Simp.ANormal     as ANormal
import qualified Icicle.Common.Fresh                as Fresh
import           Icicle.Common.Base

import           Icicle.Internal.Pretty

import           P

import           System.IO

import           Test.QuickCheck


-- Performing beta reduction
prop_beta_evaluation
 = withTypedExp
 $ \x _
 -> let x' = Beta.beta Beta.isSimpleValue x
    in  counterexample (show $ pretty x)
      $ counterexample (show $ pretty x')
       (eval0 evalPrim x === eval0 evalPrim x')

-- Converting to a-normal form
prop_anormal_form_evaluation
 = withTypedExp
 $ \x _
 -> eval0 evalPrim x == eval0 evalPrim
   ( snd
   $ Fresh.runFresh (ANormal.anormal x)
                    (Fresh.counterNameState (Name . Var "anf") 0))


return []
tests :: IO Bool
tests = $quickCheckAll
