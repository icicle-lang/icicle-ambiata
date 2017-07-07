{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE PatternGuards#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Psv.Corpus where


import           Icicle.Sea.Eval

import           Icicle.Test.Arbitrary
import           Icicle.Test.Arbitrary.Corpus
import           P

import           System.IO

import           Icicle.Test.Sea.Psv.Base

prop_success_psv_corpus
 | dup <- AllowDupTime
 = testAllCorpus dup genPsvConstants $ \wt psv ->
     testForSuccess dup wt psv

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 100)
