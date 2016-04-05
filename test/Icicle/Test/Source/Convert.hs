{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Convert where

import           Icicle.Test.Arbitrary
import           Icicle.Internal.Pretty
import qualified Icicle.Core.Program.Check      as CCheck

import           P

import           System.IO

import           Test.QuickCheck


prop_convert_ok :: QueryWithFeature -> Property
prop_convert_ok qwf
 = counterexample (qwfPretty qwf)
 $ case qwfCheck qwf of
    Left _
     -> property Discard
    Right qt'
     -> let conv = qwfConvertToCore qwf qt'
        in  counterexample (show conv) $ isRight conv


prop_convert_is_well_typed :: QueryWithFeature -> Property
prop_convert_is_well_typed qwf
 = counterexample (qwfPretty qwf)
 $ case qwfCheck qwf of
    Right qt'
     | Right c' <- qwfConvertToCore qwf qt'
     , check    <- CCheck.checkProgram c'
     -> counterexample (show $ pretty c')
      $ counterexample (show check)
      $ isRight check
    _
     -> property Discard



return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 10)
