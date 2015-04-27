{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Eval where

import           Icicle.Test.Core.Arbitrary ()
import           Icicle.Core.Exp
import           Icicle.Core.Eval.Exp

import           P

import           System.IO

import           Test.QuickCheck


-- Well typed programs don't go wrong
-- =====================

prop_progress x =
 isRight     (checkExp0 x)
 ==> isRight (eval0 x)

return []
tests :: IO Bool
tests = $quickCheckAll


