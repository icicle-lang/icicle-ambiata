{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Check where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Exp
import           Icicle.Core.Type

import           P

import           System.IO

import           Test.QuickCheck


-- Prefixing an expression with its own let doesn't affect type checking
-- =====================

prop_prefixlet x =
 checkExp0 x == checkExp0 (XLet (fresh 0) x x)

-- Prefixing a let with a known good expression doesn't affect
prop_prefixletconst x =
 checkExp0 x == checkExp0 (XLet (fresh 0) (XPrim $ PrimConst $ PrimConstInt 0) x)


-- Wrapping in a lambda does affect type, but not *whether* type exists
prop_lamwrap x =
 isRight (checkExp0 x) == isRight (checkExp0 (XLam (fresh 0) IntT x))



return []
tests :: IO Bool
tests = $quickCheckAll


