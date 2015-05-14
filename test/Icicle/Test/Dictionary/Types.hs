{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Dictionary.Types where

import Icicle.Dictionary

import Icicle.Core.Program.Check

import qualified    Icicle.Internal.Pretty as PP

import              P
import              System.IO
import              Test.QuickCheck

check_virtual prog
 = case checkProgram prog of
    Left err 
     -> counterexample ("With program:" <> show (PP.pretty prog))
      $ counterexample ("Got typechecking error:" <> show (PP.pretty err))
      $ False
    Right _
     -> property True

check_attributes (Dictionary attrs)
 = conjoin
 $ fmap check attrs
 where
  check (_, ConcreteDefinition _)
   = property True
  check (_, VirtualDefinition virtual)
   = check_virtual (program virtual)


prop_virtuals_typecheck
 = once (check_attributes demographics)

return []
tests :: IO Bool
tests = $quickCheckAll

