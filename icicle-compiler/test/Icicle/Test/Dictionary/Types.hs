{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Dictionary.Types where

import              Icicle.Dictionary
import              Icicle.Dictionary.Data
import              Icicle.Dictionary.Parse

import              Icicle.Core.Program.Check

import              Icicle.Test.Arbitrary ()

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
  check (DictionaryEntry _ (ConcreteDefinition _))
   = property True
  check (DictionaryEntry _ (VirtualDefinition virtual))
   = check_virtual (program virtual)


prop_virtuals_typecheck
 = once (check_attributes demographics)

prop_dictionary_symmetry attr encoding =
  let original  = DictionaryEntry attr (ConcreteDefinition encoding)
      recreated = parseDictionaryLineV1 (writeDictionaryLineV1 original)
  in (Right original) === recreated

-- This is a bit unecessary but it is important that this ordering does not change.
prop_attribute_ordering (Dictionary attributes) =
  let
    attributes' =
      canonicalOrderOf attributes

    good (DictionaryEntry a _ n) (DictionaryEntry a' _ n')
     | a < a' = True
     | a == a' = n <= n'
     | otherwise = False
  in
    and $ zipWith good attributes' (List.drop 1 attributes')


return []
tests :: IO Bool
tests = $checkAll
