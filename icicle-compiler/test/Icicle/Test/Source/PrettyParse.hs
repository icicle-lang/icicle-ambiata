{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.PrettyParse where

import           Icicle.Test.Arbitrary
import           Icicle.Internal.Pretty
import           Icicle.Source.Parser

import           P

import qualified Data.Text as T

import           System.IO

import           Test.QuickCheck

import           Icicle.Source.Query

prop_parse_pretty_same :: Property
prop_parse_pretty_same
 -- Ill-typed programs will be printed/parsed incorrectly if they have operators
 -- with the wrong number of arguments - for example (!) applied to no arguments.
 -- However, by setting the tableflip ratio to 0,
 -- it will generate only the correct number of arguments
 = forAll (genQueryWithFeatureTypedGen 0) check_pretty

check_pretty :: QueryWithFeature -> Property
check_pretty qwf
 = counterexample pp
 $ counterexample pp'
 $ parsed' === Right q
 where
  q  = qwfQueryTop qwf
  pp = show $ pretty q
  t  = T.pack pp

  parsed = parseQueryTop (queryName q) t
  parsed' = second (reannotQT (const ())) parsed

  pp'  = case parsed of
          Left e -> show e
          Right q' -> show $ pretty q'

return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 100)
