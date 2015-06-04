{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.PrettyParse where

import           Icicle.Internal.Pretty
import           Icicle.Source.Parser

import           Icicle.Test.Source.Arbitrary ()

import           P

import qualified Data.Text as T

import           System.IO

import           Test.QuickCheck


prop_parse_pretty_same q
 = counterexample pp
 $ counterexample pp'
 $ parsed === Right q
 where
  pp = show $ pretty q
  t  = T.pack pp

  parsed = parseQueryTop t

  pp'  = case parsed of
          Left e -> show e
          Right q' -> show $ pretty q'



return []
tests :: IO Bool
tests = $quickCheckAll
