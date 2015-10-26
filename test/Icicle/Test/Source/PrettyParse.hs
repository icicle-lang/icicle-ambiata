{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.PrettyParse where

import           Icicle.Internal.Pretty
import           Icicle.Source.Parser

import           Icicle.Test.Source.Arbitrary

import           P

import qualified Data.Text as T

import           Data.Either.Combinators

import           System.IO

import           Test.QuickCheck

import qualified Icicle.Source.Lexer.Token as T
import           Icicle.Source.Query

import           Icicle.Source.PrettyAnnot

prop_parse_pretty_same :: QueryTop () T.Variable -> Property
prop_parse_pretty_same q
 = counterexample pp
 $ counterexample pp'
 $ parsed' === Right q
 where
  pp = show $ pretty q
  t  = T.pack pp

  parsed = parseQueryTop (queryName q) t
  parsed' = mapRight (reannotQT (const ())) parsed

  pp'  = case parsed of
          Left e -> show e
          Right q' -> show $ pretty q'


prop_annotated_query_prints_well :: QueryWithFeature -> Property
prop_annotated_query_prints_well qwf
 = counterexample (qwfPretty qwf)
 $ case qwfCheck qwf of
    Left _
     -> property Discard
    Right qt'
     -> show' (noAnnotate (pretty (PrettyAnnot qt'))) === show' (noAnnotate (pretty qt'))
 where
 show' = strip . show
 -- Normalise whitespace to a single space.
 strip (' ':' ':xs)  = strip (' ':xs)
 strip (' ':'\n':xs) = strip (' ':xs)
 strip ('\n':xs)     = strip (' ':xs)
 strip (x:xs)     = x:(strip xs)
 strip [] = []


return []
tests :: IO Bool
-- tests = $quickCheckAll
-- Seems like the generator is making too big values
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSize = 10, maxDiscardRatio = 10000})
