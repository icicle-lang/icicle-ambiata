{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Progress where

import           Icicle.Test.Arbitrary
import           Icicle.Internal.Pretty
import           Icicle.Source.Checker.Base
import           Icicle.Source.Checker.Checker
import           Icicle.Source.Query
import           Icicle.Source.Eval
import           Icicle.Source.Type
import qualified Icicle.Source.Lexer.Token as T
import           Icicle.Source.Transform.Desugar

import qualified Icicle.Common.Base          as CB
import qualified Icicle.Common.Type          as CT

import           Icicle.Test.Arbitrary

import           P

import           System.IO

import           Test.QuickCheck

import qualified Data.Map as Map


mkElems :: Map.Map (CB.Name T.Variable) CT.ValType -> CheckEnv () T.Variable
mkElems m = emptyCheckEnv { checkEnvironment = Map.map (function0 . Temporality TemporalityElement . Possibility PossibilityDefinitely . typeOfValType) m }

prop_progress_no_values :: Map.Map (CB.Name T.Variable) CT.ValType -> Query () T.Variable -> Property
prop_progress_no_values f q
 | Right bland <- runDesugar (freshnamer "desugar") (desugarQ q)
 , typ         <- freshcheck "check" $ checkQ optionSmallData (mkElems f) bland
 , pp          <- show $ pretty bland
 = counterexample pp
 $ counterexample (show typ)
 $ case typ of
    Right (q',_)
     | val <- evalQ q' [] Map.empty
     -> counterexample (show val)
      $ isRight val
    Left _
     -> discard
 | otherwise = discard


return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 10)
