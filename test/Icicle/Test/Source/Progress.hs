{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Progress where

import           Icicle.Internal.Pretty
import           Icicle.Source.Checker.Base
import           Icicle.Source.Checker.Checker
import           Icicle.Source.Query
import           Icicle.Source.Eval
import           Icicle.Source.Type
import qualified Icicle.Source.Lexer.Token as T
import qualified Icicle.Common.Base          as CB
import qualified Icicle.Common.Type          as CT

import           Icicle.Test.Source.Arbitrary
import           Icicle.Test.Core.Arbitrary ()

import           P

import           System.IO

import           Test.QuickCheck

import qualified Data.Map as Map


mkElems :: Map.Map (CB.Name T.Variable) CT.ValType -> CheckEnv () T.Variable
mkElems m = emptyCheckEnv { checkEnvironment = Map.map (function0 . Temporality TemporalityElement . Possibility PossibilityDefinitely . typeOfValType) m }

prop_progress_no_values :: Map.Map (CB.Name T.Variable) CT.ValType -> Query () T.Variable -> Property
prop_progress_no_values f q
 = counterexample pp
 $ counterexample (show typ)
 $ case typ of
    Right (q',_)
     | val <- evalQ q' [] Map.empty
     -> counterexample (show val)
      $ isRight val
    Left _
     -> discard
 where
  typ = freshcheck $ checkQ (mkElems f) q
  pp = show $ pretty q



return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs { {- maxSuccess = 5000, -} maxSize = 10,  maxDiscardRatio = 10000})
