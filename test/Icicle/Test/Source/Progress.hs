{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Progress where

import           Icicle.Internal.Pretty
import           Icicle.Source.Checker.Checker
import           Icicle.Source.Query
import           Icicle.Source.Eval
import           Icicle.Source.Type
import qualified Icicle.Source.Lexer.Token as T

import           Icicle.Test.Source.Arbitrary ()
import           Icicle.Test.Core.Arbitrary ()

import           P

import           System.IO

import           Test.QuickCheck

import qualified Data.Map as Map


mkElems :: Map.Map T.Variable BaseType -> CheckEnv T.Variable
mkElems m = emptyEnv { env = Map.map (function0 . UniverseType (Universe Elem Definitely)) m }

prop_progress_no_values :: Map.Map T.Variable BaseType -> Query () T.Variable -> Property
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
  typ = checkQ (mkElems f) q
  pp = show $ pretty q



return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs { {- maxSuccess = 5000, maxSize = 10, -} maxDiscardRatio = 10000})
