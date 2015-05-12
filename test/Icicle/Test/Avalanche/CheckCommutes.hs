{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.CheckCommutes where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Program.Check
import           Icicle.Core.Exp (coreFragment)

import qualified Icicle.Avalanche.FromCore  as Convert
import qualified Icicle.Avalanche.Check     as Check

import           Icicle.Internal.Pretty

import           P

import           System.IO

import           Test.QuickCheck

-- We need a way to differentiate stream variables from scalars
namer = Convert.namerText (flip Var 0)

-- A well typed core program is well typed under Avalanche
prop_check_commutes t =
 forAll (programForStreamType t)
 $ \p ->
    isRight     (checkProgram p) ==>
     let conv = Convert.programFromCore namer p in
     case Check.checkProgram coreFragment conv of
      Right _
       -> property True
      Left err
       -> counterexample (show err)
        $ counterexample (show $ pretty p)
        $ counterexample (show conv) False
     



return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})

