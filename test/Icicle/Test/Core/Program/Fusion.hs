{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Program.Fusion where

import           Icicle.Test.Core.Arbitrary
-- import           Icicle.Core.Program.Program
import           Icicle.Core.Program.Check
import           Icicle.Core.Program.Fusion
import qualified Icicle.Core.Eval.Exp       as XV
import qualified Icicle.Core.Eval.Program   as PV

import           Icicle.Data.DateTime

import           P

import           System.IO

import           Test.QuickCheck


-- Just choose some date; it doesn't matter
someDate = dateOfYMD 2015 1 1

-- We can always fuse a well typed program with itself
-- =====================
left = Var "left" 0
right = Var "right" 0

prop_fuseself :: Program Var -> Property
prop_fuseself x =
 isRight (checkProgram x)
 ==> isRight (fusePrograms left x right x)

prop_fuseself_eval :: Program Var -> Property
prop_fuseself_eval x
 | Right v  <- PV.eval someDate [] x
 , Right x' <- fusePrograms left x right x
 = case PV.eval someDate [] x' of
    Left _
     -> property False
    Right vv
     -> property (PV.value vv == XV.VPair (PV.value v) (PV.value v))
 | otherwise
 = False ==> False -- Ignore

{-
zprop_fuseeval :: Program Var -> Program Var -> Property
zprop_fuseeval x y =
 case (PV.eval someDate [] x, PV.eval someDate [] y, fusePrograms left x right y) of
 (Right xv, Right yv, Right p')
  -> case PV.eval someDate [] p' of
      Right r -> True ==> PV.value r == XV.VPair (PV.value xv) (PV.value yv)
      _       -> True  ==> False
 _
  -> False ==> False
-}




return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 5, maxDiscardRatio = 10000})
