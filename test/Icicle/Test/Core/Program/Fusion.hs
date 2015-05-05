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

prop_fuseself t =
 forAll (programForStreamType t)
 $ \p ->
 isRight (checkProgram p)
 ==> isRight (fusePrograms left p right p)

prop_fuseself_eval t =
 forAll (programForStreamType t)
 $ \p ->
   case (fusePrograms left p right p) of
    Right p'
     | Right v  <- PV.eval someDate [] p
     -> case PV.eval someDate [] p' of
        Left _
         -> property False
        Right vv
         -> property (PV.value vv == XV.VPair (PV.value v) (PV.value v))
    _ -> False ==> False -- ignore


prop_fuse2 t =
 forAll (programForStreamType t)
 $ \p1 ->
 forAll (programForStreamType t)
 $ \p2 ->
 isRight (checkProgram p1) && isRight (checkProgram p2)
 ==> isRight (fusePrograms left p1 right p2)

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
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 5, maxDiscardRatio = 10000})
