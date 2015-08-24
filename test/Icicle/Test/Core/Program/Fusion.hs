{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Program.Fusion where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Program.Check
import           Icicle.Core.Program.Fusion
import qualified Icicle.Core.Eval.Program   as PV

import           Icicle.Data.DateTime

import           P

import           System.IO

import           Test.QuickCheck


-- Just choose some date; it doesn't matter
someDate = dateOfYMD 2015 1 1
eval = PV.eval someDate []

left = Var "left" 0
right = Var "right" 0

-- We can always fuse a well typed program with itself
-- =====================
prop_fuseself t =
 forAll (programForStreamType t)
 $ \p ->
 isRight (checkProgram p)
 ==> isRight (fusePrograms left p right p)

-- And if we fuse the program with itself, we get a pair of the two values back
prop_fuseself_eval t =
 forAll (programForStreamType t)
 $ \p ->
   -- Fuse typechecks its args
   case (fusePrograms left p right p) of
    Right p'
     | Right v  <- eval p
     -> case eval p' of
        -- An evaluation error - this is bad
        Left _
         -> property False
        -- Evaluation succeeded so the values must match
        Right vv
         -> property (PV.value vv === (PV.value v) <> (PV.value v))
    _ -> property Discard


-- We can also fuse any two well typed programs
prop_fuse2 t =
 forAll (programForStreamType t)
 $ \p1 ->
 forAll (programForStreamType t)
 $ \p2 ->
 -- If both type check, fuse must return a new program
 isRight (checkProgram p1) && isRight (checkProgram p2)
 ==> isRight (fusePrograms left p1 right p2)


-- Evaluate two programs with empty input
prop_fuseeval2 t =
 forAll (programForStreamType t)
 $ \p1 ->
 forAll (programForStreamType t)
 $ \p2 ->
 -- Evaluate both input programs and try to fuse together
 case (eval p1, eval p2, fusePrograms left p1 right p2) of
  (Right v1, Right v2, Right p')
      -- Evaluate the fused program
   -> case eval p' of
       -- It should not be an error
       Left  _  -> property False
       -- It evaluated fine, so the values should match
       Right v' -> property (PV.value v' === (PV.value v1) <> (PV.value v2))

  -- The input programs must be bad, so throw it away
  _ -> property Discard


-- Evaluate programs with same input
prop_fuseeval2_values t =
 forAll (programForStreamType t)
 $ \p1 ->
 forAll (programForStreamType t)
 $ \p2 ->
 forAll (inputsForType t)
 $ \(vs,d) ->
 -- Evaluate both input programs and try to fuse together
 case (PV.eval d vs p1, PV.eval d vs p2, fusePrograms left p1 right p2) of
  (Right v1, Right v2, Right p')
      -- Evaluate the fused program
   -> case PV.eval d vs p' of
       -- It should not be an error
       Left  _  -> property False
       -- It evaluated fine, so the values should match
       Right v' -> property (PV.value v' === (PV.value v1) <> (PV.value v2))

  -- The input programs must be bad, so throw it away
  _ -> property Discard




return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 5, maxDiscardRatio = 10000})
