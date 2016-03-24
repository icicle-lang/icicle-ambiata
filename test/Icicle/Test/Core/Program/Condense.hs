{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Core.Program.Condense where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Test.Arbitrary
import           Icicle.Core.Program.Check
import           Icicle.Core.Program.Program
import           Icicle.Core.Program.Condense
import           Icicle.Core.Program.Fusion
import           Icicle.Core.Stream
import qualified Icicle.Core.Eval.Program   as PV
import           Icicle.Internal.Pretty


import           P

import           System.IO

import           Test.QuickCheck

left = Var "left" 0
right = Var "right" 0


-- Condensing might affect the contents of the type error, but not whether there's an error
-- =====================
prop_condense_type t =
 forAll (programForStreamType t)
 $ \p ->
   counterexample (show $ pretty p)
 $ counterexample (show $ pretty (condenseProgram () p))
 (isRight (checkProgram p) == isRight (checkProgram $ condenseProgram () p))

-- Condensing doesn't affect evaluation value.
-- It does affect the history, but not the value
-- =====================
prop_condense_eval t =
 forAll (programForStreamType t)
 $ \p1 ->
 forAll (inputsForType t)
 $ \(vs,d) ->
   counterexample (show $ pretty p1)
 $ counterexample (show $ pretty (condenseProgram () p1))
 $ case (PV.eval d vs p1, PV.eval d vs (condenseProgram () p1)) of
    (Left _, Left _)
     -- it is tempting to try "e1 === e2" here, but some of the errors have names inside them.
     -- they should be the same error modulo renaming..
     -> property True
    (Right v1, Right v2) -> PV.value v1 === PV.value v2
    (_,_)                -> property False

-- Condensing gives <= total stream size
-- =====================
prop_condense_no_more_streams t =
 forAll (programForStreamType t)
 $ \p1 ->
     let p1'c = condenseProgram () p1
     in counterexample (show $ pretty p1)
      $ counterexample (show $ pretty p1'c)
      ( stream_counts p1'c <= stream_counts p1)


-- Condensing after fusion gives original number of streams
-- =====================
prop_condense_fuseself_has_same_number_of_streams t =
 forAll (programForStreamType t)
 $ \p ->
  isRight (checkProgram p) ==>
  case fusePrograms () left p right p of
    Left err
     -> counterexample (show err) (property False)
    Right p'
     -> let p'c = condenseProgram () p'
            pc  = condenseProgram () p
            s'p'c = stream_counts p'c
            s'pc  = stream_counts pc
        in  counterexample ("Fused (" <> (show s'p'c) <> "): " <> (show $ pretty p'c))
         $  counterexample ("Original (" <> (show s'pc) <> "): " <> (show $ pretty pc))
          -- Condensed fused program has same number of streams as condensed original
         ( s'p'c === s'pc )

stream_counts
 = sum . fmap stream_count . streams

stream_count :: Stream a n -> Int
stream_count s
 = case s of
    SFold{} -> 1
    SFilter _ ss -> sum $ fmap stream_count ss




return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
