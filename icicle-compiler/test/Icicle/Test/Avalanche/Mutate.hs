{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.Mutate where

import           Icicle.Common.Type
import qualified Icicle.Common.Exp                      as Common

import qualified Icicle.Core.Program.Check              as Core

import qualified Icicle.Avalanche.Eval                  as Avalanche
import qualified Icicle.Avalanche.Program               as Avalanche
import qualified Icicle.Avalanche.Prim.Eval             as Avalanche
import qualified Icicle.Avalanche.Prim.Flat             as Flat
import           Icicle.Avalanche.Statement.Statement
import qualified Icicle.Avalanche.Statement.Simp.Mutate as Mutate

import qualified Icicle.Compiler                        as Pipeline
import           Icicle.Internal.Pretty

import           Icicle.Test.Arbitrary

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Property



prop_mutate_safe =
  forAll genTestType
  $ \t ->
  forAll (programForStreamType t)
  $ \p ->
  forAll (inputsForType t)
  $ \(vs, d) ->
  isRight (Core.checkProgram p) ==>
   let aval = Pipeline.coreAvalanche p
       flatUnsimped = Pipeline.flattenAvalanche aval
   in case flatUnsimped of
        Left _
         -> discard -- not well-typed avalanche
        Right flatUnsimped'
         | hasArrayUpdate flatUnsimped'
         -> flatten_mutate_safe d vs flatUnsimped'
         | otherwise
         -> discard -- no array put :(

flatten_mutate_safe d vs flatUnsimped
  | flatMutated <- mutate flatUnsimped
  , flatMutated /= flatUnsimped -- only test when mutate actually does something
  = let evalUnsimped = eval Avalanche.evalPrim flatUnsimped
        evalMutated  = eval Avalanche.evalPrim flatMutated
     in   counterexample ("Flat unsimped:\n" <> show (pretty flatUnsimped))
        $ counterexample ("Flat mutated:\n"  <> show (pretty flatMutated))
        $ evalUnsimped `compareEvalResult` evalMutated
  | otherwise
  = discard
  where
   mutate x = x { Avalanche.statements = Mutate.mutate $ Avalanche.statements x }
   eval xp  = Avalanche.evalProgram xp d vs
   compareEvalResult xv yv =
     let xv' = second snd (first show xv)
         yv' = second snd (first show yv)
     in either (counterexample . show . pretty) (const id) xv $
        either (counterexample . show . pretty) (const id) yv $
        if xv' == yv'
        then property succeeded
        else counterexample (show xv') $
             counterexample " /="      $
             counterexample (show yv') $
             property failed

-- hopefully this gives us some array updates to test
genTestType = oneof
  [ ArrayT  <$> arbitrary
  , MapT    <$> arbitrary   <*> arbitrary
  , OptionT <$> genTestType
  , PairT   <$> genTestType <*> genTestType
  ]

hasArrayUpdate :: Avalanche.Program a n Flat.Prim -> Bool
hasArrayUpdate prog = goS (Avalanche.statements prog)
  where
    goS ss = case ss of
      If x s1 s2
        -> goX x || goS s1 || goS s2
      Let _ x s
        -> goX x || goS s
      While _ _ _ x s
        -> goX x || goS s
      ForeachInts _ _ x1 x2 s
        -> goX x1 || goX x2 || goS s
      ForeachFacts _ _ _ s
        -> goS s
      Block xs
        -> any goS xs
      InitAccumulator a s
        -> goA a || goS s
      Read _ _ _ s
        -> goS s
      Write _ x
        -> goX x
      Output _ _ xs
        -> any (goX . fst) xs
      _ -> False

    goX xx
      | Just (p, _) <- Common.takePrimApps xx
      , isArrayPut p
      = True
      | otherwise
      = False

    goA = goX . accInit

    isArrayPut p = case p of
      Flat.PrimArray (Flat.PrimArrayPutMutable _)
        -> True
      Flat.PrimArray (Flat.PrimArrayPutImmutable _)
        -> True
      _ -> False


return []
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
