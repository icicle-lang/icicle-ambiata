{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.Melt where

import           Icicle.Test.Arbitrary

import qualified Icicle.Avalanche.Annot               as AA
import qualified Icicle.Avalanche.Program             as AP
import           Icicle.Avalanche.Statement.Simp.Melt
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Exp                    (annotOfExp)
import           Icicle.Common.Type

import           Icicle.Core.Program.Check

import           Icicle.Internal.Pretty

import qualified Icicle.Pipeline                      as P

import           P

import qualified Data.Map as Map

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property


prop_roundtrip_meltValue t =
  forAll (baseValueForType t) $ \v0 ->
    checkMelt   v0 t $ \vs ->
    checkUnmelt vs t $ \v1 -> v1 === v0

checkMelt :: BaseValue -> ValType -> ([BaseValue] -> Property) -> Property
checkMelt v t go =
  case meltValue v t of
    Nothing -> counterexample ("failed to melt:")
             $ counterexample ("  type  = " <> show t)
             $ counterexample ("  value = " <> show v)
             $ failed
    Just vs -> go vs

checkUnmelt :: [BaseValue] -> ValType -> (BaseValue -> Property) -> Property
checkUnmelt vs t go =
  case unmeltValue vs t of
    Nothing -> counterexample ("failed to unmelt:")
             $ counterexample ("  type   = " <> show t)
             $ counterexample ("  values = " <> show vs)
             $ failed
    Just v' -> go v'

prop_melt_total t
 = forAll (programForStreamType t)
 $   \coreProgram -> P.isRight (checkProgram coreProgram)
 ==> case P.coreFlatten coreProgram of
      Left _
       -> discard
      Right flatProgram
       -> let checked = P.checkAvalanche $ AA.eraseAnnotP flatProgram
          in case checked of
              Left _
               -> counterexample (show $ pretty flatProgram) False
              Right p
               -> let u = unmelted (AP.statements p)
                  in   counterexample (show $ pretty $ P.coreAvalanche coreProgram)
                     $ counterexample (show $ pretty flatProgram)
                     $ counterexample ("unmelted:\n" <> show u)
                     $ isNothing u
 where
  unmelted stm
   = case stm of
      If x s1 s2
       | notAllowed (getType x) -> Just stm
       | otherwise              -> unmelted s1 <|> unmelted s2
      Let _ x s
       | notAllowed (getType x) -> Just stm
       | otherwise              -> unmelted s
      ForeachInts _ _ x1 x2 s
       | notAllowed (getType x1) -> Just stm
       | notAllowed (getType x2) -> Just stm
       | otherwise               -> unmelted s
      ForeachFacts (FactBinds _ _ xs) _ _ ss
       | or $ fmap (notAllowed . snd) xs  -> Just stm
       | otherwise                        -> unmelted ss
      Block ss                            -> foldr (<|>) Nothing $ fmap unmelted ss
      InitAccumulator (Accumulator _ _ x) s
       | notAllowed (getType x)  -> Just stm
       | otherwise               -> unmelted s
      Read _ _ _ s               -> unmelted s
      Write _ x
       | notAllowed (getType x)  -> Just stm
      _                          -> Nothing

  notAllowed tt
   = case tt of
      PairT _ _  -> True
      SumT  _ _  -> True
      OptionT _  -> True
      ArrayT  ty -> notAllowed ty
      BufT  _ ty -> notAllowed ty
      StructT st -> or $ fmap notAllowed $ Map.elems $ getStructType st
      MapT   k v -> notAllowed k || notAllowed v
      _          -> False

  getType x
   = functionReturns (annType (annotOfExp x))


return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 10)
