{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Analysis (
    factVarsOfProgram
  , resumablesOfProgram
  , accumsOfProgram
  , outputsOfProgram
  , readsOfProgram
  ) where

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Type

import           P

import           Data.Map (Map)
import qualified Data.Map as Map


------------------------------------------------------------------------
-- Analysis

factVarsOfProgram :: Ord n
                  => FactLoopType
                  -> Program (Annot a) n Prim
                  -> Maybe (ValType, [(Name n, ValType)])

factVarsOfProgram loopType = factVarsOfStatement loopType . statements


factVarsOfStatement :: Ord n
                    => FactLoopType
                    -> Statement (Annot a) n Prim
                    -> Maybe (ValType, [(Name n, ValType)])

factVarsOfStatement loopType stmt
 = case stmt of
     Block []              -> Nothing
     Block (s:ss)          -> factVarsOfStatement loopType s <|>
                              factVarsOfStatement loopType (Block ss)
     Let _ _ ss            -> factVarsOfStatement loopType ss
     If _ tt ee            -> factVarsOfStatement loopType tt <|>
                              factVarsOfStatement loopType ee
     ForeachInts  _ _ _ ss -> factVarsOfStatement loopType ss
     InitAccumulator _ ss  -> factVarsOfStatement loopType ss
     Read _ _ _ ss         -> factVarsOfStatement loopType ss
     Write _ _             -> Nothing
     Push  _ _             -> Nothing
     LoadResumable _ _     -> Nothing
     SaveResumable _ _     -> Nothing
     Output _ _ _          -> Nothing
     KeepFactInHistory     -> Nothing

     ForeachFacts ns vt lt ss
      | lt == loopType
      -> Just (vt, ns)

      | otherwise
      -> factVarsOfStatement loopType ss

------------------------------------------------------------------------

resumablesOfProgram :: Ord n => Program (Annot a) n Prim -> Map (Name n) ValType
resumablesOfProgram = resumablesOfStatement . statements

resumablesOfStatement :: Ord n => Statement (Annot a) n Prim -> Map (Name n) ValType
resumablesOfStatement stmt
 = case stmt of
     Block []              -> Map.empty
     Block (s:ss)          -> resumablesOfStatement s `Map.union`
                              resumablesOfStatement (Block ss)
     Let _ _ ss            -> resumablesOfStatement ss
     If _ tt ee            -> resumablesOfStatement tt `Map.union`
                              resumablesOfStatement ee
     ForeachInts  _ _ _ ss -> resumablesOfStatement ss
     ForeachFacts _ _ _ ss -> resumablesOfStatement ss
     InitAccumulator  _ ss -> resumablesOfStatement ss
     Read _ _ _ ss         -> resumablesOfStatement ss
     Write _ _             -> Map.empty
     Push  _ _             -> Map.empty
     Output _ _ _          -> Map.empty
     KeepFactInHistory     -> Map.empty

     LoadResumable n t     -> Map.singleton n t
     SaveResumable n t     -> Map.singleton n t

------------------------------------------------------------------------

accumsOfProgram :: Ord n => Program (Annot a) n Prim -> Map (Name n) (ValType)
accumsOfProgram = accumsOfStatement . statements

accumsOfStatement :: Ord n => Statement (Annot a) n Prim -> Map (Name n) (ValType)
accumsOfStatement stmt
 = case stmt of
     Block []              -> Map.empty
     Block (s:ss)          -> accumsOfStatement s `Map.union`
                              accumsOfStatement (Block ss)
     Let _ _ ss            -> accumsOfStatement ss
     If _ tt ee            -> accumsOfStatement tt `Map.union`
                              accumsOfStatement ee
     ForeachInts  _ _ _ ss -> accumsOfStatement ss
     ForeachFacts _ _ _ ss -> accumsOfStatement ss
     Read _ _ _ ss         -> accumsOfStatement ss
     Write _ _             -> Map.empty
     Push  _ _             -> Map.empty
     LoadResumable _ _     -> Map.empty
     SaveResumable _ _     -> Map.empty
     Output _ _ _          -> Map.empty
     KeepFactInHistory     -> Map.empty

     InitAccumulator (Accumulator n avt _) ss
      -> Map.singleton n avt `Map.union`
         accumsOfStatement ss

------------------------------------------------------------------------

readsOfProgram :: Ord n => Program (Annot a) n Prim -> Map (Name n) (ValType)
readsOfProgram = readsOfStatement . statements

readsOfStatement :: Ord n => Statement (Annot a) n Prim -> Map (Name n) (ValType)
readsOfStatement stmt
 = case stmt of
     Block []              -> Map.empty
     Block (s:ss)          -> readsOfStatement s `Map.union`
                              readsOfStatement (Block ss)
     Let _ _ ss            -> readsOfStatement ss
     If _ tt ee            -> readsOfStatement tt `Map.union`
                              readsOfStatement ee
     ForeachInts  _ _ _ ss -> readsOfStatement ss
     ForeachFacts _ _ _ ss -> readsOfStatement ss
     InitAccumulator _ ss  -> readsOfStatement ss
     Write _ _             -> Map.empty
     Push  _ _             -> Map.empty
     LoadResumable _ _     -> Map.empty
     SaveResumable _ _     -> Map.empty
     Output _ _ _          -> Map.empty
     KeepFactInHistory     -> Map.empty

     Read n _ vt ss
      -> Map.singleton n vt `Map.union`
         readsOfStatement ss

------------------------------------------------------------------------

outputsOfProgram :: Program (Annot a) n Prim -> [(OutputName, (ValType, [ValType]))]
outputsOfProgram = Map.toList . outputsOfStatement . statements

outputsOfStatement :: Statement (Annot a) n Prim -> Map OutputName (ValType, [ValType])
outputsOfStatement stmt
 = case stmt of
     Block []              -> Map.empty
     Block (s:ss)          -> outputsOfStatement s `Map.union`
                              outputsOfStatement (Block ss)
     Let _ _ ss            -> outputsOfStatement ss
     If _ tt ee            -> outputsOfStatement tt `Map.union`
                              outputsOfStatement ee
     ForeachInts  _ _ _ ss -> outputsOfStatement ss
     ForeachFacts _ _ _ ss -> outputsOfStatement ss
     InitAccumulator _ ss  -> outputsOfStatement ss
     Read _ _ _ ss         -> outputsOfStatement ss
     Write _ _             -> Map.empty
     Push  _ _             -> Map.empty
     LoadResumable _ _     -> Map.empty
     SaveResumable _ _     -> Map.empty
     KeepFactInHistory     -> Map.empty

     Output n t xts
      -> Map.singleton n (t, fmap snd xts)
