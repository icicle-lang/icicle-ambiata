{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromCanyon.Analysis (
    factVarsOfProgram
  , resumablesOfProgram
  , outputsOfProgram
  , typesOfProgram
  ) where

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Canyon.Program
import           Icicle.Canyon.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Exp
import           Icicle.Common.Type

import           Icicle.Data.Name

import           Icicle.Sea.Data

import           P

import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.Set (Set)
import qualified Data.Set as Set


------------------------------------------------------------------------
-- Analysis

factVarsOfProgram :: Eq n
                  => Program (Annot a) n
                  -> Maybe (ValType, [(Name n, ValType)])

factVarsOfProgram = factVarsOfStatement . statements


factVarsOfStatement :: Eq n
                    => Statement (Annot a) n
                    -> Maybe (ValType, [(Name n, ValType)])

factVarsOfStatement stmt
 = case stmt of
     Block []              -> Nothing
     Block (s:ss)          -> factVarsOfStatement s <|>
                              factVarsOfStatement (Block ss)
     Let _ _ _ ss          -> factVarsOfStatement ss
     If _ tt ee            -> factVarsOfStatement tt <|>
                              factVarsOfStatement ee
     Write _ _             -> Nothing
     LoadResumable _ _     -> Nothing
     SaveResumable _ _     -> Nothing
     Output _ _ _          -> Nothing
     While     _ _ _ _ ss  -> factVarsOfStatement ss
     ForeachInts _ _ _ _ ss-> factVarsOfStatement ss

     ForeachFacts binds vt _
      -> Just (vt, factBindValue binds)

------------------------------------------------------------------------

resumablesOfProgram :: Eq n => Program (Annot a) n -> Map (Name n) ValType
resumablesOfProgram = resumablesOfStatement . statements

resumablesOfStatement :: Eq n => Statement (Annot a) n -> Map (Name n) ValType
resumablesOfStatement stmt
 = case stmt of
     Block []                -> Map.empty
     Block (s:ss)            -> resumablesOfStatement s `Map.union`
                                resumablesOfStatement (Block ss)
     Let _ _ _ ss            -> resumablesOfStatement ss
     If _ tt ee              -> resumablesOfStatement tt `Map.union`
                                resumablesOfStatement ee
     ForeachInts  _ _ _ _ ss -> resumablesOfStatement ss
     While        _ _ _ _ ss -> resumablesOfStatement ss
     ForeachFacts _ _ ss     -> resumablesOfStatement ss

     Write _ _             -> Map.empty
     Output _ _ _          -> Map.empty

     LoadResumable n t     -> Map.singleton n t
     SaveResumable n t     -> Map.singleton n t

------------------------------------------------------------------------

outputsOfProgram :: Program (Annot a) n -> [(OutputId, MeltedType)]
outputsOfProgram = Map.toList . outputsOfStatement . statements

outputsOfStatement :: Statement (Annot a) n -> Map OutputId MeltedType
outputsOfStatement stmt
 = case stmt of
     Block []                -> Map.empty
     Block (s:ss)            -> outputsOfStatement s `Map.union`
                                outputsOfStatement (Block ss)
     Let _ _ _ ss            -> outputsOfStatement ss
     If _ tt ee              -> outputsOfStatement tt `Map.union`
                                outputsOfStatement ee
     While        _ _ _ _ ss -> outputsOfStatement ss
     ForeachInts  _ _ _ _ ss -> outputsOfStatement ss
     ForeachFacts _ _ ss     -> outputsOfStatement ss
     Write _ _               -> Map.empty
     LoadResumable _ _       -> Map.empty
     SaveResumable _ _       -> Map.empty

     Output n t xts
      -> Map.singleton n $ MeltedType t (fmap snd xts)

------------------------------------------------------------------------

typesOfProgram :: Program (Annot a) n -> Set ValType
typesOfProgram = typesOfStatement . statements

typesOfStatement :: Statement (Annot a) n -> Set ValType
typesOfStatement stmt
 = case stmt of
     Block []              -> Set.empty
     Block (s:ss)          -> typesOfStatement s  `Set.union`
                              typesOfStatement (Block ss)
     Let _ xt x ss         -> Set.singleton    xt `Set.union`
                              typesOfExp       x  `Set.union`
                              typesOfStatement ss
     If x tt ee            -> typesOfExp       x  `Set.union`
                              typesOfStatement tt `Set.union`
                              typesOfStatement ee
     While     _ _ nt t s  -> Set.singleton   nt `Set.union`
                              typesOfExp       t `Set.union`
                              typesOfStatement s
     ForeachInts _  _ f t s-> typesOfExp       f `Set.union`
                              typesOfExp       t `Set.union`
                              typesOfStatement s
     Write _ x             -> typesOfExp x

     ForeachFacts binds _ ss
      -> Set.fromList (fmap snd $ factBindsAll binds) `Set.union`
         typesOfStatement ss

     LoadResumable _ vt
      -> Set.singleton vt

     SaveResumable _ vt
      -> Set.singleton vt

     -- vt is the unmelted type, which is not actually part of the program
     Output _ _vt xs
      -> Set.unions (fmap goOut xs)

 where
  goOut (x,t)
   = typesOfExp x `Set.union` Set.singleton t

typesOfExp :: Exp (Annot a) n Prim -> Set ValType
typesOfExp xx
 = case xx of
    XVar a _
     -> ann a
    XPrim a _
     -> ann a
    XValue _ vt _
     -> Set.singleton vt
    XApp a b c
     -> ann a `Set.union` typesOfExp b `Set.union` typesOfExp c
    XLam a _ vt b
     -> ann a `Set.union` Set.singleton vt `Set.union` typesOfExp b
    XLet a _ b c
     -> ann a `Set.union` typesOfExp b `Set.union` typesOfExp c
 where
  ann a = Set.singleton $ functionReturns $ annType a
