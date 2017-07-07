{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Flag (
    allFlags
  , flagNames
  , namedFlags
  , whenSet
  , ifSet
  ) where

import           Control.Monad.State.Class (MonadState, gets)

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (String)

import           Icicle.Repl.Data

import           P


allFlags :: [FlagInfo]
allFlags = [
    FlagInfo FlagColor "color"
      "Render text using ANSI colors."

  , FlagInfo FlagType "type"
      "Show the checked expression type."

  , FlagInfo FlagTypeCheckLog "type-check-log"
      "" -- FIXME

  , FlagInfo FlagBigData "big-data"
      "Perform big data check, only windows & latests allowed."

  , FlagInfo FlagAnnotated "annotated"
      "Show the source with inferred types as annotations."

  , FlagInfo FlagInlined "inlined"
      "Show the source after inlining functions."

  , FlagInfo FlagDesugar "desugar"
      "Show the source after desugaring case expressions."

  , FlagInfo FlagReified "reified"
      "Show the source after reifying possibilities."

  , FlagInfo FlagCore "core"
      "Show the core conversion."

  , FlagInfo FlagCoreSimp "core-simp"
      "Simplify the result of core conversion."

  , FlagInfo FlagCoreType "core-type"
      "Show the core conversion's type."

  , FlagInfo FlagCoreEval "core-eval"
      "Show the result, using core evaluation."

  , FlagInfo FlagAvalanche "avalanche"
      "Show the avalanche conversion."

  , FlagInfo FlagAvalancheEval "avalanche-eval"
      "Show the result, using avalanche evaluation."

  , FlagInfo FlagFlattenSimp "flatten-simp"
      "Show the flattened avalanche conversion."

  , FlagInfo FlagFlattenNoSimp "flatten-no-simp"
      "" -- FIXME

  , FlagInfo FlagFlattenSimpCheck "flatten-simp-check"
      "" -- FIXME

  , FlagInfo FlagSeaPreamble "c-preamble"
      "Show the C preamble."

  , FlagInfo FlagSea "c"
      "Show the C conversion."

  , FlagInfo FlagSeaAssembly "c-assembly"
      "Show the C assembly."

  , FlagInfo FlagSeaLLVM "c-llvm"
      "Show the C LLVM IR."

  , FlagInfo FlagSeaRuntime "c-runtime"
      "Show the information necessary for Haskell <-> C interaction."

  , FlagInfo FlagSeaEval "c-eval"
      "Show the result, using C evaluation."
  ]

namedFlags :: Map String Flag
namedFlags =
  Map.fromList $ fmap (\x -> (flagName x, flagFlag x)) allFlags

flagNames :: Map Flag String
flagNames =
  Map.fromList $ fmap (\x -> (flagFlag x, flagName x)) allFlags

whenSet :: MonadState State m => Flag -> m () -> m ()
whenSet flag io = do
  flags <- gets stateFlags
  when (Set.member flag flags) io

ifSet :: MonadState State m => Flag -> m a -> m a -> m a
ifSet flag t f = do
  flags <- gets stateFlags
  if Set.member flag flags then t else f
