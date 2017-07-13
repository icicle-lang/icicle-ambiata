{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Flag (
    allFlags
  , flagInfo
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
      "Colored text rendering is now on."
      "Colored text rendering is now off."
      "Render text using ANSI colors."

  , FlagInfo FlagType "type"
      "The type of a query will be shown during evaluation."
      "The type of a query will be hidden during evaluation."
      "Show the checked expression type."

  , FlagInfo FlagTypeCheckLog "type-check-log"
      "The type checking log will be displayed during evaluation."
      "The type checking log will be hidden during evaluation."
      "" -- FIXME

  , FlagInfo FlagBigData "big-data"
      "Queries are now subject to \"big data\" restrictions."
      "Queries are no longer subject to \"big data\" restrictions."
      "Perform big data check, only windows & latests allowed."

  , FlagInfo FlagAnnotated "annotated"
      "Type annotationed source will be shown during evaluation."
      "Type annotationed source will be hidden during evaluation."
      "Show the query with inferred types as annotations."

  , FlagInfo FlagInlined "inlined"
      "The source after inlining will be shown during evaluation."
      "The source after inlining will be hidden during evaluation."
      "Show the query after inlining functions."

  , FlagInfo FlagDesugar "desugar"
      "The source after desugaring case expressions will be shown during evaluation."
      "The source after desugaring case expressions will be hidden during evaluation."
      "Show the query after desugaring case expressions."

  , FlagInfo FlagReified "reified"
      "The source after reifying possibilities will be shown during evaluation."
      "The source after reifying possibilities will be hidden during evaluation."
      "Show the query after reifying possibilities."

  , FlagInfo FlagCore "core"
      "The core will be shown during evaluation."
      "The core will be hidden during evaluation."
      "Show the core conversion."

  , FlagInfo FlagCoreSimp "core-simp"
      "The core will be simplified prior to evaluation."
      "The core will not be simplified prior to evaluation."
      "Simplify the result of core conversion."

  , FlagInfo FlagCoreType "core-type"
      "The core type will be shown during evaluation."
      "The core type will be hidden during evaluation."
      "Show the core conversion's type."

  , FlagInfo FlagCoreEval "core-eval"
      "Queries will be evaluated using the core evaluator."
      "Queries will no longer be evaluated using the core evaluator."
      "Show the result, using core evaluation."

  , FlagInfo FlagAvalanche "avalanche"
      "The avalanche will be shown during evaluation."
      "The avalanche will be hidden during evaluation."
      "Show the avalanche conversion."

  , FlagInfo FlagAvalancheEval "avalanche-eval"
      "Queries will be evaluated using the avalanche evaluator."
      "Queries will no longer be evaluated using the avalanche evaluator."
      "Show the result, using avalanche evaluation."

  , FlagInfo FlagFlattenSimp "flatten-simp"
      "The flattened avalanche will be shown during evaluation."
      "The flattened avalanche will be hidden during evaluation."
      "Show the flattened avalanche conversion."

  , FlagInfo FlagFlattenNoSimp "flatten-no-simp"
      "flatten-no-simp is now on."
      "flatten-no-simp is now off."
      "" -- FIXME more details

  , FlagInfo FlagFlattenSimpCheck "flatten-simp-check"
      "flatten-simp-check is now on."
      "flatten-simp-check is now off."
      "" -- FIXME more details

  , FlagInfo FlagSeaPreamble "c-preamble"
      "The C preamble will be shown during evaluation."
      "The C preamble will be hidden during evaluation."
      "Show the C preamble."

  , FlagInfo FlagSea "c"
      "The C will be shown during evaluation."
      "The C will be hidden during evaluation."
      "Show the C conversion."

  , FlagInfo FlagSeaLLVM "c-llvm"
      "The LLVM IR will be shown during evaluation."
      "The LLVM IR will be hidden during evaluation."
      "Show the C LLVM IR."

  , FlagInfo FlagSeaAssembly "c-assembly"
      "The x86-64 assembly will be shown during evaluation."
      "The x86-64 assembly will be hidden during evaluation."
      "Show the C assembly."

  , FlagInfo FlagSeaRuntime "c-runtime"
      "Runtime details will be shown during evaluation."
      "Runtime details will be hidden during evaluation."
      "Show the information necessary for icicle to interact with the compiled C query."

  , FlagInfo FlagSeaEval "c-eval"
      "Queries will be evaluated using the C evaluator."
      "Queries will no longer be evaluated using the C evaluator."
      "Show the result, using C evaluation."
  ]

namedFlags :: Map String Flag
namedFlags =
  Map.fromList $ fmap (\x -> (flagName x, flagFlag x)) allFlags

flagInfo :: Map Flag FlagInfo
flagInfo =
  Map.fromList $ fmap (\x -> (flagFlag x, x)) allFlags

whenSet :: MonadState State m => Flag -> m () -> m ()
whenSet flag io = do
  flags <- gets stateFlags
  when (Set.member flag flags) io

ifSet :: MonadState State m => Flag -> m a -> m a -> m a
ifSet flag t f = do
  flags <- gets stateFlags
  if Set.member flag flags then t else f
