{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Source (
    getCheckOptions
  , getEvalContext
  ) where

import           Control.Monad.State (get, gets)

import qualified Data.Set as Set

import           Icicle.Common.Eval
import           Icicle.Data.Time
import           Icicle.Repl.Data
import           Icicle.Repl.Monad
import qualified Icicle.Source.Checker as Source

import           P


getCheckOptions :: Repl Source.CheckOptions
getCheckOptions = do
  flags <- gets stateFlags
  if Set.member FlagBigData flags then
    pure Source.optionBigData
  else
    pure Source.optionSmallData

getEvalContext :: Repl EvalContext
getEvalContext = do
  s <- get
  pure $
    EvalContext
      (exclusiveSnapshotTime $ stateSnapshotDate s)
      (stateMaxMapSize s)
