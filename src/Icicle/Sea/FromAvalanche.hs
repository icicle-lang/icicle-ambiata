{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche (
    seaOfProgram
  , stateWordsOfProgram
  , factVarsOfProgram
  , accumsOfProgram
  , outputsOfProgram
  ) where

import           Icicle.Sea.FromAvalanche.Program
import           Icicle.Sea.FromAvalanche.Analysis
