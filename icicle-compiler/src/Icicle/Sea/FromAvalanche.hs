{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromAvalanche (
    seaOfPrograms
  , factVarsOfProgram
  , accumsOfProgram
  , outputsOfProgram
  ) where

import           Icicle.Sea.FromAvalanche.Program
import           Icicle.Sea.FromAvalanche.Analysis
