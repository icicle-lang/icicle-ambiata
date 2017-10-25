{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.FromCanyon (
    seaOfPrograms
  , factVarsOfProgram
  , outputsOfProgram
  ) where

import           Icicle.Sea.FromCanyon.Program
import           Icicle.Sea.FromCanyon.Analysis
