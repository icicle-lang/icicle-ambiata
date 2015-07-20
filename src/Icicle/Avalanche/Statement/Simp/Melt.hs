{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp.Melt (
    melt
  ) where

import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Fresh


-- TODO
melt :: Statement n Prim -> Fresh n (Statement n Prim)
melt = return
