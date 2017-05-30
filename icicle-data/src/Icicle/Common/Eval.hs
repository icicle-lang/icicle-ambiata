{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Eval (
      EvalContext(..)
    ) where

import              Icicle.Data.Time

import              P

data EvalContext
 = EvalContext
 { evalSnapshotTime :: Time
 , evalMaxMapSize   :: Int
 }

