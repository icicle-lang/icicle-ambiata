{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Statement.Simp.ExpEnv (
    ExpEnv
  , emptyExpEnv
  , updateExpEnv
  , getFromEnv
  ) where

import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Base
import              Icicle.Common.Exp

import              P

import qualified    Data.Map            as Map


type ExpEnv a n p = Map.Map (Name n) (Exp a n p)

emptyExpEnv :: ExpEnv a n p
emptyExpEnv = Map.empty

updateExpEnv :: Ord n
             => Statement a n p
             -> ExpEnv a n p
             -> ExpEnv a n p
updateExpEnv s env
   = case s of
      Let n x _
      -- Normal let: remember the name and expression for later
       -> Map.insert n x env

      -- Anything else, nothing changes
      _
       -> env

-- Lookup a name in the environment.
getFromEnv :: Ord n => ExpEnv a n p -> Name n -> Maybe (Exp a n p)
getFromEnv env n
 = Map.lookup n env

