{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.Statement.Simp.ExpEnv (
    ExpEnv
  , emptyExpEnv
  , updateExpEnv
  , clearFromExpEnv
  , getFromEnv
  ) where

import              Icicle.Avalanche.Statement.Statement

import              Icicle.Common.Base
import              Icicle.Common.Exp

import              P

import qualified    Data.Set            as Set
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
       -> Map.insert n x
        $ clearFromExpEnv n env

      -- New variables are bound, so clear the environment
      ForeachInts n _ _ _
       -> clearFromExpEnv n env
      ForeachFacts ns _ _ _
       -> foldr (clearFromExpEnv . fst) env ns

      Read n _ _ _
      -- we need to clear any mentions of "n" from the environment
       -> clearFromExpEnv n env

      -- Anything else, nothing changes
      _
       -> env


-- | The environment stores previously bound expressions.
-- These expressions can refer to names that are bound upwards.
-- This would be fine if there were no shadowing, but with shadowing we may end up
-- rebinding a name that is mentioned in another expression:
--
-- let a = 10
-- let b = a + 1
-- let a = 8
-- let s = a + 1
-- in  s
--
-- Here, s and b are locally alpha equivalent, but not really equivalent because they
-- refer to different "a"s.
-- When we see the second "a" binding, then, we must remove "b" from the environment of
-- previously bound expressions.
--
clearFromExpEnv :: Ord n => Name n -> ExpEnv a n p -> ExpEnv a n p
clearFromExpEnv n env
 = Map.filterWithKey (\n' x' -> n' /= n && not (Set.member n $ freevars x')) env


-- Lookup a name in the environment.
getFromEnv :: Ord n => ExpEnv a n p -> Name n -> Maybe (Exp a n p)
getFromEnv env n
 = Map.lookup n env

