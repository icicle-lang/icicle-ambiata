-- | Doing things with Constraints
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Constraints (
    dischargeC
  ) where


import                  Icicle.Source.Type.Base
import                  Icicle.Source.Type.Subst

import                  P

import qualified        Data.Map as Map

data Discharge n
 = Leftovers (Constraint n)
 | Result (SubstT n)

data Error n
 = CannotUnify (Type n) (Type n)
 | NotANumber (Type n)

dischargeC :: Ord n => Constraint n -> Either (Error n) (Discharge n)
dischargeC c
 = case c of
    CIsNum IntT
     -> return $ Result Map.empty
    CIsNum DoubleT
     -> return $ Result Map.empty
    CIsNum (TypeVar _)
     -> return $ Leftovers c
    CIsNum t
     -> Left $ NotANumber t

    CEquals a b
     -> case unifyT a b of
         Nothing -> Left $ CannotUnify a b
         Just s  -> return $ Result s

