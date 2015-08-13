-- | Doing things with Constraints
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Constraints (
    DischargeResult (..)
  , DischargeError (..)
  , dischargeC
  , dischargeCS
  , nubConstraints
  ) where


import                  Icicle.Source.Type.Base
import                  Icicle.Source.Type.Subst

import                  Icicle.Internal.Pretty

import                  P

import                  Data.List (nubBy)
import qualified        Data.Map as Map


-- | Result of discharging a single constraint
data DischargeResult n
 -- | Don't yet know whether the constraint is satisfied
 -- This is really only for things like "IsNum a"
 = DischargeLeftover (Constraint n)
 -- | Constraint requires a substitution
 | DischargeSubst (SubstT n)
 deriving (Eq, Ord, Show)

-- | Errors that arise from constraints
data DischargeError n
 -- | Two types that can't be unified.
 -- For example, "Int = Double"
 = CannotUnify (Type n) (Type n)
 -- | IsNum String
 | NotANumber  (Type n)
 deriving (Eq, Ord, Show)

instance Pretty n => Pretty (DischargeError n) where
 pretty (CannotUnify p q)
  = text "Cannot unify " <> pretty p <> " = " <> pretty q
 pretty (NotANumber t)
  = text "Not a number: " <> pretty t

-- | Discharge a single constraint
dischargeC :: Ord n => Constraint n -> Either (DischargeError n) (DischargeResult n)
dischargeC c
 = case c of
    CIsNum IntT
     -> return $ DischargeSubst Map.empty
    CIsNum DoubleT
     -> return $ DischargeSubst Map.empty
    CIsNum (TypeVar _)
     -> return $ DischargeLeftover c
    CIsNum t
     -> Left   $ NotANumber t

    CEquals a b
     -> case unifyT a b of
         Nothing -> Left $ CannotUnify a b
         Just s  -> return $ DischargeSubst s


-- | Attempt to discharge a set of constraints
-- Return a list of errors, or the substitution and any leftover constraints
dischargeCS :: Ord n => [(a, Constraint n)] -> Either [(a, DischargeError n)] (SubstT n, [(a, Constraint n)])
dischargeCS
 -- Accumulators: substitution, leftover constraints and errors
 = go Map.empty [] []
 where
  -- Reached the end with no errors
  go s cs [] []
   = return (s, nubConstraints cs)
  -- Reached the end, but errors is not empty
  go _ _ errs []
   = Left errs

  -- Try to discharge one constraint
  go s cs errs ((a,c):rest)
   = case dischargeC c of
      -- Error, so just add it to the list
      Left e
       -> go s cs ((a,e):errs) rest
      -- Leftover, so add to leftovers list
      Right (DischargeLeftover c')
       -> go s ((a,c'):cs) errs rest
      -- If the constraint results in a substitution,
      -- it might be able to discharge some earlier leftovers.
      -- Perform the substitution over the leftovers and the rest.
      --
      -- This does loop over the leftovers again,
      -- but is definitely terminating since "c" is removed.
      --
      Right (DischargeSubst s')
       | not $ Map.null s'
       -> go (compose s s') [] errs
        $ fmap (substOver s') (cs <> rest)
       -- However, as a special case, if the substitution is empty
       -- we needn't re-check the leftovers
       | otherwise
       -> go s cs errs rest

  substOver s (a,c)
   = (a, substC s c)


nubConstraints
    :: Eq n
    => [(a, Constraint n)]
    -> [(a, Constraint n)]
nubConstraints cs
 = nubBy ((==) `on` snd) cs
