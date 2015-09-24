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
 -- | IsNum String
 | ConflictingLetTemporalities (Type n) (Type n) (Type n)
 deriving (Eq, Ord, Show)

instance Pretty n => Pretty (DischargeError n) where
 pretty (CannotUnify p q)
  =  "Cannot unify: " <> indent 0 (pretty p) <> line
  <> "With type:    " <> indent 0 (pretty q) <> line
  <> "These types were required to be equal, but are not."
 pretty (NotANumber t)
  =  "Not a number: " <> pretty t <> line
  <> "Chances are you tried to apply some numerical computation like (+) or sum to the wrong field."
 pretty (ConflictingLetTemporalities _ def body)
  =  "Conflicting let temporalities." <> line
  <> "This kind of let isn't allowed because its definition could never be used." <> line
  <> "The definition is a " <> pretty def
  <> ", while the body is a " <> pretty body <> "."

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

    -- Still variables, so can't discharge
    CReturnOfLetTemporalities _ (TypeVar _) _
     -> return $ DischargeLeftover c
    CReturnOfLetTemporalities _ _ (TypeVar _)
     -> return $ DischargeLeftover c
    CReturnOfLetTemporalities ret def body
     -> case returnOfLet def body of
         Just tmp
          -> dischargeC (CEquals ret tmp)
         Nothing
          -> Left $ ConflictingLetTemporalities ret def body

    CReturnOfLatest ret tmp dat
     -> case returnOfLatest tmp dat of
         Just ret'
          -> dischargeC (CEquals ret ret')
         _
          -> return $ DischargeLeftover c

    CPossibilityJoin ret PossibilityPossibly _
     -> dischargeC (CEquals ret PossibilityPossibly)
    CPossibilityJoin ret _ PossibilityPossibly
     -> dischargeC (CEquals ret PossibilityPossibly)
    CPossibilityJoin ret PossibilityDefinitely z
     -> dischargeC (CEquals ret z)
    CPossibilityJoin ret y PossibilityDefinitely
     -> dischargeC (CEquals ret y)
    CPossibilityJoin ret y z
     | y == z
     -> dischargeC (CEquals ret y)
    CPossibilityJoin _ _ _
     -> return $ DischargeLeftover c

 where
  returnOfLet def body
   = case (def,body) of
      (TemporalityPure, TemporalityPure)
       -> Just TemporalityPure
      (TemporalityPure, TemporalityElement)
       -> Just TemporalityElement
      (TemporalityPure, TemporalityAggregate)
       -> Just TemporalityAggregate
      (TemporalityElement, TemporalityPure)
       -> Just TemporalityElement
      (TemporalityElement, TemporalityElement)
       -> Just TemporalityElement
      (TemporalityElement, TemporalityAggregate)
       -> Just TemporalityAggregate
      (TemporalityAggregate, TemporalityPure)
       -> Just TemporalityAggregate

      -- This implies that the let is not actually used.
      -- I'm not sure whether this should be allowed or not.
      -- At the moment this cannot be compiled to core, so it is outlawed.
      (TemporalityAggregate, TemporalityElement)
       -> Nothing

      (TemporalityAggregate, TemporalityAggregate)
       -> Just TemporalityAggregate

      (_, _)
       -> Nothing

  returnOfLatest TemporalityPure d
   = Just $ ArrayT d
  returnOfLatest TemporalityElement d
   = Just $ ArrayT d
  returnOfLatest TemporalityAggregate d
   = Just d
  -- Probably a variable
  returnOfLatest _ _
   = Nothing

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
    :: Ord n
    => [(a, Constraint n)]
    -> [(a, Constraint n)]
nubConstraints cs
 = concatMap removeEmpties
 $ nubBy ((==) `on` snd) cs
 where
  removeEmpties (a,c)
   | Right (DischargeSubst sub) <- dischargeC c
   , Map.null sub
   = []
   | otherwise
   = [(a,c)]
