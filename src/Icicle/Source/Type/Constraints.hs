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
import                  Icicle.Source.Type.Compounds

import                  Icicle.Internal.Pretty

import                  P hiding (join)

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
 | ConflictingJoinTemporalities (Type n) (Type n) (Type n)
 deriving (Eq, Ord, Show)

instance Pretty n => Pretty (DischargeError n) where
 pretty (CannotUnify p q)
  = "Cannot unify " <> pretty p <> " = " <> pretty q
 pretty (NotANumber t)
  = "Not a number: " <> pretty t
 pretty (ConflictingLetTemporalities ret def body)
  =  "Conflicting let temporalities." <> line
  <> "An Aggregate let statement with an Element body is not allowed: " <> pretty ret <+> pretty def <+> pretty body
 pretty (ConflictingJoinTemporalities a b c)
  =  "Conflicting join temporalities." <> line
  <> pretty a <+> pretty b <+> pretty c

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

    -- "Join" temporalities. Pure joins with anything. Aggregate only joins with itself or Pure.
    CTemporalityJoin x y z
     | ty       <- getTemporalityOrPure y
     , tz       <- getTemporalityOrPure z
     , Just tmp <- join ty tz
     -> case getTemporality x of
         Nothing
          -> return $ DischargeSubst Map.empty
         Just (tx, _)
          -> dischargeC (CEquals tmp tx)
          >> innerEquals y z
     | otherwise
     -> Left $ ConflictingJoinTemporalities x y z

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

 where
  innerEquals x y
   | Just x' <- getBaseType x
   , Just y' <- getBaseType y
   = dischargeC $ CEquals x' y'
   | otherwise
   = Left $ CannotUnify x y

  join TemporalityPure      TemporalityAggregate = Just TemporalityAggregate
  join TemporalityAggregate TemporalityPure      = Just TemporalityAggregate
  join TemporalityPure      TemporalityElement   = Just TemporalityElement
  join TemporalityElement   TemporalityPure      = Just TemporalityElement
  join y z
   | y == z    = Just y
   | otherwise = Nothing

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
