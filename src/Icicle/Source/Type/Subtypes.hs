{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Subtypes (
    isSubtype
  , isBottom
  , isTop
  , isArith
  , isPureOrElem
  , isAgg
  , isGroup
  , maxOf
  , maxOfPossibility
  , castPossibilityWith
  , canCastTemporality
  , canCastPossibility
  ) where

import Icicle.Source.Type.Base

import qualified    Data.Map as Map

import              P

-- | Check if one type is a subtype of another
--  ---------
-- | T <: T' |
--  ---------
--
-- -------- (Id)        --------------- (Double)
--  t <: t               Int <: Double
--
--  \forall f : t \in ps. f : t \in qs
-- ------------------------------------ (Struct)
--            {ps} <: {qs}
--
isSubtype :: Eq n => BaseType n -> BaseType n -> Bool
isSubtype p q
 -- (Id)
 | p == q
 = True
 -- (Double)
 | p == IntT
 , q == DoubleT
 = True
 -- (Struct)
 | StructT ps <- p
 , StructT qs <- q
 = Map.fold (&&) True
 $ Map.mapWithKey (\k t -> Map.lookup k qs == Just t)
   ps

 | otherwise
 = False

-- | Check if a type is a bottom type.
-- That is, there are no types below it.
--
-- bottom t = \not\exist u. u /= t /\ u <: t
--
isBottom :: BaseType n -> Bool
isBottom p
 -- Int <: Double
 | DoubleT <- p
 = False
 -- {} <: {n : t}
 | StructT ps <- p
 , not $ Map.null ps
 = False

 -- Everything else, only Id applies
 | otherwise
 = True

-- | Check if a type is a top type.
-- That is, there are no types above it.
--
-- top t = \not\exist u. u /= t /\ t <: u
--
isTop :: BaseType n -> Bool
isTop p
 -- Int <: Double
 | IntT <- p
 = False
 -- {...} <: {..., n : t}
 | StructT _ <- p
 = False
 | otherwise
 = True


-- | Is this type a number?
isArith :: BaseType n -> Bool
isArith t
 = case t of
    IntT        -> True
    DoubleT     -> True
    _           -> False

isPureOrElem :: Universe n -> Bool
isPureOrElem (Universe u _)
 = case u of
    Pure -> True
    Elem -> True
    _    -> False

isAgg :: Universe n -> Bool
isAgg (Universe u _)
 = case u of
    AggU -> True
    _    -> False


maxOf :: Eq n => Universe n -> Universe n -> Maybe (Universe n)
maxOf a b
 = let ut u = universeTemporality u
       up u = universePossibility u

       t | canCastTemporality (ut a) (ut b)
         = Just (ut b)
         | canCastTemporality (ut b) (ut a)
         = Just (ut a)
         | otherwise
         = Nothing

       p = maxOfPossibility (up a) (up b)

  in Universe <$> t <*> Just p


maxOfPossibility :: Eq n => Possibility n -> Possibility n -> Possibility n
maxOfPossibility a b
 = let p | canCastPossibility a b
         = b
         | canCastPossibility b a
         = a
         | otherwise
         = Possibly
  in p

castPossibilityWith :: Eq n => Universe n -> Universe n -> Universe n
castPossibilityWith u1 u2
 = u1
 { universePossibility = maxOfPossibility (universePossibility u1) (universePossibility u2) }


canCastTemporality :: Eq n => Temporality n -> Temporality n -> Bool
canCastTemporality a b
 | a == Pure
 = True
 | a == b
 = True
 | otherwise
 = False

canCastPossibility :: Eq n => Possibility n -> Possibility n -> Bool
canCastPossibility a b
 | a == Definitely
 = True
 | a == b
 = True
 | otherwise
 = False


isGroup :: Universe n -> Bool
isGroup u
 | Universe (Group _) _ <- u
 = True
 | otherwise
 = False

