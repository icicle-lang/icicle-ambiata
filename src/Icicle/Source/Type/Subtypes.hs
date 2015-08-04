{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Subtypes (
    isSubtype
  , isBottom
  , isTop
  ) where

import Icicle.Common.Type
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
 | p == BaseType IntT
 , q == BaseType DoubleT
 = True
 -- (Struct)
 | BaseType (StructT (StructType ps)) <- p
 , BaseType (StructT (StructType qs)) <- q
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
 | BaseType DoubleT <- p
 = False
 -- {} <: {n : t}
 | BaseType (StructT (StructType ps)) <- p
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
 | BaseType IntT <- p
 = False
 -- {...} <: {..., n : t}
 | BaseType (StructT _) <- p
 = False
 | otherwise
 = True

