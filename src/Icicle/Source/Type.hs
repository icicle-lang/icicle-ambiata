-- | Source programs have different types than Core and Avalanche:
-- In Source, we need to infer which stage of the computation,
-- so each type is tagged with a universe describing the stage.
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type (
    BaseType
  , isEnum
  , Universe(..)
  , Temporality(..)
  , Possibility(..)
  , isPureOrElem
  , isAgg
  , isGroup
  , maxOf
  , maxOfPossibility
  , castPossibilityWith
  , possibly, definitely
  , canCastTemporality
  , canCastPossibility
  , UniverseType(..)
  , FunctionType(..)
  ) where

import                  Icicle.Common.Type

import                  Icicle.Internal.Pretty

import                  P

type BaseType
 = ValType

-- | Can this type be used as a grouping?
-- Very conservative for now.
isEnum :: BaseType -> Bool
isEnum t
 = case t of
    IntT        -> True
    BoolT       -> True
    DateTimeT   -> True
    _           -> False

data Universe
 = Universe
 { universeTemporality :: Temporality
 , universePossibility :: Possibility }
 deriving (Eq, Ord, Show)

data Temporality
 = Pure
 | Elem
 | Group BaseType
 | AggU
 deriving (Eq, Ord, Show)


data Possibility
 = Possibly
 | Definitely
 deriving (Eq, Ord, Show)

isPureOrElem :: Universe -> Bool
isPureOrElem (Universe u _)
 = case u of
    Pure -> True
    Elem -> True
    _    -> False

isAgg :: Universe -> Bool
isAgg (Universe u _)
 = case u of
    AggU  -> True
    _    -> False


maxOf :: Universe -> Universe -> Maybe Universe
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


maxOfPossibility :: Possibility -> Possibility -> Possibility
maxOfPossibility a b
 = let p | canCastPossibility a b
         = b
         | canCastPossibility b a
         = a
         | otherwise
         = Possibly
  in p

castPossibilityWith :: Universe -> Universe -> Universe
castPossibilityWith u1 u2
 = u1
 { universePossibility = maxOfPossibility (universePossibility u1) (universePossibility u2) }

definitely :: Universe -> Universe
definitely u
 = u { universePossibility = Definitely }

possibly   :: Universe -> Universe
possibly u
 = u { universePossibility = Possibly }


canCastTemporality :: Temporality -> Temporality -> Bool
canCastTemporality a b
 | a == Pure
 = True
 | a == b
 = True
 | otherwise
 = False

canCastPossibility :: Possibility -> Possibility -> Bool
canCastPossibility a b
 | a == Definitely
 = True
 | a == b
 = True
 | otherwise
 = False


isGroup :: Universe -> Bool
isGroup (Universe u _)
 | Group _ <- u
 = True
 | otherwise
 = False



data UniverseType
 = UniverseType
 { universe :: Universe
 , baseType :: BaseType }
 deriving (Eq, Ord, Show)


data FunctionType
 = FunctionType [UniverseType] UniverseType
 deriving (Eq, Ord, Show)


instance Pretty Universe where
 pretty (Universe t p) = pretty t <+> pretty p

instance Pretty Temporality where
 pretty Pure        = ""
 pretty Elem        = "Elem"
 pretty (Group t)   = "Group" <+> pretty t
 pretty AggU        = "Agg"

instance Pretty Possibility where
 pretty Possibly    = "Possibly"
 pretty Definitely  = ""

instance Pretty UniverseType where
 pretty (UniverseType u    t) = pretty u <+> pretty t

instance Pretty FunctionType where
 pretty (FunctionType [] t) = pretty t
 pretty (FunctionType (x:xs) t)
  = pretty x <+> "->" <+> pretty (FunctionType xs t)

