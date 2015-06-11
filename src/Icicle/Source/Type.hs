-- | Source programs have different types than Core and Avalanche:
-- In Source, we need to infer which stage of the computation,
-- so each type is tagged with a universe describing the stage.
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Type (
    BaseType
  , isEnum
  , Universe(..)
  , isPureOrElem
  , isAgg
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
 = Pure
 | Elem
 | Group BaseType
 | AggU
 deriving (Eq, Ord, Show)

isPureOrElem :: Universe -> Bool
isPureOrElem u
 = case u of
    Pure -> True
    Elem -> True
    _    -> False

isAgg :: Universe -> Bool
isAgg u
 = case u of
    AggU  -> True
    _    -> False



data UniverseType
 = UniverseType
 { universe :: Universe
 , baseType :: BaseType }
 deriving (Eq, Ord, Show)


data FunctionType
 = FunctionType [UniverseType] UniverseType
 deriving (Eq, Ord, Show)


instance Pretty Universe where
 pretty Pure        = ""
 pretty Elem        = "Elem"
 pretty (Group t)   = "Group" <+> pretty t
 pretty AggU        = "Agg"

instance Pretty UniverseType where
 pretty (UniverseType Pure t) = pretty t
 pretty (UniverseType u    t) = pretty u <+> pretty t

instance Pretty FunctionType where
 pretty (FunctionType [] t) = pretty t
 pretty (FunctionType (x:xs) t)
  = pretty x <+> "->" <+> pretty (FunctionType xs t)

