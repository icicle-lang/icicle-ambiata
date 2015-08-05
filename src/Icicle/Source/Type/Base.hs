-- | Source programs have different types than Core and Avalanche:
-- In Source, we need to infer which stage of the computation,
-- so each type is tagged with a universe describing the stage.
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Base (
    BaseType (..)
  , valTypeOfBaseType
  , baseTypeOfValType
  , isArith
  , isEnum
  , Universe(..)
  , Temporality(..)
  , Possibility(..)
  , isPureOrElem
  , isAgg
  , isGroup
  , unwrapGroup
  , maxOf
  , maxOfPossibility
  , castPossibilityWith
  , possibly, definitely, definitelyUT, possiblyUT
  , canCastTemporality
  , canCastPossibility
  , UniverseType(..)
  , FunctionType(..)
  , function0
  ) where

import qualified        Icicle.Common.Type as CT

import                  Icicle.Internal.Pretty

import                  P

import qualified        Data.Map as Map

data BaseType n
 = BoolT
 | DateTimeT
 | DoubleT
 | IntT
 | StringT
 | UnitT
 | ArrayT (BaseType n)
 | MapT   (BaseType n) (BaseType n)
 | OptionT             (BaseType n)
 | PairT  (BaseType n) (BaseType n)
 | StructT (Map.Map CT.StructField (BaseType n))
 | TypeVar n
 | TypeVarExistential n
 deriving (Eq,Ord,Show)

baseTypeOfValType :: CT.ValType -> BaseType n
baseTypeOfValType vt
 = case vt of
    CT.BoolT        -> BoolT
    CT.DateTimeT    -> DateTimeT
    CT.DoubleT      -> DoubleT
    CT.IntT         -> IntT
    CT.StringT      -> StringT
    CT.UnitT        -> UnitT
    CT.ArrayT a     -> ArrayT (go a)
    CT.MapT  k v    -> MapT (go k) (go v)
    CT.OptionT a    -> OptionT (go a)
    CT.PairT a b    -> PairT (go a) (go b)
    CT.StructT st   -> StructT (Map.map go $ CT.getStructType st)
 where
  go = baseTypeOfValType

valTypeOfBaseType :: BaseType n -> Maybe CT.ValType
valTypeOfBaseType bt
 = case bt of
    BoolT        -> return CT.BoolT
    DateTimeT    -> return CT.DateTimeT
    DoubleT      -> return CT.DoubleT
    IntT         -> return CT.IntT
    StringT      -> return CT.StringT
    UnitT        -> return CT.UnitT
    ArrayT a     -> CT.ArrayT  <$> go a
    MapT  k v    -> CT.MapT    <$> go k <*> go v
    OptionT a    -> CT.OptionT <$> go a
    PairT a b    -> CT.PairT   <$> go a <*> go b
    StructT st   -> (CT.StructT . CT.StructType)
                <$> traverse go st

    TypeVar _    -> Nothing
    TypeVarExistential _
                 -> Nothing
 where
  go = valTypeOfBaseType

-- | Can this type be used as a grouping?
-- Very conservative for now.
isEnum :: BaseType n -> Bool
isEnum t
 = case t of
    IntT        -> True
    BoolT       -> True
    DateTimeT   -> True
    _           -> False

-- | Is this type a number?
isArith :: BaseType n -> Bool
isArith t
 = case t of
    IntT        -> True
    DoubleT     -> True
    _           -> False


data Universe n
 = Universe
 { universeTemporality :: Temporality n
 , universePossibility :: Possibility }
 deriving (Eq, Ord, Show)

data Temporality n
 = Pure
 | Elem
 | Group (BaseType n)
 | AggU
 deriving (Eq, Ord, Show)


data Possibility
 = Possibly
 | Definitely
 deriving (Eq, Ord, Show)

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


maxOfPossibility :: Possibility -> Possibility -> Possibility
maxOfPossibility a b
 = let p | canCastPossibility a b
         = b
         | canCastPossibility b a
         = a
         | otherwise
         = Possibly
  in p

castPossibilityWith :: Universe n -> Universe n -> Universe n
castPossibilityWith u1 u2
 = u1
 { universePossibility = maxOfPossibility (universePossibility u1) (universePossibility u2) }

definitely :: Universe n -> Universe n
definitely u
 = u { universePossibility = Definitely }

possibly   :: Universe n -> Universe n
possibly u
 = u { universePossibility = Possibly }

definitelyUT :: UniverseType n -> UniverseType n
definitelyUT u
 = u { universe = definitely $ universe u }

possiblyUT :: UniverseType n -> UniverseType n
possiblyUT u
 = u { universe = possibly $ universe u }


canCastTemporality :: Eq n => Temporality n -> Temporality n -> Bool
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


isGroup :: Universe n -> Bool
isGroup (Universe u _)
 | Group _ <- u
 = True
 | otherwise
 = False

unwrapGroup :: UniverseType n -> UniverseType n
unwrapGroup g
 | Group tk <- universeTemporality $ universe g
 = UniverseType (Universe AggU (universePossibility $ universe g)) (MapT tk $ baseType g)
 | otherwise
 = g


data UniverseType n
 = UniverseType
 { universe :: Universe n
 , baseType :: BaseType n }
 deriving (Eq, Ord, Show)


data FunctionType n
 = FunctionType [UniverseType n] (UniverseType n)
 deriving (Eq, Ord, Show)
function0 :: UniverseType n -> FunctionType n
function0 u
 = FunctionType [] u


instance Pretty n => Pretty (BaseType n) where
 pretty IntT            = text "Int"
 pretty DoubleT         = text "Double"
 pretty UnitT           = text "Unit"
 pretty BoolT           = text "Bool"
 pretty DateTimeT       = text "DateTime"
 pretty StringT         = text "String"
 pretty (ArrayT t)      = parens (text "Array " <> pretty t)
 pretty (MapT k v)      = parens (text "Map" <+> pretty k <+> pretty v)
 pretty (OptionT a)     = parens (text "Option" <+> pretty a)
 pretty (PairT a b)     = text "(" <> pretty a <> text ", " <> pretty b <> text ")"
 pretty (StructT fs)    = parens (text "Struct" <+> pretty (Map.toList fs))
 pretty (TypeVar n)     = pretty n
 pretty (TypeVarExistential n)
                        = text "?" <> pretty n

instance Pretty n => Pretty (Universe n) where
 pretty (Universe t p) = pretty t <+?> pretty p

instance Pretty n => Pretty (Temporality n) where
 pretty Pure        = ""
 pretty Elem        = "Elem"
 pretty (Group t)   = "Group" <+?> pretty t
 pretty AggU        = "Agg"

instance Pretty Possibility where
 pretty Possibly    = "Possibly"
 pretty Definitely  = ""

instance Pretty n => Pretty (UniverseType n) where
 pretty (UniverseType u    t) = pretty u <+?> pretty t

instance Pretty n => Pretty (FunctionType n) where
 pretty (FunctionType [] t) = pretty t
 pretty (FunctionType (x:xs) t)
  = pretty x <+> "->" <+> pretty (FunctionType xs t)


