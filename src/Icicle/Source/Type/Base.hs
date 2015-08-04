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

import                  Icicle.Common.Type

import                  Icicle.Internal.Pretty


import                  P

data BaseType n
 = BaseType (ValType' (BaseType n))
 | BaseTypeVar n
 | BaseTypeExistential n
 deriving (Eq, Ord, Show)

valTypeOfBaseType :: BaseType n -> Maybe ValType
valTypeOfBaseType bt
 = case bt of
    BaseType IntT -> return $ ValType IntT
    BaseType DoubleT -> return $ ValType DoubleT
    BaseType StringT -> return $ ValType StringT
    BaseType UnitT     -> return $ ValType UnitT
    BaseType BoolT     -> return $ ValType BoolT
    BaseType DateTimeT -> return $ ValType DateTimeT
    BaseType (ArrayT a) -> (ValType . ArrayT) <$> valTypeOfBaseType a
    BaseType (MapT a b)  -> ValType <$> (MapT <$> valTypeOfBaseType a <*> valTypeOfBaseType b)
    BaseType (OptionT a) -> (ValType . OptionT) <$> valTypeOfBaseType a
    BaseType (PairT a b) -> ValType <$> (PairT <$> valTypeOfBaseType a <*> valTypeOfBaseType b)
    BaseType (StructT st) -> (ValType . StructT . StructType) <$> traverse valTypeOfBaseType (getStructType st)
    BaseTypeVar _ -> Nothing
    BaseTypeExistential _ -> Nothing

-- | Can this type be used as a grouping?
-- Very conservative for now.
isEnum :: BaseType n -> Bool
isEnum t
 = case t of
    BaseType IntT       -> True
    BaseType BoolT      -> True
    BaseType DateTimeT  -> True
    _                   -> False

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
    AggU  -> True
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
 = UniverseType (Universe AggU (universePossibility $ universe g)) (BaseType $ MapT tk $ baseType g)
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

instance Pretty n => Pretty (BaseType n) where
 pretty (BaseType t) = pretty t
 pretty (BaseTypeVar n) = pretty n
 pretty (BaseTypeExistential n) = "?" <> pretty n


