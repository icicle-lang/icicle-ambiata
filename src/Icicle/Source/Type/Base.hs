-- | Source programs have different types than Core and Avalanche:
-- In Source, we need to infer which stage of the computation,
-- so each type is tagged with a universe describing the stage.
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Base (
    BaseType    (..)
  , valTypeOfBaseType
  , baseTypeOfValType
  , Universe    (..)
  , Temporality (..)
  , Possibility (..)
  , UniverseType(..)
  , unwrapGroup
  , Constraint  (..)
  , FunctionType(..)
  , function0
  ) where

import                  Icicle.Common.Base
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
 | BaseTypeVar (Name n)
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
    BaseTypeVar _-> Nothing
 where
  go = valTypeOfBaseType


data Universe n
 = Universe
 { universeTemporality :: Temporality n
 , universePossibility :: Possibility n }
 deriving (Eq, Ord, Show)

data Temporality n
 = Pure
 | Elem
 | Group (BaseType n)
 | AggU
 | TemporalityVar (Name n)
 deriving (Eq, Ord, Show)


data Possibility n
 = Possibly
 | Definitely
 | PossibilityVar (Name n)
 deriving (Eq, Ord, Show)

data UniverseType n
 = UniverseType
 { universe :: Universe n
 , baseType :: BaseType n }
 deriving (Eq, Ord, Show)


unwrapGroup :: UniverseType n -> UniverseType n
unwrapGroup g
 | Universe (Group tk) poss <- universe g
 = UniverseType (Universe AggU poss) (MapT tk $ baseType g)
 | otherwise
 = g


data Constraint n
 = ConstraintBaseType (BaseType n) ConstraintType (BaseType n)
 | ConstraintTemporality (Temporality n) ConstraintType (Temporality n)
 | ConstraintPossibility (Possibility n) ConstraintType (Possibility n)
 | ConstraintUniverseType (UniverseType n) ConstraintType (UniverseType n)
 deriving (Eq, Ord, Show)

data ConstraintType
 = ConstraintSub
 | ConstraintEq
 deriving (Eq, Ord, Show)


data FunctionType n
 = FunctionType
 { functionForalls      :: [Name n]
 , functionConstraints  :: [Constraint n]
 , functionArguments    :: [UniverseType n]
 , functionReturn       :: UniverseType n
 }
 deriving (Eq, Ord, Show)

function0 :: UniverseType n -> FunctionType n
function0 u
 = FunctionType [] [] [] u


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
 pretty (BaseTypeVar v) = pretty v

instance Pretty n => Pretty (Universe n) where
 pretty (Universe t p) = pretty t <+?> pretty p

instance Pretty n => Pretty (Temporality n) where
 pretty Pure        = ""
 pretty Elem        = "Elem"
 pretty (Group t)   = "Group" <+?> pretty t
 pretty AggU        = "Agg"
 pretty (TemporalityVar v) = pretty v

instance Pretty n => Pretty (Possibility n) where
 pretty Possibly    = "Possibly"
 pretty Definitely  = ""
 pretty (PossibilityVar v) = pretty v

instance Pretty n => Pretty (UniverseType n) where
 pretty (UniverseType u    t) = pretty u <+?> pretty t

instance Pretty n => Pretty (Constraint n) where
 pretty (ConstraintBaseType p c q)
  = pretty p <+> pretty c <+> pretty q
 pretty (ConstraintTemporality p c q)
  = pretty p <+> pretty c <+> pretty q
 pretty (ConstraintPossibility p c q)
  = pretty p <+> pretty c <+> pretty q
 pretty (ConstraintUniverseType p c q)
  = pretty p <+> pretty c <+> pretty q

instance Pretty ConstraintType where
 pretty ConstraintSub = "<:"
 pretty ConstraintEq  = "=:"

instance Pretty n => Pretty (FunctionType n) where
 pretty fun
  =  foralls (functionForalls       fun)
  <> constrs (functionConstraints   fun)
  <> args    (functionArguments     fun)
  <> pretty  (functionReturn        fun)
  where
   foralls []
    = ""
   foralls xs
    = "forall" <+> hsep (fmap pretty xs) <> ". "

   constrs []
    = ""
   constrs xs
    = tupled (fmap pretty xs) <> " => "

   args xs
    = hsep (fmap (\x -> pretty x <+> "->") xs)


