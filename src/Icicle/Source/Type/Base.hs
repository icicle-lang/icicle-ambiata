-- | Source programs have different types than Core and Avalanche:
-- In Source, we need to infer which stage of the computation,
-- so each type is tagged with a universe describing the stage.
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Base (
    Type        (..)
  , valTypeOfType
  , typeOfValType
  , Constraint  (..)
  , FunctionType(..)
  , Annot (..)
  , annotDiscardConstraints
  ) where

import                  Icicle.Common.Base
import qualified        Icicle.Common.Type as CT

import                  Icicle.Internal.Pretty

import                  P

import                  Data.List (intersperse)
import qualified        Data.Map as Map

data Type n
 = BoolT
 | DateTimeT
 | DoubleT
 | IntT
 | StringT
 | UnitT

 | ArrayT   (Type n)
 | GroupT   (Type n) (Type n)
 | OptionT  (Type n)
 | PairT    (Type n) (Type n)
 | StructT (Map.Map CT.StructField (Type n))

 | Temporality         (Type n) (Type n)
 | TemporalityPure
 | TemporalityElement
 | TemporalityAggregate

 | Possibility         (Type n) (Type n)
 | PossibilityPossibly
 | PossibilityDefinitely

 | TypeVar             (Name n)
 deriving (Eq,Ord,Show)

typeOfValType :: CT.ValType -> Type n
typeOfValType vt
 = case vt of
    CT.BoolT        -> BoolT
    CT.DateTimeT    -> DateTimeT
    CT.DoubleT      -> DoubleT
    CT.IntT         -> IntT
    CT.StringT      -> StringT
    CT.UnitT        -> UnitT
    CT.ArrayT a     -> ArrayT (go a)
    CT.MapT  k v    -> GroupT (go k) (go v)
    CT.OptionT a    -> OptionT (go a)
    CT.PairT a b    -> PairT (go a) (go b)
    CT.StructT st   -> StructT (Map.map go $ CT.getStructType st)
 where
  go = typeOfValType

valTypeOfType :: Type n -> Maybe CT.ValType
valTypeOfType bt
 = case bt of
    BoolT        -> return CT.BoolT
    DateTimeT    -> return CT.DateTimeT
    DoubleT      -> return CT.DoubleT
    IntT         -> return CT.IntT
    StringT      -> return CT.StringT
    UnitT        -> return CT.UnitT
    ArrayT a     -> CT.ArrayT  <$> go a
    GroupT k v   -> CT.MapT    <$> go k <*> go v
    OptionT a    -> CT.OptionT <$> go a
    PairT a b    -> CT.PairT   <$> go a <*> go b
    StructT st   -> (CT.StructT . CT.StructType)
                <$> traverse go st

    Temporality _ a         -> go a
    TemporalityPure         -> Nothing
    TemporalityElement      -> Nothing
    TemporalityAggregate    -> Nothing

    Possibility _ a         -> go a
    PossibilityPossibly     -> Nothing
    PossibilityDefinitely   -> Nothing

    TypeVar _               -> Nothing
 where
  go = valTypeOfType



data Constraint n
 = CEquals (Type n) (Type n)
 | CIsNum (Type n)
 | CReturnOfLetTemporalities (Type n) (Type n) (Type n)
 | CReturnOfLatest (Type n) (Type n) (Type n)
 deriving (Eq, Ord, Show)


data FunctionType n
 = FunctionType
 { functionForalls      :: [Name n]
 , functionConstraints  :: [Constraint n]
 , functionArguments    :: [Type n]
 , functionReturn       :: Type n
 }
 deriving (Eq, Ord, Show)


data Annot a n
 = Annot
 { annAnnot         :: a
 , annResult        :: Type n
 , annConstraints   :: [(a, Constraint n)]
 }
 deriving (Eq, Ord, Show)


annotDiscardConstraints :: Annot a n -> (a, Type n)
annotDiscardConstraints ann
 = (annAnnot ann, annResult ann)



instance Pretty n => Pretty (Type n) where
 pretty IntT            = text "Int"
 pretty DoubleT         = text "Double"
 pretty UnitT           = text "Unit"
 pretty BoolT           = text "Bool"
 pretty DateTimeT       = text "DateTime"
 pretty StringT         = text "String"
 pretty (ArrayT t)      = parens (text "Array " <> pretty t)
 pretty (GroupT k v)    = parens (text "Group" <+> pretty k <+> pretty v)
 pretty (OptionT a)     = parens (text "Option" <+> pretty a)
 pretty (PairT a b)     = text "(" <> pretty a <> text ", " <> pretty b <> text ")"
 pretty (StructT fs)    = parens (text "Struct" <+> pretty (Map.toList fs))
 pretty (TypeVar v) = pretty v

 pretty (Temporality a b) = pretty a <+> pretty b
 pretty TemporalityPure   = "Pure"
 pretty TemporalityElement = "Element"
 pretty TemporalityAggregate = "Aggregate"

 pretty (Possibility a b) = pretty a <+> pretty b
 pretty PossibilityPossibly = "Possibly"
 pretty PossibilityDefinitely = "Definitely"


instance Pretty n => Pretty (Constraint n) where
 pretty (CEquals p q)
  = pretty p <+> "=:" <+> pretty q
 pretty (CIsNum p)
  = "Num" <+> pretty p
 pretty (CReturnOfLetTemporalities t def body)
  = pretty t <+> "=: ReturnOfLet" <+> pretty def <+> pretty body
 pretty (CReturnOfLatest t tmp dat)
  = pretty t <+> "=: ReturnOfLatest" <+> pretty tmp <+> pretty dat


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
    = hsep (fmap (\x -> pretty x <+> "-> ") xs)


instance (Pretty n) => Pretty (Annot a n) where
 pretty ann
  | [] <- annConstraints ann
  = pretty (annResult ann)
  | otherwise
  = "(" <> hsep (intersperse ", " $ fmap (pretty.snd) $ annConstraints ann)
  <> ") => " <> pretty (annResult ann)

