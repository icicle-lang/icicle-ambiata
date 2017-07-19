-- | Source programs have different types than Core and Avalanche:
-- In Source, we need to infer which stage of the computation,
-- so each type is tagged with a universe describing the stage.
--
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Base (
    Type        (..)
  , typeOfValType
  , valTypeOfType
  , Constraint  (..)
  , FunctionType(..)
  , Annot (..)
  , annotDiscardConstraints
  , prettyFun
  ) where

import qualified Data.Map as Map

import           GHC.Generics (Generic)

import           Icicle.Common.Base
import qualified Icicle.Common.Type as CT

import           Icicle.Internal.Pretty

import           P


data Type n
 = BoolT
 | TimeT
 | DoubleT
 | IntT
 | StringT
 | UnitT
 | ErrorT

 | ArrayT   (Type n)
 | GroupT   (Type n) (Type n)
 | OptionT  (Type n)
 | PairT    (Type n) (Type n)
 | SumT     (Type n) (Type n)
 | StructT  (Map.Map CT.StructField (Type n))

 | Temporality         (Type n) (Type n)
 | TemporalityPure
 | TemporalityElement
 | TemporalityAggregate

 | Possibility         (Type n) (Type n)
 | PossibilityPossibly
 | PossibilityDefinitely

 | TypeVar             (Name n)
 deriving (Eq, Ord, Show, Generic)

instance NFData n => NFData (Type n)

typeOfValType :: CT.ValType -> Type n
typeOfValType vt
 = case vt of
    CT.BoolT        -> BoolT
    CT.TimeT        -> TimeT
    CT.DoubleT      -> DoubleT
    CT.IntT         -> IntT
    CT.StringT      -> StringT
    CT.UnitT        -> UnitT
    CT.ErrorT       -> ErrorT

    -- Fact identifiers are represented as integers.
    CT.FactIdentifierT
                    -> IntT

    CT.ArrayT a     -> ArrayT (go a)
    CT.BufT  _ a    -> ArrayT (go a)
    CT.MapT  k v    -> GroupT (go k) (go v)
    CT.OptionT a    -> OptionT (go a)
    CT.PairT a b    -> PairT (go a) (go b)
    CT.SumT  a b    -> SumT  (go a) (go b)
    CT.StructT st   -> StructT (Map.map go $ CT.getStructType st)
 where
  go = typeOfValType

valTypeOfType :: Type n -> Maybe CT.ValType
valTypeOfType bt
 = case bt of
    BoolT        -> return CT.BoolT
    TimeT        -> return CT.TimeT
    DoubleT      -> return CT.DoubleT
    IntT         -> return CT.IntT
    StringT      -> return CT.StringT
    UnitT        -> return CT.UnitT
    ErrorT       -> return CT.ErrorT
    ArrayT a     -> CT.ArrayT  <$> go a
    GroupT k v   -> CT.MapT    <$> go k <*> go v
    OptionT a    -> CT.OptionT <$> go a
    PairT a b    -> CT.PairT   <$> go a <*> go b
    SumT  a b    -> CT.SumT    <$> go a <*> go b
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
 | CPossibilityOfNum (Type n) (Type n)
 | CTemporalityJoin (Type n) (Type n) (Type n)
 | CReturnOfLetTemporalities (Type n) (Type n) (Type n)
 | CDataOfLatest (Type n) (Type n) (Type n) (Type n)
 | CPossibilityOfLatest (Type n) (Type n) (Type n)
 | CPossibilityJoin (Type n) (Type n) (Type n)
 deriving (Eq, Ord, Show, Generic)

instance NFData n => NFData (Constraint n)

data FunctionType n
 = FunctionType
 { functionForalls      :: [Name n]
 , functionConstraints  :: [Constraint n]
 , functionArguments    :: [Type n]
 , functionReturn       :: Type n
 }
 deriving (Eq, Ord, Show, Generic)

instance NFData n => NFData (FunctionType n)

data Annot a n
 = Annot
 { annAnnot         :: a
 , annResult        :: Type n
 , annConstraints   :: [(a, Constraint n)]
 }
 deriving (Eq, Ord, Show, Generic)

instance (NFData a, NFData n) => NFData (Annot a n)


annotDiscardConstraints :: Annot a n -> (a, Type n)
annotDiscardConstraints ann
 = (annAnnot ann, annResult ann)



instance Pretty n => Pretty (Type n) where
  prettyPrec p = \case
    IntT ->
      prettyConstructor "Int"
    DoubleT ->
      prettyConstructor "Double"
    UnitT ->
      prettyConstructor "Unit"
    ErrorT ->
      prettyConstructor "ErrorT"
    BoolT ->
      prettyConstructor "Bool"
    TimeT ->
      prettyConstructor "Time"
    StringT ->
      prettyConstructor "String"
    ArrayT t ->
      prettyApp hsep p (prettyConstructor "Array") [t]
    GroupT k v ->
      prettyApp hsep p (prettyConstructor "Group") [k, v]
    OptionT a ->
      prettyApp hsep p (prettyConstructor "Option") [a]
    PairT a b ->
      parens $
        pretty a <> annotate AnnPunctuation (text ",") <+> pretty b
    SumT a b ->
      prettyApp hsep p (prettyConstructor "Sum") [a, b]
    StructT fs ->
      prettyStructType hcat . fmap (bimap pretty pretty) $ Map.toList fs
    TypeVar v ->
      annotate AnnVariable (pretty v)

    Temporality a b ->
      prettyApp hsep p a [b]
    TemporalityPure ->
      prettyConstructor "Pure"
    TemporalityElement ->
      prettyConstructor "Element"
    TemporalityAggregate ->
      prettyConstructor "Aggregate"

    Possibility a b ->
      prettyApp hsep p a [b]
    PossibilityPossibly ->
      prettyConstructor "Possibly"
    PossibilityDefinitely ->
      prettyConstructor "Definitely"


instance Pretty n => Pretty (Constraint n) where
  pretty = \case
    CEquals p q ->
      pretty p <+> prettyPunctuation "=:" <+> pretty q

    CIsNum t ->
      prettyConstructor "Num" <+> pretty t

    CPossibilityOfNum ret t ->
      pretty ret <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "PossibilityOfNum") [t]

    CTemporalityJoin ret a b ->
      pretty ret <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "TemporalityJoin") [a, b]

    CReturnOfLetTemporalities t def body ->
      pretty t <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "ReturnOfLet") [def, body]

    CDataOfLatest t tmp pos dat ->
      pretty t <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "DataOfLatest") [tmp, pos, dat]

    CPossibilityOfLatest t tmp dat ->
      pretty t <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "PossibilityOfLatest") [tmp, dat]

    CPossibilityJoin a b c ->
      pretty a <+> prettyPunctuation "=:" <+>
      prettyApp hsep 0 (prettyConstructor "PossibilityJoin") [b, c]

prettyFun :: Pretty n => FunctionType n -> PrettyFunType
prettyFun fun =
  PrettyFunType
    (fmap pretty $ functionConstraints fun)
    (fmap pretty $ functionArguments fun)
    (pretty $ functionReturn fun)

instance Pretty n => Pretty (FunctionType n) where
  pretty =
    pretty . prettyFun

instance (Pretty n) => Pretty (Annot a n) where
  pretty ann =
    prettyItems sep (pretty $ annResult ann) $
      fmap (PrettyItem (prettyPunctuation "=>") . pretty . snd) (annConstraints ann)
