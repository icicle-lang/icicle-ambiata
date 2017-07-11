-- | Base definitions common across all languages.
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Base (
      Name
    , NameBase(..)
    , nameOf
    , nameHash
    , nameBase
    , modName

    , BaseValue(..)
    , StructField(..)
    , ExceptionInfo(..)
    , WindowUnit(..)
    , FactIdentifier(..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Data.Time

import              P

import qualified    Data.Map    as Map
import qualified    Data.Text   as T
import              Data.Hashable

import              GHC.Generics (Generic)


data Name n = Name {
    nameHash :: {-# UNPACK #-} !Int
  , nameBase ::                !(NameBase n)
  } deriving (Show, Generic)

instance Hashable (Name n) where
  hash           (Name h _) = h
  hashWithSalt s (Name h _) = hashWithSalt s h

instance Eq n => Eq  (Name n) where
  (==) x y = (nameHash x == nameHash y) && (nameBase x == nameBase y)

instance Eq n => Ord (Name n) where
  compare x y = compare (nameHash x) (nameHash y)

-- | User defined names.
data NameBase n =
 -- | Raw name
   NameBase !n
 -- | Prefix a name.
 -- Very useful for generating fresh(ish) readable names.
 | NameMod  !n !(NameBase n)
 deriving (Eq, Ord, Show, Functor, Generic)

instance Hashable n => Hashable (NameBase n)

nameOf :: Hashable n => NameBase n -> Name n
nameOf n = Name (hash n) n

modName :: Hashable n => n -> Name n -> Name n
modName prefix = nameOf . NameMod prefix . nameBase


instance NFData (NameBase n) where rnf x = seq x ()
instance NFData (Name n)     where rnf x = seq x ()

--------------------------------------------------------------------------------

data WindowUnit
 = Days   !Int
 | Months !Int
 | Weeks  !Int
 deriving (Show, Eq, Ord, Generic)

instance NFData WindowUnit where rnf x = seq x ()

-- | Base values - real values that can be serialised and whatnot
-- These are used in the expressions, but actual values can be
-- closures, which include expressions.
-- This is in here to resolve circular dependency.
data BaseValue
 = VInt      !Int
 | VDouble   !Double
 | VUnit
 | VBool     !Bool
 | VTime     !Time
 | VString   !T.Text
 | VArray    ![BaseValue]
 | VPair     !BaseValue !BaseValue
 | VLeft     !BaseValue
 | VRight    !BaseValue
 | VNone
 | VSome     !BaseValue
 | VMap      !(Map.Map BaseValue    BaseValue)
 | VStruct   !(Map.Map StructField  BaseValue)
 | VBuf      ![BaseValue]
 | VError    !ExceptionInfo
 | VFactIdentifier !FactIdentifier
 deriving (Show, Ord, Eq, Generic)

instance NFData BaseValue where rnf x = seq x ()

-- | Fact identifiers are represented as indices into the input stream for
-- Core and Avalanche evaluators, but for a real streaming model such as C
-- we need to convert this as a unique (and consistent) identifier across runs.
-- Perhaps a pair of timestamp and index
newtype FactIdentifier
 = FactIdentifier
 { getFactIdentifierIndex :: Int }
 deriving (Eq, Ord, Show, Generic)

instance NFData FactIdentifier where rnf x = seq x ()

-- | Called "exceptions"
-- because they aren't really errors,
-- but they aren't really good values either..
data ExceptionInfo
 -- | This particular exception is for packing (SumT ErrorT a) together into (ErrorT, a)
 = ExceptNotAnError
 | ExceptTombstone
 | ExceptFold1NoValue
 | ExceptCannotCompute
 deriving (Show, Ord, Eq, Generic)

instance NFData ExceptionInfo where rnf x = seq x ()

newtype StructField
 = StructField
 { nameOfStructField :: T.Text
 }
 deriving (Ord, Eq, Generic)

instance NFData StructField where rnf x = seq x ()

instance Show StructField where
 showsPrec p (StructField x)
  = showParen (p > 10) (showString "StructField " . showsPrec 11 x)

-- Pretty printing ---------------

instance Pretty n => Pretty (Name n) where
 pretty (Name _ n) = pretty n

instance Pretty n => Pretty (NameBase n) where
 pretty (NameBase n)   = pretty n
 pretty (NameMod  p n) = pretty p <> text "-" <> pretty n

instance Pretty BaseValue where
  prettyPrec p = \case
    VInt i ->
      annotate AnnConstant $ pretty i
    VDouble i ->
      annotate AnnConstant $ pretty i
    VUnit ->
      prettyPunctuation "()"
    VBool b ->
      annotate AnnConstant $ pretty b
    VTime t ->
      annotate AnnConstant $ text $ T.unpack $ renderOutputTime t
    VString t ->
      annotate AnnConstant $ text $ show t
    VArray vs ->
      pretty vs
    VPair a b ->
      pretty (a, b)
    VLeft a ->
      prettyApp hsep p (prettyConstructor "Left") [a]
    VRight a ->
      prettyApp hsep p (prettyConstructor "Right") [a]
    VSome a ->
      prettyApp hsep p (prettyConstructor "Some") [a]
    VNone ->
      prettyConstructor "None"
    VMap mv ->
      prettyApp hsep p (prettyConstructor "Map") [Map.toList mv]
    VStruct mv ->
      prettyApp hsep p (prettyConstructor "Struct") [Map.toList mv]
    VError e ->
      pretty e
    VFactIdentifier f ->
      pretty f
    VBuf vs ->
      prettyApp hsep p (prettyConstructor "Buf") [vs]

instance Pretty FactIdentifier where
  prettyPrec p f =
    prettyApp hsep p (prettyConstructor "FactIdentifier") [getFactIdentifierIndex f]

instance Pretty StructField where
  pretty =
     text . T.unpack . nameOfStructField

instance Pretty ExceptionInfo where
  pretty =
    prettyConstructor . show

instance Pretty WindowUnit where
  pretty = \case
    Days i ->
      annotate AnnConstant (pretty i) <+> prettyKeyword "days"
    Months i ->
      annotate AnnConstant (pretty i) <+> prettyKeyword "months"
    Weeks i ->
      annotate AnnConstant (pretty i) <+> prettyKeyword "weeks"
