-- | Base definitions common across all languages.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}

module Icicle.Common.Base (
      Name   (..)
    , BaseValue (..)
    , StructField (..)
    , ExceptionInfo (..)
    , OutputName (..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Data.DateTime

import              P
import qualified    Data.Map    as Map
import qualified    Data.Text   as T


-- | User defined names.
data Name n =
 -- | Raw name
   Name     n
 -- | Prefix a name.
 -- Very useful for generating fresh(ish) readable names.
 | NameMod  n (Name n)
 deriving (Eq,Ord,Show,Functor)


-- | Base values - real values that can be serialised and whatnot
-- These are used in the expressions, but actual values can be
-- closures, which include expressions.
-- This is in here to resolve circular dependency.
data BaseValue
 = VInt   Int
 | VDouble Double
 | VUnit
 | VBool  Bool
 | VDateTime        DateTime
 | VString T.Text
 | VArray [BaseValue]
 | VPair  BaseValue BaseValue
 | VSome  BaseValue
 | VNone
 | VMap    (Map.Map BaseValue    BaseValue)
 | VStruct (Map.Map StructField  BaseValue)

 | VException ExceptionInfo
 deriving (Show, Ord, Eq)

data ExceptionInfo
 = ExceptFold1NoValue
 | ExceptScalarVariableNotAvailable
 deriving (Show, Ord, Eq)


data StructField
 = StructField
 { nameOfStructField :: T.Text
 }
 deriving (Show, Ord, Eq)


newtype OutputName
 = OutputName
 { unOutputName :: T.Text }
 deriving (Eq, Ord, Show)


-- Pretty printing ---------------

instance Pretty n => Pretty (Name n) where
 pretty (Name n)        = pretty n
 pretty (NameMod p n)   = pretty p <> text "$" <> pretty n

instance Pretty BaseValue where
 pretty v
  = case v of
     VInt i
      -> pretty i
     VDouble i
      -> pretty i
     VUnit
      -> text "()"
     VBool b
      -> pretty b
     VDateTime dt
      -> text $ T.unpack $ renderDate dt
     VString t
      -> text $ show t
     VArray vs
      -> pretty vs
     VPair a b
      -> text "(" <> pretty a <> text ", " <> pretty b <> text ")"
     VSome a
      -> text "Some" <+> pretty a
     VNone
      -> text "None"
     VMap mv
      -> text "Map" <+> pretty (Map.toList mv)
     VStruct mv
      -> text "Struct" <+> pretty (Map.toList mv)
     VException e
      -> text "Exception:" <+> pretty e

instance Pretty StructField where
 pretty = text . T.unpack . nameOfStructField

instance Pretty ExceptionInfo where
 pretty ExceptFold1NoValue
        = text "Fold1, but there is no value"
 pretty ExceptScalarVariableNotAvailable
        = text "Scalar variable not available here"

instance Pretty OutputName where
 pretty = pretty . unOutputName

