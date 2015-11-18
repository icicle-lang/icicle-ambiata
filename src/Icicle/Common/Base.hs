-- | Base definitions common across all languages.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Icicle.Common.Base (
      Name   (..)
    , BaseValue (..)
    , StructField (..)
    , ExceptionInfo (..)
    , OutputName (..)
    , WindowUnit(..)
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Data.DateTime

import              P
import qualified    Data.Map    as Map
import qualified    Data.Text   as T


-- | User defined names.
data Name n =
 -- | Raw name
   Name     !n
 -- | Prefix a name.
 -- Very useful for generating fresh(ish) readable names.
 | NameMod  !n !(Name n)
 deriving (Eq,Ord,Show,Functor)

data WindowUnit
 = Days   !Int
 | Months !Int
 | Weeks  !Int
 deriving (Show, Eq, Ord)


-- | Base values - real values that can be serialised and whatnot
-- These are used in the expressions, but actual values can be
-- closures, which include expressions.
-- This is in here to resolve circular dependency.
data BaseValue
 = VInt      {-# UNPACK #-}!Int
 | VDouble   {-# UNPACK #-}!Double
 | VUnit
 | VBool     !Bool
 | VDateTime {-# UNPACK #-}!DateTime
 | VString   {-# UNPACK #-}!T.Text
 | VArray    ![BaseValue]
 | VPair     !BaseValue !BaseValue
 | VLeft     !BaseValue
 | VRight    !BaseValue
 | VSome     !BaseValue
 | VNone
 | VMap      !(Map.Map BaseValue    BaseValue)
 | VStruct   !(Map.Map StructField  BaseValue)
 | VBuf      ![BaseValue]
 | VError    !ExceptionInfo
 deriving (Show, Ord, Eq)

-- | Called "exceptions"
-- because they aren't really errors,
-- but they aren't really good values either..
data ExceptionInfo
 = ExceptTombstone
 | ExceptFold1NoValue
 | ExceptScalarVariableNotAvailable
 deriving (Show, Ord, Eq)


newtype StructField
 = StructField
 { nameOfStructField :: T.Text
 }
 deriving (Ord, Eq)


newtype OutputName
 = OutputName
 { unOutputName :: T.Text }
 deriving (Eq, Ord)

instance Show StructField where
 showsPrec p (StructField x)
  = showParen (p > 10) (showString "StructField " . showsPrec 11 x)

instance Show OutputName where
 showsPrec p (OutputName x)
  = showParen (p > 10) (showString "OutputName " . showsPrec 11 x)

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
     VLeft a
      -> text "Left"  <+> pretty a
     VRight a
      -> text "Right" <+> pretty a
     VSome a
      -> text "Some" <+> pretty a
     VNone
      -> text "None"
     VMap mv
      -> text "Map" <+> pretty (Map.toList mv)
     VStruct mv
      -> text "Struct" <+> pretty (Map.toList mv)
     VError e
      -> pretty e
     VBuf vs
      -> text "Buf" <+> pretty vs

instance Pretty StructField where
 pretty = text . T.unpack . nameOfStructField

instance Pretty ExceptionInfo where
 pretty = text . show

instance Pretty OutputName where
 pretty = pretty . unOutputName

instance Pretty WindowUnit where
 pretty wu
  = case wu of
     Days   i -> pretty i <+> "days"
     Months i -> pretty i <+> "months"
     Weeks  i -> pretty i <+> "weeks"

