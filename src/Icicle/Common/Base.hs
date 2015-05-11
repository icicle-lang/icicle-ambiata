-- | Base definitions common across all languages.
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Base (
      Name   (..)
    , BaseValue (..)
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
 deriving (Eq,Ord,Show)


-- | Base values - real values that can be serialised and whatnot
-- These are used in the expressions, but actual values can be
-- closures, which include expressions.
-- This is in here to resolve circular dependency.
data BaseValue
 = VInt   Int
 | VBool  Bool
 | VDateTime        DateTime
 | VArray [BaseValue]
 | VPair  BaseValue BaseValue
 | VSome  BaseValue
 | VNone
 | VMap   (Map.Map BaseValue BaseValue)
 deriving (Show, Ord, Eq)



-- Pretty printing ---------------

instance Pretty n => Pretty (Name n) where
 pretty (Name n)        = pretty n
 pretty (NameMod p n)   = pretty p <> text "$" <> pretty n

instance Pretty BaseValue where
 pretty v
  = case v of
     VInt i
      -> pretty i
     VBool b
      -> pretty b
     VDateTime dt
      -> text $ T.unpack $ renderDate dt
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

