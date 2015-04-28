-- | Base definitions for Core programs.
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Base (
      Name (..)
    ) where

import              Icicle.Internal.Pretty

import              P


-- | User defined names.
data Name n =
 -- | Raw name
   Name     n
 -- | Prefix a name.
 -- Very useful for generating fresh(ish) readable names.
 | NameMod  n (Name n)
 deriving (Eq,Ord,Show)


-- Pretty printing ---------------

instance Pretty n => Pretty (Name n) where
 pretty (Name n)        = pretty n
 pretty (NameMod p n)   = pretty p <> text "$" <> pretty n
