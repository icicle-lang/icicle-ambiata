-- | Base definitions for Core programs.
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Base (
      Name   (..)
    , Rename (..)
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


-- | This is really a functor but the expressions, program etc
-- are parameterised over "n" and have fields of "Name n"
-- so I did something silly there.
-- Maybe we want a (Type -> Type) and (Prim -> Prim) one too though.
class Rename f where
 rename :: (Name n -> Name n') -> f n -> f n'


-- Pretty printing ---------------

instance Pretty n => Pretty (Name n) where
 pretty (Name n)        = pretty n
 pretty (NameMod p n)   = pretty p <> text "$" <> pretty n
