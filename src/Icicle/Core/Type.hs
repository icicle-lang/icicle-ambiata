{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Type (
      ValType (..)
    , FunType (..)
    , Type

    , canApply

    ) where

import              Icicle.Internal.Pretty

import              P

data ValType =
   IntT
 | ArrayT ValType
 | PairT  ValType ValType
 deriving (Eq,Ord,Show)
 -- | Struct | String | Double | ...

data FunType =
 FunT [FunType] ValType
 deriving (Eq,Ord,Show)

-- Top-level type
type Type = FunType


canApply :: Type -> Type -> Maybe Type
canApply (FunT args p) q
 = case args of
    (b:bs)
     | b == q
     -> Just (FunT bs p)
    _
     -> Nothing


-- Pretty printing ---------------

instance Pretty ValType where
 pretty IntT            = text "Int"
 pretty (ArrayT t)      = text "[" <> pretty t <> text "]"
 pretty (PairT a b)     = text "(" <> pretty a <> text ", " <> pretty b <> text ")"


instance Pretty FunType where
 pretty (FunT [] t)     = pretty t
 pretty (FunT (b:bs) t) = inner b <> text " -> " <> pretty (FunT bs t)
  where
   inner i@(FunT [] _) = pretty i
   inner i             = text "(" <> pretty i <> text ")"


