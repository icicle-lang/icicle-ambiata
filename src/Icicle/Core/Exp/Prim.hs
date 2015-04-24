{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Prim (
      Prim   (..)
    , typeOfPrim
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Type

import              P


data Prim
 = PrimPlus
 | PrimMinus
 deriving (Eq, Ord, Show)

typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    PrimPlus
     -> FunT [intT, intT] IntT
    PrimMinus
     -> FunT [intT, intT] IntT

 where
  intT = FunT [] IntT

instance Pretty Prim where
 pretty PrimPlus  = text "add"
 pretty PrimMinus = text "sub"
