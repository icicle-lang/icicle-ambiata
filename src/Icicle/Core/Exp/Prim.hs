{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Prim (
      Prim   (..)
    , PrimArith(..)
    , PrimConst(..)
    , typeOfPrim
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Type

import              P


data Prim
 = PrimArith PrimArith
 | PrimConst PrimConst
 deriving (Eq, Ord, Show)

data PrimArith
 = PrimArithPlus
 | PrimArithMinus
 deriving (Eq, Ord, Show)

data PrimConst
 = PrimConstInt Int
 deriving (Eq, Ord, Show)


typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    PrimArith PrimArithPlus
     -> FunT [intT, intT] IntT
    PrimArith PrimArithMinus
     -> FunT [intT, intT] IntT

    PrimConst (PrimConstInt _)
     -> intT
 where
  intT = FunT [] IntT

instance Pretty Prim where
 pretty (PrimArith PrimArithPlus)    = text  "add"
 pretty (PrimArith PrimArithMinus)   = text  "sub"
 pretty (PrimConst (PrimConstInt i)) = text (show i)


