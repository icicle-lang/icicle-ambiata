-- | Primitive functions, constant values and so on
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


-- | Top-level primitive
-- Pretty empty for now.
data Prim
 = PrimArith PrimArith
 | PrimConst PrimConst
 deriving (Eq, Ord, Show)

-- | Arithmetic primitives
data PrimArith
 = PrimArithPlus
 | PrimArithMinus
 deriving (Eq, Ord, Show)

-- | Constant primitives
data PrimConst
 = PrimConstInt Int
 deriving (Eq, Ord, Show)


-- | A primitive always has a well-defined type
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


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimArith PrimArithPlus)    = text  "add"
 pretty (PrimArith PrimArithMinus)   = text  "sub"
 pretty (PrimConst (PrimConstInt i)) = text (show i)


