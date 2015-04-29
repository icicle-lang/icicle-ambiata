-- | Primitive functions, constant values and so on
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Prim (
      Prim   (..)
    , PrimArith(..)
    , PrimRelation(..)
    , PrimLogical(..)
    , PrimConst(..)
    , typeOfPrim
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Core.Type

import              P


-- | Top-level primitive
-- Pretty empty for now.
data Prim
 = PrimArith    PrimArith
 | PrimRelation PrimRelation
 | PrimLogical  PrimLogical
 | PrimConst    PrimConst
 deriving (Eq, Ord, Show)

-- | Arithmetic primitives
data PrimArith
 = PrimArithPlus
 | PrimArithMinus
 | PrimArithDiv
 deriving (Eq, Ord, Show)

-- | Predicates like >=
data PrimRelation
 = PrimRelationGt
 | PrimRelationGe
 | PrimRelationLt
 | PrimRelationLe
 | PrimRelationEq
 | PrimRelationNe
 deriving (Eq, Ord, Show)

-- | Logical relations like &&, not
data PrimLogical
 = PrimLogicalNot
 | PrimLogicalAnd
 | PrimLogicalOr
 deriving (Eq, Ord, Show)

-- | Constant primitives
data PrimConst
 = PrimConstInt  Int
 | PrimConstBool Bool
 deriving (Eq, Ord, Show)


-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    -- All arithmetics are int to int for now
    PrimArith _
     -> FunT [intT, intT] IntT

    -- All relations are ints
    PrimRelation _
     -> FunT [intT, intT] BoolT

    -- Logical relations
    PrimLogical PrimLogicalNot
     -> FunT [funOfVal BoolT] BoolT
    PrimLogical PrimLogicalAnd
     -> FunT [funOfVal BoolT, funOfVal BoolT] BoolT
    PrimLogical PrimLogicalOr
     -> FunT [funOfVal BoolT, funOfVal BoolT] BoolT

    -- Constants
    PrimConst (PrimConstInt _)
     -> intT
    PrimConst (PrimConstBool _)
     -> FunT [] BoolT
 where
  intT = FunT [] IntT


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimArith PrimArithPlus)       = text  "add#"
 pretty (PrimArith PrimArithMinus)      = text  "sub#"
 pretty (PrimArith PrimArithDiv)        = text  "div#"

 pretty (PrimRelation PrimRelationGt)   = text  "gt#"
 pretty (PrimRelation PrimRelationGe)   = text  "ge#"
 pretty (PrimRelation PrimRelationLt)   = text  "lt#"
 pretty (PrimRelation PrimRelationLe)   = text  "le#"
 pretty (PrimRelation PrimRelationEq)   = text  "eq#"
 pretty (PrimRelation PrimRelationNe)   = text  "ne#"

 pretty (PrimLogical  PrimLogicalNot)   = text  "not#"
 pretty (PrimLogical  PrimLogicalAnd)   = text  "and#"
 pretty (PrimLogical  PrimLogicalOr)    = text  "or#"

 pretty (PrimConst (PrimConstInt i))    = text (show i)
 pretty (PrimConst (PrimConstBool b))   = text (show b)


