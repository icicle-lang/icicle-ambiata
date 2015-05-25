{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Exp.Prim.Minimal (
      Prim   (..)
    , PrimArith(..)
    , PrimRelation(..)
    , PrimLogical(..)
    , PrimConst(..)
    , PrimDateTime (..)
    , typeOfPrim
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type

import              P


-- | Top-level primitive
-- Pretty empty for now.
data Prim
 = PrimArith    PrimArith
 -- | Relation prims like less than, equal etc work for a bunch of different types
 | PrimRelation PrimRelation ValType
 | PrimLogical  PrimLogical
 | PrimConst    PrimConst
 -- | Date primitives
 | PrimDateTime PrimDateTime
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

-- | Constant primitives and constructors
data PrimConst
 = PrimConstPair ValType ValType
 | PrimConstSome ValType
 deriving (Eq, Ord, Show)

-- | DateTime primitives
data PrimDateTime
 = PrimDateTimeDaysDifference
 deriving (Eq, Ord, Show)



-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    -- All arithmetics are int to int for now
    PrimArith _
     -> FunT [intT, intT] IntT

    -- All relations are binary to bool
    PrimRelation _ val
     -> FunT [funOfVal val, funOfVal val] BoolT

    -- Logical relations
    PrimLogical PrimLogicalNot
     -> FunT [funOfVal BoolT] BoolT
    PrimLogical PrimLogicalAnd
     -> FunT [funOfVal BoolT, funOfVal BoolT] BoolT
    PrimLogical PrimLogicalOr
     -> FunT [funOfVal BoolT, funOfVal BoolT] BoolT

    -- Constants
    PrimConst (PrimConstPair a b)
     -> FunT [funOfVal a, funOfVal b] (PairT a b)
    PrimConst (PrimConstSome a)
     -> FunT [funOfVal a] (OptionT a)

    PrimDateTime PrimDateTimeDaysDifference
     -> FunT [funOfVal DateTimeT, funOfVal DateTimeT] IntT
 where
  intT = FunT [] IntT


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimArith PrimArithPlus)       = text  "add#"
 pretty (PrimArith PrimArithMinus)      = text  "sub#"
 pretty (PrimArith PrimArithDiv)        = text  "div#"

 pretty (PrimRelation rel t)
  = text prel <+> brackets (pretty t)
  where
   prel
    = case rel of
       PrimRelationGt -> "gt#"
       PrimRelationGe -> "ge#"
       PrimRelationLt -> "lt#"
       PrimRelationLe -> "le#"
       PrimRelationEq -> "eq#"
       PrimRelationNe -> "ne#"

 pretty (PrimLogical  PrimLogicalNot)   = text  "not#"
 pretty (PrimLogical  PrimLogicalAnd)   = text  "and#"
 pretty (PrimLogical  PrimLogicalOr)    = text  "or#"

 pretty (PrimConst (PrimConstPair a b)) = text "pair#" <+> brackets (pretty a) <+> brackets (pretty b)
 pretty (PrimConst (PrimConstSome t))   = text "some#" <+> brackets (pretty t)

 pretty (PrimDateTime PrimDateTimeDaysDifference)
  = text "DateTime_daysDifference#"



