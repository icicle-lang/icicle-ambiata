-- | Primitive functions, constant values and so on
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Prim (
      Prim   (..)
    , PrimArith(..)
    , PrimRelation(..)
    , PrimLogical(..)
    , PrimConst(..)
    , PrimFold (..)
    , PrimMap  (..)
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
 -- | Fold and return type
 | PrimFold     PrimFold ValType
 -- | Map primitives
 | PrimMap      PrimMap
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

-- | Folds and destructing things
data PrimFold
 = PrimFoldBool
 | PrimFoldPair   ValType ValType
 | PrimFoldArray  ValType
 | PrimFoldOption ValType
 | PrimFoldMap    ValType ValType
 deriving (Eq, Ord, Show)

-- | Map primitives
data PrimMap
 = PrimMapInsertOrUpdate ValType ValType
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

    -- Folds
    PrimFold PrimFoldBool ret
     -> FunT [funOfVal ret, funOfVal ret, funOfVal BoolT] ret
    PrimFold (PrimFoldPair a b) ret
     -> FunT [FunT [funOfVal a, funOfVal b] ret, funOfVal (PairT a b)] ret
    PrimFold (PrimFoldArray a) ret
     -> FunT [FunT [funOfVal ret, funOfVal a] ret, funOfVal ret, funOfVal (ArrayT a)] ret
    PrimFold (PrimFoldOption a) ret
     -> FunT [FunT [funOfVal a] ret, funOfVal ret, funOfVal (OptionT a)] ret
    PrimFold (PrimFoldMap k v) ret
     -> FunT [FunT [funOfVal ret, funOfVal k, funOfVal v] ret, funOfVal ret, funOfVal (MapT k v)] ret

    PrimMap (PrimMapInsertOrUpdate k v)
     -> FunT [FunT [funOfVal v] v, funOfVal v, funOfVal k, funOfVal (MapT k v)] (MapT k v)

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

 pretty (PrimFold f ret)
  = let f' = case f of
              PrimFoldBool
                -> text "if#"
              PrimFoldPair a b
                -> text "unpair#" <+> brackets (pretty a) <+> brackets (pretty b)
              PrimFoldArray a
               -> text "Array_fold#" <+> brackets (pretty a)
              PrimFoldOption a
               -> text "Option_fold#" <+> brackets (pretty a)
              PrimFoldMap k v
               -> text "Map_fold#" <+> brackets (pretty k) <+> brackets (pretty v)
    in f' <+> brackets (pretty ret)

 pretty (PrimMap (PrimMapInsertOrUpdate k v))
  = text "Map_insertOrUpdate#" <+> brackets (pretty k) <+> brackets (pretty v)

 pretty (PrimDateTime PrimDateTimeDaysDifference)
  = text "DateTime_daysDifference#"

