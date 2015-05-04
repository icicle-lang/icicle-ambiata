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
 -- | Fold and return type
 | PrimFold     PrimFold ValType
 -- | Map primitives
 | PrimMap      PrimMap
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
 = PrimConstInt  Int
 | PrimConstBool Bool
 | PrimConstPair ValType ValType
 | PrimConstSome ValType
 | PrimConstNone ValType
 | PrimConstArrayEmpty ValType
 | PrimConstMapEmpty   ValType ValType
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
    PrimConst (PrimConstPair a b)
     -> FunT [funOfVal a, funOfVal b] (PairT a b)
    PrimConst (PrimConstSome a)
     -> FunT [funOfVal a] (OptionT a)
    PrimConst (PrimConstNone a)
     -> FunT [] (OptionT a)
    PrimConst (PrimConstArrayEmpty a)
     -> FunT [] (ArrayT a)
    PrimConst (PrimConstMapEmpty k v)
     -> FunT [] (MapT k v)

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

 pretty (PrimConst (PrimConstPair a b)) = text "pair#" <+> brackets (pretty a) <+> brackets (pretty b)
 pretty (PrimConst (PrimConstSome t))   = text "some#" <+> brackets (pretty t)
 pretty (PrimConst (PrimConstNone t))   = text "none#" <+> brackets (pretty t)
 pretty (PrimConst (PrimConstArrayEmpty t))
                                        = text "Array_empty#" <+> brackets (pretty t)
 pretty (PrimConst (PrimConstMapEmpty k v))
                                        = text "Map_empty#" <+> brackets (pretty k) <+> brackets (pretty v)

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

