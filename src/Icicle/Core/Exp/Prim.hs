-- | Primitive functions, constant values and so on
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Prim (
      Prim   (..)
    , PrimFold (..)
    , PrimMap  (..)
    , typeOfPrim
    ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              P


-- | Top-level primitive for Core expressions
-- Includes folds etc that won't be present in Avalanche
data Prim
 -- | Include a bunch of basic things common across languages
 = PrimMinimal    Min.Prim
 -- | Fold and return type
 | PrimFold     PrimFold ValType
 -- | Map primitives
 | PrimMap      PrimMap
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
    PrimMinimal m
     -> Min.typeOfPrim m

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


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimMinimal m) = pretty m

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

