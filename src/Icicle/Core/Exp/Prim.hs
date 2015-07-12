-- | Primitive functions, constant values and so on
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Core.Exp.Prim (
      Prim          (..)
    , PrimFold      (..)
    , PrimArray     (..)
    , PrimMap       (..)
    , PrimTraverse  (..)
    , typeOfPrim
    , extractOption
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
 -- | Array primitives
 | PrimArray    PrimArray
 -- | Map primitives
 | PrimMap      PrimMap
 -- | Converting nested Options to outer Options
 | PrimTraverse PrimTraverse
 deriving (Eq, Ord, Show)


-- | Folds and destructing things
data PrimFold
 = PrimFoldBool
 | PrimFoldArray  ValType
 | PrimFoldOption ValType
 | PrimFoldMap    ValType ValType
 deriving (Eq, Ord, Show)


-- | Array primitives
data PrimArray
 = PrimArrayMap ValType ValType
 deriving (Eq, Ord, Show)


-- | Map primitives
data PrimMap
 = PrimMapInsertOrUpdate ValType ValType
 | PrimMapInsertOrUpdateOption ValType ValType
 | PrimMapMapValues ValType ValType ValType
 deriving (Eq, Ord, Show)


-- | Converting nested Options to outer Options
data PrimTraverse
 = PrimTraverseByType ValType
 deriving (Eq, Ord, Show)

-- | Pull out all Options from a type.
-- If there are no Options, return Nothing.
-- If there are Options, return the type with options filtered out.
-- TODO: we should probably have a separate "Error" type than Option.
extractOption :: ValType -> Maybe ValType
extractOption t
 = case t of
    IntT        -> Nothing
    UnitT       -> Nothing
    BoolT       -> Nothing
    DateTimeT   -> Nothing
    StringT     -> Nothing
    -- TODO: this may be necessary after adding updates
    StructT _   -> Nothing

    ArrayT s
     -> ArrayT <$> extractOption s

    OptionT s
     -> case extractOption s of
         Nothing -> Just s
         Just s' -> Just s'

    MapT k v
     -> case (extractOption k, extractOption v) of
         (Just k', Just v') -> Just (MapT k' v')
         (Just k', Nothing) -> Just (MapT k' v )
         (Nothing, Just v') -> Just (MapT k  v')
         (Nothing, Nothing) -> Nothing

    PairT k v
     -> case (extractOption k, extractOption v) of
         (Just k', Just v') -> Just (PairT k' v')
         (Just k', Nothing) -> Just (PairT k' v )
         (Nothing, Just v') -> Just (PairT k  v')
         (Nothing, Nothing) -> Nothing


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
    PrimFold (PrimFoldArray a) ret
     -> FunT [FunT [funOfVal ret, funOfVal a] ret, funOfVal ret, funOfVal (ArrayT a)] ret
    PrimFold (PrimFoldOption a) ret
     -> FunT [FunT [funOfVal a] ret, funOfVal ret, funOfVal (OptionT a)] ret
    PrimFold (PrimFoldMap k v) ret
     -> FunT [FunT [funOfVal ret, funOfVal k, funOfVal v] ret, funOfVal ret, funOfVal (MapT k v)] ret

    -- Array primitives
    PrimArray (PrimArrayMap a b)
     -> FunT [FunT [funOfVal a] b, funOfVal (ArrayT a)] (ArrayT b)

    -- Map primitives
    PrimMap (PrimMapInsertOrUpdate k v)
     -> FunT [FunT [funOfVal v] v, funOfVal v, funOfVal k, funOfVal (MapT k v)] (MapT k v)

    PrimMap (PrimMapInsertOrUpdateOption k v)
     -> FunT [FunT [funOfVal v] (OptionT v), funOfVal v, funOfVal k, funOfVal (MapT k v)] (OptionT (MapT k v))

    PrimMap (PrimMapMapValues k v v')
     -> FunT [FunT [funOfVal v] v', funOfVal (MapT k v)] (MapT k v')

    PrimTraverse (PrimTraverseByType t)
     -> FunT [funOfVal t] $ OptionT $ maybe t id $ extractOption t



-- Pretty -------------

instance Pretty Prim where
 pretty (PrimMinimal m) = pretty m

 pretty (PrimFold f ret)
  = let f' = case f of
              PrimFoldBool
                -> text "if#"
              PrimFoldArray a
               -> text "Array_fold#" <+> brackets (pretty a)
              PrimFoldOption a
               -> text "Option_fold#" <+> brackets (pretty a)
              PrimFoldMap k v
               -> text "Map_fold#" <+> brackets (pretty k) <+> brackets (pretty v)
    in f' <+> brackets (pretty ret)

 pretty (PrimArray (PrimArrayMap a b))
  = text "Array_map#" <+> brackets (pretty a) <+> brackets (pretty b)

 pretty (PrimMap (PrimMapInsertOrUpdate k v))
  = text "Map_insertOrUpdate#" <+> brackets (pretty k) <+> brackets (pretty v)

 pretty (PrimMap (PrimMapInsertOrUpdateOption k v))
  = text "Map_insertOrUpdateOption#" <+> brackets (pretty k) <+> brackets (pretty v)

 pretty (PrimMap (PrimMapMapValues k v v'))
  = text "Map_mapValues#" <+> brackets (pretty k) <+> brackets (pretty v) <+> brackets (pretty v')

 pretty (PrimTraverse (PrimTraverseByType t))
  = text "traverse#" <+> brackets (pretty t)

