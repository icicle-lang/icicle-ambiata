-- | Flat primitives - after the folds are removed
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Prim.Flat (
      Prim   (..)
    , PrimProject (..)
    , PrimUnsafe    (..)
    , typeOfPrim
  ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import              P
import              Prelude (String)


-- | Primitives for flattened avalanche programs
-- Folds are converted to imperative accessors, loops and so on.
data Prim
 -- | Include a bunch of basic things common across languages
 = PrimMinimal    Min.Prim
 -- | Safe projections
 | PrimProject         PrimProject

 -- | Unsafe projections
 | PrimUnsafe          PrimUnsafe

 | PrimTODO String
 deriving (Eq, Ord, Show)


data PrimProject
 = PrimProjectPair Bool  ValType ValType
 | PrimProjectArrayLength ValType
 | PrimProjectMapLength   ValType ValType
 | PrimProjectOptionIsSome ValType
 deriving (Eq, Ord, Show)


data PrimUnsafe
 = PrimUnsafeArrayIndex ValType
 | PrimUnsafeMapIndex   ValType ValType
 | PrimUnsafeOptionGet  ValType
 deriving (Eq, Ord, Show)




-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    -- All arithmetics are int to int for now
    PrimMinimal m
     -> Min.typeOfPrim m

    PrimProject (PrimProjectPair False a b)
     -> FunT [funOfVal (PairT a b)] a

    PrimProject (PrimProjectPair True a b)
     -> FunT [funOfVal (PairT a b)] b

    PrimProject (PrimProjectArrayLength a)
     -> FunT [funOfVal (ArrayT a)] IntT

    PrimProject (PrimProjectMapLength a b)
     -> FunT [funOfVal (MapT a b)] IntT

    PrimProject (PrimProjectOptionIsSome a)
     -> FunT [funOfVal (OptionT a)] BoolT


    PrimUnsafe  (PrimUnsafeArrayIndex a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT] a

    PrimUnsafe  (PrimUnsafeMapIndex a b)
     -> FunT [funOfVal (MapT a b), funOfVal IntT] (PairT a b)

    PrimUnsafe  (PrimUnsafeOptionGet a)
     -> FunT [funOfVal (OptionT a)] a

    PrimTODO _
     -> FunT [] IntT


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimMinimal m) = pretty m

 pretty (PrimProject (PrimProjectPair False a b))
  = text "fst#" <+> brackets (pretty a) <+> brackets (pretty b)
 pretty (PrimProject (PrimProjectPair True a b))
  = text "snd#" <+> brackets (pretty a) <+> brackets (pretty b)

 pretty (PrimProject (PrimProjectArrayLength a))
  = text "Array_length#" <+> brackets (pretty a)
 pretty (PrimProject (PrimProjectMapLength a b))
  = text "Map_length#" <+> brackets (pretty a) <+> brackets (pretty b)
 pretty (PrimProject (PrimProjectOptionIsSome a))
  = text "Option_isSome#" <+> brackets (pretty a)


 pretty (PrimUnsafe (PrimUnsafeArrayIndex a))
  = text "unsafe_Array_index#" <+> brackets (pretty a)

 pretty (PrimUnsafe (PrimUnsafeMapIndex a b))
  = text "unsafe_Map_index#" <+> brackets (pretty a) <+> brackets (pretty b)

 pretty (PrimUnsafe (PrimUnsafeOptionGet a))
  = text "unsafe_Option_get#" <+> brackets (pretty a)


 pretty (PrimTODO str)
  = text "todo#" <+> text str

