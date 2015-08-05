-- | Flat primitives - after the folds are removed
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Avalanche.Prim.Flat (
      Prim   (..)
    , PrimProject (..)
    , PrimUnsafe    (..)
    , PrimUpdate    (..)
    , PrimArray     (..)
    , typeOfPrim
    , flatFragment
  ) where

import              Icicle.Internal.Pretty
import              Icicle.Common.Type
import qualified    Icicle.Common.Exp.Prim.Minimal as Min

import qualified    Icicle.Common.Fragment         as Frag

import              P

flatFragment :: Frag.Fragment Prim
flatFragment
 = Frag.Fragment
 { Frag.typeOfPrim           = typeOfPrim
 , Frag.primsFullyApplied    = True
 , Frag.allowLambdas         = Frag.AllowLambdasAsPrimArgs
 }

-- | Primitives for flattened avalanche programs
-- Folds are converted to imperative accessors, loops and so on.
data Prim
 -- | Include a bunch of basic things common across languages
 = PrimMinimal    Min.Prim
 -- | Safe projections
 | PrimProject         PrimProject

 -- | Unsafe projections
 | PrimUnsafe          PrimUnsafe

 -- | Safe updates
 | PrimUpdate          PrimUpdate

 -- | Safe updates
 | PrimArray           PrimArray
 deriving (Eq, Ord, Show)


data PrimProject
 = PrimProjectArrayLength ValType
 | PrimProjectMapLength   ValType ValType
 | PrimProjectMapLookup   ValType ValType
 | PrimProjectOptionIsSome ValType
 deriving (Eq, Ord, Show)


data PrimUnsafe
 = PrimUnsafeArrayIndex ValType
 -- | Create a new, uninitialised array.
 -- This is unsafe because it's uninitialised:
 -- you need to promise me that you'll initialise it before reading from it.
 | PrimUnsafeArrayCreate        ValType
 | PrimUnsafeMapIndex   ValType ValType
 | PrimUnsafeOptionGet  ValType
 deriving (Eq, Ord, Show)


data PrimUpdate
 = PrimUpdateMapPut  ValType ValType
 -- | Should this be unsafe too? It's really both.
 | PrimUpdateArrayPut        ValType
 deriving (Eq, Ord, Show)

data PrimArray
 = PrimArrayZip  ValType ValType
 deriving (Eq, Ord, Show)




-- | A primitive always has a well-defined type
typeOfPrim :: Prim -> Type
typeOfPrim p
 = case p of
    -- All arithmetics are int to int for now
    PrimMinimal m
     -> Min.typeOfPrim m

    PrimProject (PrimProjectArrayLength a)
     -> FunT [funOfVal (ArrayT a)] IntT

    PrimProject (PrimProjectMapLength a b)
     -> FunT [funOfVal (MapT a b)] IntT

    PrimProject (PrimProjectMapLookup a b)
     -> FunT [funOfVal (MapT a b), funOfVal a] (OptionT b)


    PrimProject (PrimProjectOptionIsSome a)
     -> FunT [funOfVal (OptionT a)] BoolT


    PrimUnsafe  (PrimUnsafeArrayIndex a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT] a

    PrimUnsafe  (PrimUnsafeArrayCreate a)
     -> FunT [funOfVal IntT] (ArrayT a)

    PrimUnsafe  (PrimUnsafeMapIndex a b)
     -> FunT [funOfVal (MapT a b), funOfVal IntT] (PairT a b)

    PrimUnsafe  (PrimUnsafeOptionGet a)
     -> FunT [funOfVal (OptionT a)] a

    PrimUpdate  (PrimUpdateMapPut a b)
     -> FunT [funOfVal (MapT a b), funOfVal a, funOfVal b] (MapT a b)

    PrimUpdate  (PrimUpdateArrayPut a)
     -> FunT [funOfVal (ArrayT a), funOfVal IntT, funOfVal a] (ArrayT a)

    PrimArray  (PrimArrayZip a b)
     -> FunT [funOfVal (ArrayT a), funOfVal (ArrayT b)] (ArrayT (PairT a b))


-- Pretty -------------

instance Pretty Prim where
 pretty (PrimMinimal m) = pretty m

 pretty (PrimProject (PrimProjectArrayLength a))
  = text "Array_length#" <+> brackets (pretty a)
 pretty (PrimProject (PrimProjectMapLength a b))
  = text "Map_length#" <+> brackets (pretty a) <+> brackets (pretty b)
 pretty (PrimProject (PrimProjectMapLookup a b))
  = text "Map_lookup#" <+> brackets (pretty a) <+> brackets (pretty b)
 pretty (PrimProject (PrimProjectOptionIsSome a))
  = text "Option_isSome#" <+> brackets (pretty a)


 pretty (PrimUnsafe (PrimUnsafeArrayIndex a))
  = text "unsafe_Array_index#" <+> brackets (pretty a)

 pretty (PrimUnsafe (PrimUnsafeArrayCreate a))
  = text "unsafe_Array_create#" <+> brackets (pretty a)

 pretty (PrimUnsafe (PrimUnsafeMapIndex a b))
  = text "unsafe_Map_index#" <+> brackets (pretty a) <+> brackets (pretty b)

 pretty (PrimUnsafe (PrimUnsafeOptionGet a))
  = text "unsafe_Option_get#" <+> brackets (pretty a)


 pretty (PrimUpdate (PrimUpdateMapPut a b))
  = text "Map_put#" <+> brackets (pretty a) <+> brackets (pretty b)

 pretty (PrimUpdate (PrimUpdateArrayPut a))
  = text "Array_put#" <+> brackets (pretty a)


 pretty (PrimArray (PrimArrayZip a b))
  = text "Array_zip#" <+> brackets (pretty a) <+> brackets (pretty b)

