{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Test.Gen.Core.Type where

import           Icicle.Common.Type

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import P

import           Disorder.Corpus
import qualified Data.Map   as Map

genDerivedTypeTop :: ValType -> Gen ValType
genDerivedTypeTop t
 = Gen.recursive Gen.choice
 [ genDerivedType t ]
 -- Wrap in choice to make 50/50 chance of raw genDerivedType
 -- (instead of 1/6)
 [ Gen.choice
 [ PairT <$> genDerivedTypeTop t <*> genDerivedTypeTop t
 , OptionT <$> genDerivedTypeTop t
 , SumT <$> genDerivedTypeTop t <*> genDerivedTypeTop t
 , ArrayT <$> genDerivedTypeTop t
 , MapT <$> genOrdValType <*> genDerivedTypeTop t
 , BufT <$> genBufLength <*> genDerivedTypeTop t
 ] ]

genDerivedType :: ValType -> Gen ValType
genDerivedType t = case t of
 IntT       -> usually
 UnitT      -> usually
 BoolT      -> usually
 DoubleT    -> usually
 TimeT      -> usually
 StringT    -> usually
 ErrorT     -> usually
 PairT a b  -> Gen.choice [genDerivedType a, genDerivedType b, usually]
 SumT a b   -> Gen.choice [genDerivedType a, genDerivedType b, usually]
 OptionT a  -> Gen.choice [genDerivedType a, usually]
 StructT (StructType fs)
            -> Gen.choice ((genDerivedType <$> Map.elems fs) <> [usually])
 ArrayT t'  -> Gen.choice [genDerivedType t', usually]
 MapT k v   -> Gen.choice [genDerivedType k, genDerivedType v, usually]
 BufT _ t'  -> Gen.choice [genDerivedType t', usually]

 where
  usually = Gen.choice [genPrimType, return t]

genBufLength :: Gen Int
genBufLength = Gen.integral (Range.linear 1 5)

--------------------------------------------------------------------------------

genArithType :: Gen ArithType
genArithType = Gen.element
  [ ArithIntT
  , ArithDoubleT ]

genValType :: Gen ValType
genValType = Gen.recursive Gen.choice
    genPrimTypes
  [ PairT   <$> genValType <*> genValType
  , OptionT <$> genValType
  , SumT    <$> genValType <*> genValType
  , ArrayT  <$> genValType
  , BufT    <$> genBufLength <*> genValType
  , MapT    <$> genOrdValType <*> genValType
  , StructT <$> genStructType
  ]

genStructType' :: Gen ValType -> Gen StructType
genStructType' genT
 = StructType . Map.fromList <$> Gen.list (Range.linear 1 5) genField
 where
   genField = (,) <$> genStructField <*> genT

-- Structs have at most five fields, to prevent them from being too large.
-- Field types are much more likely to be "primitives", so that they are not too deep.
genStructType :: Gen StructType
genStructType = genStructType' (Gen.recursive Gen.choice [genPrimType] [genValType])

genStructField :: Gen StructField
genStructField = StructField <$> Gen.element colours

-- Generate an "Ord-able" ValType.
-- Some of the Ord instances for values are different to the flattened/melted instances:
-- particularly arrays and maps of non-primitives.
--
-- [1, 2] > [3]
-- Core: True
-- Melted: False
--
-- This case could be fixed easily enough, but it gets more complicated with flattened array of pairs vs pair of arrays:
--
-- [(1, 1), (2, 2)] < [(1,2), (0,0)]
-- = True
-- ([1, 2], [1, 2]) < ([1, 0], [2, 0])
-- = False
--
-- The lexicographic comparison on the flattened pairs compares the entire array of firsts, then the array of seconds,
-- whereas the unflattened one compares the firsts and seconds interspersed.
--
-- It is possible to fix this, but since the pair of arrays representation is itself a valid ordering, there is not much point.
--
genOrdValType :: Gen ValType
genOrdValType = Gen.recursive Gen.choice
  [ return UnitT
  , return BoolT
  , return IntT
  , return TimeT
  , return StringT ]
  [ PairT   <$> genOrdValType <*> genOrdValType
  , OptionT <$> genOrdValType
  , SumT    <$> genOrdValType <*> genOrdValType
  , StructT <$> genStructType' genOrdValType
  ]

-- Try to generate an Ord type based on an arbitrary type.
-- If the type contains an array or map, we will just replace it with the element type:
--
-- > Int                ==> Int
-- > Array a            ==> a
-- > (Array a, Map k v) ==> (a, k)
-- > (Array a, Map k v) ==> (a, v)
--
genOrdValTypeOf :: ValType -> Gen ValType
genOrdValTypeOf t = case t of
  _ | isOrdValType t -> return t
  PairT a b
   -> PairT <$> genOrdValTypeOf a <*> genOrdValTypeOf b
  SumT a b
   -> SumT <$> genOrdValTypeOf a <*> genOrdValTypeOf b
  OptionT a
   -> OptionT <$> genOrdValTypeOf a
  StructT (StructType fs)
   -> Gen.choice (genOrdValType : fmap genOrdValTypeOf (Map.elems fs))
  ArrayT a
   -> genOrdValTypeOf a
  MapT k v
   -> Gen.choice [genOrdValTypeOf k, genOrdValTypeOf v]
  BufT _ a
   -> genOrdValTypeOf a
  -- Generate a primitive if we can't salvage any of it.
  -- Eg. if the input type is Double
  _
   -> genOrdValType

genOrdValTypeOf' :: Gen ValType -> Gen ValType
genOrdValTypeOf' genT = genT >>= genOrdValTypeOf

isOrdValType :: ValType -> Bool
isOrdValType t = case t of
 IntT       -> True
 UnitT      -> True
 BoolT      -> True
 TimeT      -> True
 StringT    -> True
 ErrorT     -> True
 -- Double is not "Ord" because NaN cannot be used as the key of a Map.
 DoubleT    -> False
 PairT a b  -> isOrdValType a && isOrdValType b
 SumT a b   -> isOrdValType a && isOrdValType b
 OptionT a  -> isOrdValType a
 StructT (StructType fs)
            -> all isOrdValType fs
 ArrayT{}   -> False
 MapT{}     -> False
 BufT{}     -> False


genPrimType :: Gen ValType
genPrimType = Gen.choice genPrimTypes

genPrimTypes :: [Gen ValType]
genPrimTypes = 
  [ return UnitT
  , return BoolT
  , return IntT
  , return DoubleT
  , return TimeT
  , return StringT ]

genInputType :: Gen ValType
genInputType = Gen.recursive Gen.choice
    genPrimTypes
  [ PairT   <$> genInputType <*> genInputType
  , OptionT <$> genInputType
  , SumT    <$> genInputType <*> genInputType
  , StructT <$> genStructType' genInputType
  , ArrayT  <$> genInputType
  , MapT    <$> genPrimType  <*> genInputType
  ]

genOutputType :: Gen ValType
genOutputType = Gen.recursive Gen.choice
  -- FIXME
  -- This is the subset of output types unsupported by PSV.
  -- Change this to normal type generator when we switch to Zebra.
  [ return BoolT
  , return IntT
  , return DoubleT
  , return TimeT
  , return StringT ]
  [ OptionT <$> genOutputType
  , SumT ErrorT <$> genOutputType
  , PairT   <$> genOutputType <*> genOutputType
  , ArrayT  <$> genOutputType
  , MapT    <$> genOrdValType <*> genOutputType
  , StructT <$> genStructType' genOutputType
  ]

