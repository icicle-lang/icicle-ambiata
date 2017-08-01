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
 , MapT <$> genDerivedTypeTop t <*> genDerivedTypeTop t
 -- , BufT <$> Gen.integral (Range.linear 1 10) <*> genDerivedTypeTop t
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
 FactIdentifierT
            -> usually
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
  -- , BufT    <$> (Gen.integral $ Range.linear 1 20) <*> genValType
  , MapT    <$> genValType <*> genValType
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
    genPrimTypes
  [ PairT   <$> genOrdValType <*> genOrdValType
  , OptionT <$> genOrdValType
  , SumT    <$> genOrdValType <*> genOrdValType
  , StructT <$> genStructType' genOrdValType
  ]

isOrdValType :: ValType -> Bool
isOrdValType t = case t of
 IntT       -> True
 UnitT      -> True
 BoolT      -> True
 DoubleT    -> True
 TimeT      -> True
 StringT    -> True
 ErrorT     -> True
 FactIdentifierT
            -> True
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
  , MapT    <$> genInputType <*> genInputType
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
  , PairT <$> genOutputType <*> genOutputType
  , ArrayT <$> genOutputType
  , MapT <$> genOutputType <*> genOutputType
  , StructT <$> genStructType' genOutputType
  ]

