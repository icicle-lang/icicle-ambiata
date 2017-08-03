{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Test.Gen.Core.Prim where

import           Icicle.Common.Base
import           Icicle.Common.Type

import           Icicle.Core.Exp.Prim
import qualified Icicle.Common.Exp.Prim.Minimal as PM

import Icicle.Test.Gen.Core.Type

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import P
import qualified Data.Map   as Map
import qualified Data.Set   as Set

genPrimLookupList :: Gen ValType -> Gen (Map.Map ValType [Prim])
genPrimLookupList genT = Gen.prune $ fmap Set.toList <$> genPrimLookup genT

genPrimLookup :: Gen ValType -> Gen (Map.Map ValType (Set.Set Prim))
genPrimLookup genT = go Map.empty (10 :: Int)
 where
  go m 0 = return m
  go m n = do
   is <- genPrimMany genT
   let m' = foldl insert m is
   go m' (n - 1)

  insert m p =
   let (FunT _ ret) = typeOfPrim p
   in Map.insertWith Set.union ret (Set.singleton p) m


genPrimMany :: Gen ValType -> Gen [Prim]
genPrimMany genT = do
  m <- mins
  r <- rest
  l <- latest
  return (m <> r <> l)
 where
  mins = fmap PrimMinimal <$> genPrimMinimalMany genT
  rest = sequence
    [ PrimFold       PrimFoldBool   <$> genT
    , PrimFold  <$> (PrimFoldArray  <$> genT) <*> genT
    , PrimFold  <$> (PrimFoldOption <$> genT) <*> genT
    , PrimFold  <$> (PrimFoldSum    <$> genT  <*> genT) <*> genT
    , PrimFold  <$> (PrimFoldMap    <$> genT  <*> genT) <*> genT
    , PrimMap   <$> (PrimMapInsertOrUpdate    <$> genT  <*> genT)
    , PrimMap   <$> (PrimMapMapValues         <$> genT  <*> genT <*> genT)
    , PrimMap   <$> (PrimMapDelete            <$> genT  <*> genT)
    , PrimMap   <$> (PrimMapLookup            <$> genT  <*> genT)
    , PrimArray <$> (PrimArrayMap   <$> genT  <*> genT)
    -- TODO: missing PrimWindow; investigate conversion of windows to Avalanche and reinstate
    -- , PrimWindow <$> genWindowUnit <*> Gen.maybe genWindowUnit
    ]

  -- Generate buffer prims in pairs so for a given size and type we can always push and read
  latest = do
   n <- genBufLength
   a <- genT
   return [ PrimLatest $ PrimLatestPush n a
          , PrimLatest $ PrimLatestRead n a ]


genWindowUnit :: Gen WindowUnit
genWindowUnit = Gen.choice
  [ Days <$> pos
  , Months <$> pos
  , Weeks <$> pos ]
 where
  pos = Gen.integral $ Range.linear 0 10

genPrimMinimalMany :: Gen ValType -> Gen [PM.Prim]
genPrimMinimalMany genT
 = concatGens [pure arith, pure logical, relation <$> genT, struct <$> genT, pure consts, pure builtins]
 where
  concatGens :: [Gen [Gen PM.Prim]] -> Gen [PM.Prim]
  concatGens gs = join (sequence <$> (concat <$> (sequence gs)))

  arith
   = [ PM.PrimArithUnary  <$> genPrimArithUnary <*> genArithType
     , PM.PrimArithBinary <$> genPrimArithBinary <*> genArithType ]

  logical
   = [ PM.PrimLogical     <$> genPrimLogical
     , PM.PrimTime        <$> genPrimTime ]

  consts
   = [ PM.PrimConst <$> (PM.PrimConstPair  <$> genT <*> genT)
     , PM.PrimConst  .   PM.PrimConstSome  <$> genT
     , PM.PrimConst <$> (PM.PrimConstLeft  <$> genT <*> genT)
     , PM.PrimConst <$> (PM.PrimConstRight <$> genT <*> genT)
     , PM.PrimPair  <$> (PM.PrimPairFst    <$> genT <*> genT)
     , PM.PrimPair  <$> (PM.PrimPairSnd    <$> genT <*> genT) ]

  builtins
   = [ PM.PrimBuiltinFun <$> genPrimBuiltinFun genT ]

  relation t
   | isOrdValType t
   = [ PM.PrimRelation <$> genPrimRelation <*> pure t ]
   | otherwise
   = [ return $ PM.PrimRelation PM.PrimRelationEq t
     , return $ PM.PrimRelation PM.PrimRelationNe t ]

  struct t
   | StructT st@(StructType fs) <- t
   = fmap (\(fn,ft) -> return $ PM.PrimStruct $ PM.PrimStructGet fn ft st)
   $ Map.toList fs
   | otherwise
   = []


--------------------------------------------------------------------------------

-- We restrict the space of some arbitrary primitives, so we never get invalid
-- applications such as applying pow to negative exponents.

genPrimMinimal :: Gen PM.Prim
genPrimMinimal
 = Gen.choice
 [ PM.PrimArithUnary  <$> genPrimArithUnary <*> genArithType
 , PM.PrimArithBinary <$> genPrimArithBinary <*> genArithType
 , PM.PrimLogical     <$> genPrimLogical
 , PM.PrimTime        <$> genPrimTime
 , PM.PrimConst <$> (PM.PrimConstPair  <$> genValType <*> genValType)
 , PM.PrimConst  .   PM.PrimConstSome  <$> genValType
 , PM.PrimConst <$> (PM.PrimConstLeft  <$> genValType <*> genValType)
 , PM.PrimConst <$> (PM.PrimConstRight <$> genValType <*> genValType)
 , PM.PrimPair  <$> (PM.PrimPairFst    <$> genValType <*> genValType)
 , PM.PrimPair  <$> (PM.PrimPairSnd    <$> genValType <*> genValType)
 , PM.PrimBuiltinFun <$> genPrimBuiltinFun genValType
 , PM.PrimRelation <$> genPrimRelation <*> genOrdValType
 , PM.PrimRelation PM.PrimRelationEq <$> genValType
 , PM.PrimRelation PM.PrimRelationNe <$> genValType
 , genStructType >>= struct
 ]
 where
  struct st@(StructType fs) = do
   (fn,ft) <- Gen.element $ Map.toList fs
   return $ PM.PrimStruct $ PM.PrimStructGet fn ft st


genPrimArithUnary :: Gen PM.PrimArithUnary
genPrimArithUnary = Gen.enumBounded

genPrimArithBinary :: Gen PM.PrimArithBinary
genPrimArithBinary = Gen.element
  [ PM.PrimArithPlus
  , PM.PrimArithMinus
  , PM.PrimArithMul ]

genPrimRelation :: Gen PM.PrimRelation
genPrimRelation = Gen.enumBounded

genPrimLogical :: Gen PM.PrimLogical
genPrimLogical = Gen.enumBounded

-- TODO: add PrimTimeMinusDays and PrimTimeMinusMonths after choosing a consistent time representation
-- We have problems because the C and the Core use different representations, so they overflow or underflow at different points.
-- I think if we only allow minus seconds it will be less likely, because you need a lot of seconds to overflow.
-- You don't need many months to overflow.
genPrimTime :: Gen PM.PrimTime
genPrimTime = Gen.element
 [ PM.PrimTimeDaysDifference
 , PM.PrimTimeSecondsDifference
 , PM.PrimTimeDaysJulianEpoch
 , PM.PrimTimeSecondsJulianEpoch
 , PM.PrimTimeMinusSeconds
 , PM.PrimTimeProjectDay
 , PM.PrimTimeProjectMonth
 , PM.PrimTimeProjectYear ]

genPrimBuiltinMath :: Gen PM.PrimBuiltinMath
genPrimBuiltinMath = Gen.element
  [ PM.PrimBuiltinCeiling
  , PM.PrimBuiltinFloor
  , PM.PrimBuiltinTruncate
  , PM.PrimBuiltinRound
  , PM.PrimBuiltinToDoubleFromInt
  -- TODO: missing NaN-introducing primitives; modify tests to use NanEq instead of Eq.
  -- , PM.PrimBuiltinPow
  -- , PM.PrimBuiltinDiv
  -- , PM.PrimBuiltinLog
  -- , PM.PrimBuiltinExp
  -- , PM.PrimBuiltinSqrt
  , PM.PrimBuiltinDoubleIsValid ]

genPrimBuiltinMap :: Gen ValType -> Gen PM.PrimBuiltinMap
genPrimBuiltinMap genT = Gen.choice
    [ PM.PrimBuiltinKeys <$> genT <*> genT
    , PM.PrimBuiltinVals <$> genT <*> genT ]

-- TODO: missing PrimBuiltinIndex; modify index to be safe.
-- Returning an Option would probably be fine for Core, if we had an explicitly unsafe primitive in Flat.
genPrimBuiltinArray :: Gen ValType -> Gen PM.PrimBuiltinArray
genPrimBuiltinArray genT = Gen.choice
  [ PM.PrimBuiltinSort <$> genT
  , PM.PrimBuiltinLength <$> genT ]

genPrimBuiltinFun :: Gen ValType -> Gen PM.PrimBuiltinFun
genPrimBuiltinFun genT = Gen.choice
   [ PM.PrimBuiltinMath  <$> genPrimBuiltinMath
   , PM.PrimBuiltinMap   <$> genPrimBuiltinMap   genT
   , PM.PrimBuiltinArray <$> genPrimBuiltinArray genT]

