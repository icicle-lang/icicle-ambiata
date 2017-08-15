{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Test.Gen.Core.Value where

import           Icicle.Data hiding (Value(..), StructField(..))
import           Icicle.Data.Time

import           Icicle.Common.Base
import           Icicle.Common.Eval
import           Icicle.Common.Type
import           Disorder.Corpus
import           Icicle.Test.Arbitrary.Data ()


import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen.QuickCheck as Qc

import P
import qualified Data.Map   as Map


-- | Generate a value for given value type
baseValueForType :: MonadGen m => ValType -> m BaseValue
baseValueForType t
 = case t of
    IntT
     -> VInt <$> Gen.integral (Range.linear (-100) 100)
    DoubleT
     -> VDouble <$> Gen.double (Range.linearFrac (-100) 100)
    UnitT
     -> return VUnit
    ErrorT
     -> VError <$> Gen.enumBounded
    BoolT
     -> VBool <$> Gen.bool
    TimeT
     -> VTime <$> Qc.arbitrary
    ArrayT t'
     -> Gen.small (VArray <$> Gen.list r10 (baseValueForType t'))
    BufT n t'
     -> Gen.small (VBuf <$> Gen.list (Range.singleton n) (baseValueForType t'))
    PairT a b
     -> VPair <$> baseValueForType a <*> baseValueForType b
    SumT a b
     -> Gen.choice [ VLeft  <$> baseValueForType a
                   , VRight <$> baseValueForType b ]
    OptionT t'
     -> Gen.recursive Gen.choice
                    [ return VNone ]
                    [ VSome <$> baseValueForType t' ]
    MapT k v
     -> Gen.small
       (VMap . Map.fromList
     <$> Gen.list r10 ((,) <$> baseValueForType k <*> baseValueForType v))

    StringT
     -> VString <$> Gen.element simpsons
    StructT (StructType fs)
     -> Gen.small
      (VStruct <$> traverse baseValueForType fs)

r10 :: Integral a => Range a
r10 = Range.linear 0 10

r100 :: Integral a => Range a
r100 = Range.linear 0 100

inputsForType :: MonadGen m => ValType -> m ([AsAt BaseValue], EvalContext)
inputsForType t = do
  start         <- Gen.integral r100
  values        <- Gen.list r100 ((,) <$> daysIncrement <*> baseValueForType t)
  maxMap        <- Gen.integral r100
  let (facts, time) = go start values
  return (facts, EvalContext time maxMap)
 where
  daysIncrement = Gen.integral r100

  go days0 [] = ([], timeOfDays days0)
  go days0 ((days,v):rs) =
   let days' = days0 + days
       time' = timeOfDays days'
       (rs',last') = go days' rs
   in  (AsAt v time' : rs', last')

