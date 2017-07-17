{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary.Core where

import           Icicle.BubbleGum
import           Icicle.Data hiding (Value(..), StructField(..))

import           Icicle.Common.Base
import           Icicle.Common.Eval
import           Icicle.Common.Exp
import           Icicle.Common.Type
import           Icicle.Common.Value
import qualified Icicle.Common.Fresh                as Fresh

import qualified Icicle.Core.Exp                as X
import           Icicle.Core.Exp.Prim
import           Icicle.Core.Program.Program    as P

import           Icicle.Test.Arbitrary.Data

import qualified Test.QuickCheck.Hedgehog as Qc
import qualified Icicle.Test.Gen.Core.Value as CoreGen
import qualified Icicle.Test.Gen.Core.Program as CoreGen
import qualified Icicle.Test.Gen.Core.Type as CoreGen

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P


testFreshT :: Functor m => Text -> Fresh.FreshT Var m a -> m a
testFreshT desc prog
 = fmap snd
 $ Fresh.runFreshT prog
 $ Fresh.counterNameState (NameBase . Var desc) 0

testFresh :: Text -> Fresh.Fresh Var a -> a
testFresh desc prog
 = snd
 $ Fresh.runFresh prog
 $ Fresh.counterNameState (NameBase . Var desc) 0

-- | Check if values are equal except for functions/closures
-- Because closure heaps can differ..
equalExceptFunctions :: (Eq a, Eq n, Eq p) => Value a n p -> Value a n p -> Bool
equalExceptFunctions p q
 | VFun{} <- p
 , VFun{} <- q
 = True
 | otherwise
 = p == q

equalExceptFunctionsE :: (Eq a, Eq n, Eq p, Eq l)
                      => Either l (Value a n p)
                      -> Either l (Value a n p)
                      -> Bool
equalExceptFunctionsE p q
 | Right p' <- p
 , Right q' <- q
 = p' `equalExceptFunctions` q'
 | otherwise
 = p == q


-- | Generate a well typed expression.
-- If we can't generate a well typed expression we want quickcheck to count it as
-- failing to satisfy a precondition.
-- withTypedExp :: Testable prop => (Exp () Var Prim -> ValType -> prop) -> Property
withTypedExp :: Testable prop => (Exp () Var Prim -> ValType -> prop) -> Property
withTypedExp prop
 = forAll genExp
 $ \(x, t)
 -> typeExp0 X.coreFragment x == Right (FunT [] t) ==> prop x t

genExp :: Gen (Exp () Var Prim, ValType)
genExp = do
 Qc.hedgehog $ CoreGen.genExpTop

genExpNoType :: Gen (Exp () Var Prim)
genExpNoType = fst <$> genExp

programForStreamType :: ValType -> Gen (Program () Var)
programForStreamType i = do
 o <- genOutputType
 Qc.hedgehog $ CoreGen.programForStreamType i o

programForStreamTypeWithOutput :: ValType -> ValType -> Gen (Program () Var)
programForStreamTypeWithOutput i o = Qc.hedgehog $ CoreGen.programForStreamType i o

genInputType :: Gen ValType
genInputType = Qc.hedgehog $ CoreGen.genInputType

genOutputType :: Gen ValType
genOutputType = Qc.hedgehog $ CoreGen.genOutputType

baseValueForType :: ValType -> Gen BaseValue
baseValueForType = Qc.hedgehog . CoreGen.baseValueForType

inputsForType :: ValType -> Gen ([AsAt (BubbleGumFact, BaseValue)], EvalContext)
inputsForType = Qc.hedgehog . CoreGen.inputsForType

instance Arbitrary ValType where
 arbitrary = Qc.hedgehog CoreGen.genValType

