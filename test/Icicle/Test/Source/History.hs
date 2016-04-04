{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Convert where

import              Icicle.BubbleGum
import           Icicle.Test.Arbitrary
import           Icicle.Internal.Pretty
import qualified Icicle.Core.Program.Program    as C
import qualified Icicle.Core.Eval.Program       as CV
import           Icicle.Common.Base
import qualified Icicle.Source.Lexer.Token as T
import              Icicle.Data

-- import qualified Icicle.Avalanche.Program       as A
-- import qualified Icicle.Avalanche.Prim.Flat     as A


import           Data.List (zip)

import           Icicle.Test.Source.Arbitrary
import           Icicle.Test.Core.Arbitrary

import           P

import           System.IO

import           Test.QuickCheck

import qualified Data.Set as Set

data TestStuff
 = TestStuff
 { tsQwf    :: QueryWithFeature
 , tsInputs :: [AsAt (BubbleGumFact, BaseValue)]
 , tsTime   :: Time
 , tsCore   :: C.Program () T.Variable
-- , tsAval :: A.Program () T.Variable A.Flat
 }
 deriving Show

instance Arbitrary TestStuff where
 arbitrary
  = do  -- we can be conservative with these expressions
        qwf       <- genQueryWithFeatureTypedGen 1
        (vs,d)    <- inputsForType $ qwfFeatureT qwf
        let ret
              | Right qt' <- qwfCheckBigData qwf
              , Right c'  <- qwfConvertToCore qwf qt'
              = return $ TestStuff qwf vs d c'
              | otherwise
              = discard
        ret

evalCore ts vs
 = CV.eval (tsTime ts) vs (tsCore ts)

extractFacts inps fids
 = fmap snd
 $ filter (\(ix,_) -> FactIdentifier ix `Set.member` fids)
 $ zip [0..] inps

prop_check_history :: TestStuff -> Property
prop_check_history ts
 = counterexample (qwfPretty $ tsQwf ts)
 $ counterexample (show $ tsInputs ts)
 $ counterexample (show $ pretty $ tsCore ts)
 $ let ret
        | Right pv  <- evalCore ts (tsInputs ts)
        , hist      <- CV.history pv
        , inps'     <- extractFacts (tsInputs ts) hist
        , Right pv' <- evalCore ts inps'
        = ( CV.history pv === CV.history pv' .&&.
            CV.value   pv === CV.value   pv' )
        | otherwise
        = discard
   in ret



return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 100)
