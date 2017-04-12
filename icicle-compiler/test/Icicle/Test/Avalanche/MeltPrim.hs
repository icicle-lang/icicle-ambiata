{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE PatternGuards   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.MeltPrim where

import           Icicle.Test.Arbitrary

import qualified Icicle.Avalanche.Annot               as AA
import           Icicle.Avalanche.Statement.Simp.Melt
import           Icicle.Avalanche.Statement.Simp.Constructor
import           Icicle.Avalanche.Statement.Statement
import qualified Icicle.Avalanche.Prim.Eval as FlatEval
import qualified Icicle.Avalanche.Prim.Flat as Flat
import qualified Icicle.Avalanche.Eval      as Eval
import           Icicle.Avalanche.Statement.Simp (freevarsStmt)

import           Icicle.Data (Namespace(..))
import           Icicle.Data.Time (timeOfDays)
import           Icicle.Common.Base
import           Icicle.Common.Exp
import           Icicle.Common.Type
import           Icicle.Common.FixT (fixpoint)


import           Icicle.Internal.Pretty


import           P

import qualified Data.Map as Map

import           System.IO

import           Test.QuickCheck

import Data.List (zip)

-- TODO: add more?
genPrim :: Gen Flat.Prim
genPrim = Flat.PrimMinimal <$> arbitrary

genPrimApps :: Gen (Flat.Prim, [(ValType, BaseValue)])
genPrimApps = do
  prim <- genPrim
  let ts = fmap functionReturns $ functionArguments $ Flat.typeOfPrim prim
  vs <- mapM baseValueForType ts
  return (prim, zip ts vs)

prop_melt_prim = forAll genPrimApps $ \(prim, vals) ->
  let funT = Flat.typeOfPrim prim
      resT = functionReturns funT
      args = fmap (\(t,v) -> XValue () t v) vals
      xpp  = makeApps () (XPrim () prim) args
      out  = OutputName "out" (Namespace "out")
      inits :: Statement () Var Flat.Prim
      inits = Output out resT [(xpp, resT)] 
      melts = testFresh "melt" $ do
               s0 <- melt () inits
               let s1 = freevarsStmt s0
               s2 <- fixpoint (constructor ()) s1
               return $ AA.reannotS (const ()) s2

  in counterexample "initial program: "
   $ counterexample (show $ pretty inits)
   $ counterexample "melted program: "
   $ counterexample (show $ pretty melts)
   $ case (evalOut inits, evalOut melts) of
      (Left l, _)
       -> counterexample "init eval error: "
        $ counterexample l
          False
      (_, Left l)
       -> counterexample "melt eval error: "
        $ counterexample l
          False
      (Right [(o1,v1)], Right [(o2,v2)])
       | out == o1
       , out == o2
       -> v1 === v2
      (Right os1, Right os2)
       -> counterexample "init eval got: "
        $ counterexample (show os1)
        $ counterexample "melt eval got: "
        $ counterexample (show os2)
          False


evalOut :: Statement () Var Flat.Prim
        -> Either [Char] [(OutputName,BaseValue)]
evalOut ss
 = case Eval.evalStmt FlatEval.evalPrim (timeOfDays 0) Map.empty [] Nothing Map.empty ss of
    Left err
     -> Left ("Evaluating: got " <> show err)
    Right (acc,outs,_)
     | not $ Map.null acc
     -> Left ("Oh no! The accumulator heap should be empty but got: " <> show acc)
     | otherwise
     -> Right outs
  

return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 10)
