{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Test.Arbitrary.SourceWithCore where

import Icicle.Test.Arbitrary.Source


import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.List (zip)

import qualified Icicle.Avalanche.Eval as AE
import qualified Icicle.Avalanche.FromCore as AC
import qualified Icicle.Avalanche.Prim.Eval as AE
import qualified Icicle.Avalanche.Prim.Flat as AF
import qualified Icicle.Avalanche.Program as AP
import qualified Icicle.Avalanche.Statement.Flatten as AF
import           Icicle.BubbleGum
import           Icicle.Common.Base
import           Icicle.Common.Eval
import qualified Icicle.Common.Fresh as Fresh
import qualified Icicle.Core.Eval.Program as CV
import qualified Icicle.Core.Program.Program as C
import           Icicle.Data
import           Icicle.Internal.Pretty
import qualified Icicle.Source.Lexer.Token as T
import           Icicle.Test.Arbitrary
import           Icicle.Test.Arbitrary.NanEq

import           P

import           System.IO

import           Test.QuickCheck


data TestSourceConvert
 = TestSourceConvert
 { tsQwf    :: QueryWithFeature
 , tsInputs :: [AsAt (BubbleGumFact, BaseValue)]
 , tsEvalCtx:: EvalContext
 , tsCore   :: C.Program () T.Variable
 , tsAval   :: AP.Program () T.Variable AF.Prim
 }
 deriving Show

instance Arbitrary TestSourceConvert where
 arbitrary
  = do  -- we can be conservative with these expressions
        qwf       <- genQueryWithFeatureTypedGen 10
        (vs,d)    <- inputsForType $ qwfFeatureT qwf
        let ret
              | Right qt'   <- qwfCheckBigData qwf
              , Right k'    <- qwfCheckKey qwf
              , Right c'    <- qwfConvertToCore qwf k' qt'
              , aval        <- freshFromCore $ AC.programFromCore avalancheNamer c'
              , Right flatS <- freshFlat $ AF.flatten () $ AP.statements aval
              , flatP       <- aval { AP.statements = flatS }
              = return $ TestSourceConvert qwf vs d c' flatP
              | otherwise
              = discard
        ret

  where
   avalancheNamer :: AC.Namer T.Variable
   avalancheNamer = AC.namerText T.Variable -- (flip Var 0)

   freshFlat prog
    = fmap snd
    $ Fresh.runFreshT prog
    $ Fresh.counterNameState (counter "flat") 0
 
   freshFromCore prog
    = snd
    $ Fresh.runFresh prog
    $ Fresh.counterNameState (counter "fromCore") 0

   counter desc i =
    NameBase $ T.Variable (desc <> T.pack (show i))
 

evalCore :: TestSourceConvert -> [AsAt (BubbleGumFact, BaseValue)] -> Either (CV.RuntimeError () T.Variable) (CV.ProgramValue T.Variable)
evalCore ts vs
 = CV.eval (tsEvalCtx ts) vs (tsCore ts)

evalAval :: TestSourceConvert -> [AsAt (BubbleGumFact, BaseValue)] -> Either (AE.RuntimeError () T.Variable AF.Prim) ([(OutputName, BaseValue)], Set.Set FactIdentifier)
evalAval ts vs
 = AE.evalProgram AE.evalPrim (tsEvalCtx ts) vs (tsAval ts)

