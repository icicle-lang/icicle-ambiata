{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.History where

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


data TestStuff
 = TestStuff
 { tsQwf    :: QueryWithFeature
 , tsInputs :: [AsAt (BubbleGumFact, BaseValue)]
 , tsTime   :: Time
 , tsCore   :: C.Program () T.Variable
 , tsAval   :: AP.Program () T.Variable AF.Prim
 }
 deriving Show

instance Arbitrary TestStuff where
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
              = return $ TestStuff qwf vs d c' flatP
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
 

evalCore ts vs
 = CV.eval (tsTime ts) vs (tsCore ts)

evalAval ts vs
 = AE.evalProgram AE.evalPrim (tsTime ts) vs (tsAval ts)


extractFacts inps fids
 -- Temporary hack:
 -- if there is no recorded history, it might be a group with no folds inside.
 -- If so, we need at least one fact to work over - the last one is fine.
 | Set.null fids
 , x:_ <- reverse inps
 = [x] 

 -- Otherwise just be careful to keep facts in same order
 | otherwise
 = fmap snd
 $ filter (\(ix,_) -> FactIdentifier ix `Set.member` fids)
 $ zip [0..] inps

prop_check_history_core :: TestStuff -> Property
prop_check_history_core ts
 = counterexample (qwfPretty $ tsQwf ts)
 $ counterexample (show $ tsInputs ts)
 $ counterexample (show $ pretty $ tsCore ts)
 $ let ret
        | Right pv  <- evalCore ts (tsInputs ts)
        , hist      <- CV.history pv
        , inps'     <- extractFacts (tsInputs ts) hist
        , Right pv' <- evalCore ts inps'
        -- Only check if the sizes are the same, since fact identifier (index) changes
        = ( Set.size (CV.history pv) === Set.size (CV.history pv') .&&.
            CV.value   pv =~= CV.value   pv' )
        | otherwise
        = discard
   in ret

prop_check_history_aval :: TestStuff -> Property
prop_check_history_aval ts
 = counterexample (qwfPretty $ tsQwf ts)
 $ counterexample (show $ tsInputs ts)
 $ counterexample (show $ pretty $ tsAval ts)
 $ let ret
        | Right pv  <- evalAval ts (tsInputs ts)
        , inps'     <- extractFacts (tsInputs ts) (snd pv)
        , Right pv' <- evalAval ts inps'
        = ( Set.size (snd pv) === Set.size (snd pv') .&&.
            fst   pv =~= fst   pv' )
        | otherwise
        = discard
   in ret


prop_check_history_core_same_aval :: TestStuff -> Property
prop_check_history_core_same_aval ts
 = counterexample (qwfPretty $ tsQwf ts)
 $ counterexample (show $ tsInputs ts)
 $ counterexample (show $ pretty $ tsCore ts)
 $ counterexample (show $ pretty $ tsAval ts)
 $ let ret
        | Right pvc <- evalCore ts (tsInputs ts)
        , Right pva <- evalAval ts (tsInputs ts)
        = ( CV.history pvc === snd pva .&&.
            CV.value   pvc =~= fst pva )
        | otherwise
        = discard
   in ret


return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 100)
