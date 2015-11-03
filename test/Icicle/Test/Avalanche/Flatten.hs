{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Avalanche.Flatten where

import           Icicle.Test.Core.Arbitrary
import           Icicle.Core.Program.Check
import qualified Icicle.Core.Eval.Exp       as XV

import qualified Icicle.Avalanche.Program   as AP
import qualified Icicle.Avalanche.FromCore  as AC
import qualified Icicle.Avalanche.Eval      as AE
import qualified Icicle.Avalanche.Statement.Flatten   as AF
import qualified Icicle.Avalanche.Prim.Eval as AE
import qualified Icicle.Avalanche.Simp      as AS
import qualified Icicle.Avalanche.Check     as AC
import qualified Icicle.Avalanche.Prim.Flat as APF

import           Icicle.Common.Base
import           Icicle.Common.Annot
import           Icicle.Common.Type
import qualified Icicle.Common.Fresh                as Fresh

import           Icicle.Internal.Pretty

import           Data.Either.Combinators

import           P

import           System.IO

import           Test.QuickCheck

-- We need a way to differentiate stream variables from scalars
namer = AC.namerText (flip Var 0)


-- Flattening - removing all folds keeps value same
zprop_flatten_commutes_value t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,d) ->
    P.isRight     (checkProgram p) ==>
     let p' = AC.programFromCore namer p

         eval xp = AE.evalProgram xp d vs

         simp = Fresh.runFreshT
                        (AF.flatten () $ AP.statements p')
                        (Fresh.counterNameState (Name . Var "anf") 0)
     in case simp of
         Left e
          -> counterexample (show e)
           $ counterexample (show $ pretty p')
             False
         Right (_, s')
          -> counterexample (show $ pretty p')
           $ counterexample (show $ pretty s')
             (mapLeft show (eval XV.evalPrim p') === mapLeft show (eval AE.evalPrim p' { AP.statements = s'}))


-- Flatten simplifier preserves value too
prop_flatten_simp_commutes_value t =
 forAll (programForStreamType t)
 $ \p ->
 forAll (inputsForType t)
 $ \(vs,d) ->
    P.isRight (checkProgram p) ==>
     let eval xp  = AE.evalProgram xp d vs
         counter  = (Fresh.counterNameState (Name . Var "anf") 0)
         counter' = (Fresh.counterNameState (Name . Var "simp") 0)
         dummyAnn = Annot (FunT [] ErrorT) ()
         simp s'  = (s', Fresh.runFresh (AS.simpFlattened dummyAnn (avalanched {AP.statements = s'})) counter')
         replaceStmts prog stms
          = prog { AP.statements = stms }
         compareEvalResult xv yv =
           let xv' = mapRight snd (mapLeft show xv)
               yv' = mapRight snd (mapLeft show yv)
           in either (counterexample . show . pretty) (const id) xv $
              either (counterexample . show . pretty) (const id) yv $
              xv' === yv'

         avalanched = AC.programFromCore namer p
         flattened  = Fresh.runFreshT (AF.flatten () $ AP.statements avalanched) counter
         checked    = mapRight (AC.checkProgram APF.flatFragment) (fmap (replaceStmts avalanched . snd) flattened)
     in  case (fmap (fmap (simp . AP.statements)) $ checked) of -- simp <$> checked' of
                  Left e
                   -> counterexample (show e)
                    $ counterexample (show $ pretty avalanched)
                      False
                  Right (Left _)
                   -> discard -- not well-typed avalanche
                  Right (Right (s', (_, p'')))
                   -> counterexample ("FLATTENED:\n" <> show (pretty s' <> "\n")) -- (avalanched { AP.statements = s' }))
                    $ counterexample ("SIMPED:\n" <> show (pretty p''))
                      (eval XV.evalPrim avalanched `compareEvalResult` eval AE.evalPrim p'')


return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10000, maxSize = 10})

