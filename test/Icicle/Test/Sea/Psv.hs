{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Psv where

import           Control.Monad.Trans.Either

import qualified Data.Map as Map

import           Disorder.Core.IO

import           Icicle.Data
import           Icicle.Internal.Pretty

import qualified Icicle.Sea.Eval as S

import           Icicle.Test.Sea.Arbitrary

import qualified Jetski as J

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property

import           X.Control.Monad.Catch (bracketEitherT')


prop_psv wt = testIO $ do
  let seaProgram = Map.singleton (Attribute "eval") (wtAvalanche wt)
  x <- runEitherT $ bracketEitherT' (S.seaCompile S.Psv seaProgram) S.seaRelease (const (pure ()))
  case x of
    Right _
     -> pure (property succeeded)
    Left (S.SeaJetskiError (J.CompilerError _ src err))
     -> pure
      $ counterexample (show $ pretty src)
      $ counterexample (show $ pretty err)
      $ counterexample (show $ pretty (wtCore wt))
      $ failed
    Left err
     -> pure
      $ counterexample (show $ pretty err)
      $ counterexample (show $ pretty (wtCore wt))
      $ failed

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 50, maxDiscardRatio = 10000})
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})
