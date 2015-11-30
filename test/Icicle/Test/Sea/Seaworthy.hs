{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Seaworthy where

import           Icicle.Data
import           Icicle.Internal.Pretty

import qualified Icicle.Sea.Eval                    as S

import qualified Icicle.Avalanche.FromCore          as A

import           Icicle.Test.Arbitrary
import           Icicle.Test.Sea.Arbitrary

import           Control.Monad.Trans.Either

import           X.Control.Monad.Catch (bracketEitherT')

import qualified Data.Map                           as Map

import           P

import           System.IO

import           Disorder.Core.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property

namer = A.namerText (flip Var 0)

-- | Any well-typed Icicle program is convertible to C.
--   Like prop_psv, but without psv.
prop_seaworthy wt
 = testIO
 $ do let seaProgram = Map.singleton (mkAttribute "eval") (wtAvalanche wt)
      x <- runEitherT $ go seaProgram
      return $ case x of
       Right _
        -> property succeeded
       Left err
        -> counterexample (show $ pretty err)
        $  counterexample (show $ pretty (wtCore wt))
        $  failed
 where
  go p
   = bracketEitherT' (S.seaCompile S.NoPsv p) S.seaRelease (const $ return ())


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 50, maxDiscardRatio = 10000})
