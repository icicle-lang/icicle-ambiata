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

import qualified Data.Map                           as Map

import           P

import           System.IO

import           Disorder.Core.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property

import           X.Control.Monad.Trans.Either


namer = A.namerText (flip Var 0)

-- | Any well-typed Icicle program is convertible to C.
--   Like prop_psv, but without psv.
prop_seaworthy wt
 = testIO
 $ do let seaProgram = Map.singleton (Attribute "eval") (wtAvalancheFlat wt)
      x <- runEitherT $ go seaProgram
      return $ case x of
       Right _
        -> property succeeded
       Left err
        -> counterexample (show $ pretty err)
        $  counterexample (show $ pretty (wtCore wt))
        $  counterexample (show $ pretty (wtAvalancheFlat wt))
        $  failed
 where
  go p
   = bracketEitherT' (S.seaCompile S.NoPsv p) S.seaRelease (const $ return ())


return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
