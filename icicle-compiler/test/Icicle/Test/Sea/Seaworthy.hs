{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Seaworthy where

import           Icicle.Data
import           Icicle.Internal.Pretty

import qualified Icicle.Sea.Eval                    as S

import qualified Icicle.Avalanche.FromCore          as A

import           Icicle.Test.Arbitrary

import           Data.List.NonEmpty ( NonEmpty(..) )
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
prop_seaworthy
 = forAll (validated tryCountInputType genWellTypedCluster) $ \wt -> testIO
 $ do let a = [inputid|default:eval|]
      let seaProgram = Map.singleton a (wtAvalancheFlat wt :| [])
      x <- runEitherT $ go [a] seaProgram
      return $ case x of
       Right _
        -> property succeeded
       Left err
        -> counterexample (show $ pretty err)
        $  counterexample (show $ pretty (wtCore wt))
        $  counterexample (show $ pretty (wtAvalancheFlat wt))
        $  failed
 where
  go a p =
    bracketEitherT' (S.seaCompile "Icicle.Test.Sea.Seaworthy.prop_seaworthy" S.SkipJetskiCache S.NoInput a p Nothing) S.seaRelease (const $ return ())


return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
