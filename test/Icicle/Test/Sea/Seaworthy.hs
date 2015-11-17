{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Seaworthy where

import           Icicle.Data
import           Icicle.Internal.Pretty

import qualified Icicle.Core.Program.Check          as C

import qualified Icicle.Sea.Eval                    as S

import qualified Icicle.Avalanche.FromCore          as A

import qualified Icicle.Pipeline as P

import           Icicle.Test.Arbitrary
import           Icicle.Test.Core.Arbitrary

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           X.Control.Monad.Catch (bracketEitherT')
import qualified Data.Map                           as Map

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Monadic


namer = A.namerText (flip Var 0)

-- | Any Icicle Core program is convertible to C.
prop_seaworthy t
 = monadicIO
 $ forAllM (programForStreamType t)
 $ \coreProgram 
 -> do pre $ isRight (C.checkProgram coreProgram)
       let flat = join $ fmap P.checkAvalanche $ P.coreFlatten coreProgram
       case flat of
        Right f
         -> do fleet <- lift $ runEitherT $ go f
               stop $ case fleet of
                Right _
                 -> property True
                Left err
                 -> counterexample (show $ pretty err)
                 $  counterexample (show $ pretty coreProgram)
                    False
        Left _
         -> discard -- not well typed flattened avalanche
 where
  go f
   = do let attr       = Attribute "eval"
            seaProgram = Map.singleton attr f
        bracketEitherT' (S.seaCompile S.NoPsv seaProgram) S.seaRelease (const $ return ())


return []
tests :: IO Bool
tests = $quickCheckAll
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})
