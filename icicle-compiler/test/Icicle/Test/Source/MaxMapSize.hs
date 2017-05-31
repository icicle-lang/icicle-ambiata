{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.MaxMapSize where

import           Icicle.Test.Arbitrary.SourceWithCore
import           Icicle.Test.Arbitrary
import           Icicle.Core.Program.Check
import qualified Icicle.Core.Eval.Program   as PV
import           Icicle.Internal.Pretty

import           Icicle.Common.Eval
import           Icicle.Common.Base

import           P

import           System.IO

import           Test.QuickCheck
import qualified Data.Map as Map

prop_maxsize :: TestSourceConvert -> Property
prop_maxsize ts =
  let ev = evalCore ts (tsInputs ts)
  in counterexample (show ev)
   $ case ev of
      Left _ -> property False
      Right r  ->
       let vals  = fmap snd $ PV.value r
           props = fmap (checkMaxMapSize $ evalMaxMapSize $ tsEvalCtx ts) vals
       in  conjoin props

checkMaxMapSize :: Int -> BaseValue -> Property
checkMaxMapSize maxSize = go
 where
  go v = case v of
   VMap m
    | checkLength m
    -> conjoin (fmap go $ Map.keys m) .&&. conjoin (fmap go $ Map.elems m)
    | otherwise
    -> counterexample (show m) False

   -- Arbitrary-sized buffers are allowed for now.
   -- We need to allow arrays too, since buffers get converted to arrays. Darn.
   VBuf vs
    -> conjoin (fmap go vs)
   VArray vs
    -> conjoin (fmap go vs)

   VInt    _ -> ok
   VDouble _ -> ok
   VUnit     -> ok
   VBool   _ -> ok
   VTime   _ -> ok
   VString _ -> ok
   VPair a b -> go a .&&. go b
   VLeft   a -> go a
   VRight  a -> go a
   VNone     -> ok
   VSome   a -> go a
   VStruct m -> conjoin (fmap go $ Map.elems m)
   VError  _ -> ok
   VFactIdentifier _ -> ok

  ok = property True

  checkLength vs
   = length vs < maxSize || maxSize <= 0


return []
tests :: IO Bool
tests = $checkAllWith TestRunMore (checkArgsSized 100)
