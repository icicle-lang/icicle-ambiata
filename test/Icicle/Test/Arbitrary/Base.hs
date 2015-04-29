{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary.Base where

import           Test.QuickCheck

import           P


smaller :: Gen a -> Gen a
smaller g
 = sized
 $ \s -> resize (s `div` 2) g


oneof_sized :: [Gen a] -> [Gen a] -> Gen a
oneof_sized smalls bigs
 = sized
 $ \s -> if   s <= 1
         then oneof  smalls
         else oneof (smalls <> bigs')
 where
  bigs'   = fmap smaller bigs

oneof_sized_vals :: [a] -> [Gen a] -> Gen a
oneof_sized_vals smalls bigs
 = oneof_sized (fmap return smalls) bigs
