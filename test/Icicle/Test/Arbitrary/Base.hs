-- | Just some junk for generating arbitrary instances
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Arbitrary.Base where

import           Test.QuickCheck

import           P


-- | Make a smaller generator
smaller :: Gen a -> Gen a
smaller g
 = sized
 $ \s -> resize (s `div` 2) g


-- | Take list of small generators and list of large generators.
-- Look at the size of thing we want to create, and use either small or both
oneof_sized :: [Gen a] -> [Gen a] -> Gen a
oneof_sized smalls bigs
 = sized
 $ \s -> if   s <= 1
         then oneof  smalls
         else oneof (smalls <> bigs')
 where
  bigs'   = fmap smaller bigs

-- | Same as oneof_sized but smalls are just values, not generators
oneof_sized_vals :: [a] -> [Gen a] -> Gen a
oneof_sized_vals smalls bigs
 = oneof_sized (fmap return smalls) bigs

-- | For when you only want values
oneof_vals :: [a] -> Gen a
oneof_vals = oneof . fmap return

