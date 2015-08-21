{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Internal.EditDistance where

import           Icicle.Internal.EditDistance
import           Icicle.Internal.Pretty

import           P

import           System.IO

import           Test.QuickCheck

import           Data.List (delete)

-- Also covers adding one iff symmetry passes
prop_edit_rem_one :: Char -> [Char] -> Property
prop_edit_rem_one c cs = (isJust $ find ((==) c) cs) ==>
  (===) 1 $ editDistance (text cs) (text $ delete c cs)

prop_edit_symmetric :: [Char] -> [Char] -> Property
prop_edit_symmetric as bs =
  (===) (editDistance (text as) (text bs)) (editDistance (text bs) (text as))

prop_edit_swap_one :: [Char] -> [Char] -> Char -> Char -> Property
prop_edit_swap_one as bs a b = a /= b ==>
  (===) 1 $ (editDistance (text $ as <> (pure a) <> bs) (text $ as <> (pure b) <> bs))

prop_edit_flip_middle :: [Char] -> [Char] -> Char -> Char -> Property
prop_edit_flip_middle as bs a b = a /= b ==>
  (===) 1 $ (editDistance (text $ as <> (pure a) <> (pure b) <> bs) (text $ as <> (pure b) <> (pure a) <> bs))

return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs { {- maxSuccess = 5000, -} maxSize = 10,  maxDiscardRatio = 10000})
