{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Serial where

import           Icicle.Test.Arbitrary ()
import           Icicle.Serial

import           P

import           System.IO

import           Test.QuickCheck


-- Render/parse symmtery
-- =====================

prop_eavt f = (parseEavt . renderEavt) f === Right f



return []
tests :: IO Bool
tests = $quickCheckAll
