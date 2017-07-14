{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Test.Gen.Data.Name (
    genNamespace
  , genInputName
  , genInputId
  , genOutputName
  , genOutputId
  ) where

import           Disorder.Corpus (cooking, waters, boats)

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           Icicle.Data.Name

import           P


genNamespace :: Gen Namespace
genNamespace =
  Gen.just $
    parseNamespace <$> Gen.element cooking

genInputName :: Gen InputName
genInputName =
  Gen.just $
    parseInputName <$> Gen.element waters

genInputId :: Gen InputId
genInputId =
  InputId
    <$> genNamespace
    <*> genInputName

genOutputName :: Gen OutputName
genOutputName =
  Gen.just $
    parseOutputName <$> Gen.element boats

genOutputId :: Gen OutputId
genOutputId =
  OutputId
    <$> genNamespace
    <*> genOutputName
