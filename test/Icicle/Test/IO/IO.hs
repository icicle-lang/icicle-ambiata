{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.IO.IO where

import           Control.Monad.Trans.Class

import           Data.List (nubBy)
import qualified Data.Set                           as Set

import           Disorder.Core.IO
import           Disorder.Corpus

import           Icicle.Data
import           Icicle.Dictionary.Data
import qualified Icicle.Internal.Pretty as PP
import           Icicle.Storage.Dictionary.Toml
import           Icicle.Storage.Dictionary.Toml.Persist
import           Icicle.Test.Arbitrary
import           Icicle.Source.Checker.Base (optionSmallData)

import           P

import           System.IO
import           System.IO.Temp
import           System.FilePath.Posix

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Property

import           X.Control.Monad.Trans.Either

prop_toml_dictionary_symmetry :: Dictionary -> Property
prop_toml_dictionary_symmetry x
 = testIO
 $ withSystemTempDirectory "foodictionary"
 $ \dir -> do
   let p = dir </> "d.toml"
   let i = dir </> "normalised_imports.icicle"
   writeFile p $ PP.display (PP.renderPretty 0.4 80 $ normalisedTomlDictionary x)
   writeFile i $ PP.display (PP.renderPretty 0.4 80 $ normalisedFunctions x)
   runEitherT  $ (\x' -> sortD x' === sortD x )    <$> loadDictionary optionSmallData NoImplicitPrelude p

prop_toml_dictionary_example :: Property
prop_toml_dictionary_example
 = once
 $ testIO
 $ withSystemTempDirectory "foodictionary"
 $ \dir -> runEitherT $ do
   x  <- loadDictionary optionSmallData ImplicitPrelude "data/example/DictionaryTrial.toml"
   let p = dir </> "d.toml"
   let i = dir </> "normalised_imports.icicle"
   lift $ writeFile p $ PP.display (PP.renderPretty 0.4 80 $ normalisedTomlDictionary x)
   lift $ writeFile i $ PP.display (PP.renderPretty 0.4 80 $ normalisedFunctions x)
   x' <- loadDictionary optionSmallData NoImplicitPrelude p
   -- I run pretty here because nameMod is written with a $, but we read as a Name with a $ (it's a bit ugly).
   pure $ (PP.display $ PP.renderCompact $ prettyDictionarySummary $ sortD x') === (PP.display $ PP.renderCompact $ prettyDictionarySummary $ sortD x)

instance Arbitrary Dictionary where
 arbitrary
  = Dictionary <$> (nubBy ((==) `on` getAttr) <$> arbitrary) <*> pure []

instance Arbitrary DictionaryEntry where
 arbitrary
  = DictionaryEntry <$> arbitrary <*> (ConcreteDefinition <$> arbitrary <*> (Set.singleton <$> elements viruses))

instance Show a => Testable (Either a Property) where
  property (Right x) = x
  property (Left  x) = (counterexample . show) x $ failed

getAttr :: DictionaryEntry -> Attribute
getAttr (DictionaryEntry a _) = a

sortD :: Dictionary -> Dictionary
sortD (Dictionary es fs) =
  Dictionary (sortOn getAttr es) fs

return []
tests :: IO Bool
tests = $checkAllWith TestRunFewer checkArgs
