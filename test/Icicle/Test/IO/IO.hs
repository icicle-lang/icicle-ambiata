{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.IO.IO where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either

import           System.IO
import           System.IO.Temp
import           System.FilePath.Posix
import           P
import           Disorder.Core.IO
import           Disorder.Corpus

import           Data.List (nubBy)

import           Icicle.Data
import           Icicle.Dictionary.Data
import           Icicle.Storage.Dictionary.Toml
import           Icicle.Storage.Dictionary.Toml.Persist
import           Icicle.Test.Arbitrary ()
import           Icicle.Test.Source.Arbitrary ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Property
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set

import qualified Icicle.Internal.Pretty as PP

prop_toml_dictionary_symmetry :: Dictionary -> Property
prop_toml_dictionary_symmetry x
 = testIO
 $ withSystemTempDirectory "foodictionary"
 $ \dir -> do
   let p = dir </> "d.toml"
   writeFile p $ PP.display (PP.renderPretty 0.4 80 $ normalisedTomlDictionary x)
   runEitherT  $ (\x' -> sortD x' === sortD x )    <$> loadDictionary p

prop_toml_dictionary_example :: Property
prop_toml_dictionary_example
 = once
 $ testIO
 $ withSystemTempDirectory "foodictionary"
 $ \dir -> runEitherT $ do
   x  <- loadDictionary "data/example/DictionaryTrial.toml"
   let p = dir </> "d.toml"
   lift $ writeFile p $ PP.display (PP.renderPretty 0.4 80 $ normalisedTomlDictionary x)
   x' <- loadDictionary p
   -- I run pretty here because nameMod is written with a $, but we read as a Name with a $ (it's a bit ugly).
   pure $ (PP.display $ PP.renderCompact $ prettyDictionarySummary $ sortD x') === (PP.display $ PP.renderCompact $ prettyDictionarySummary $ dropFunctions $ sortD x)

instance Arbitrary Dictionary where
 arbitrary
  = Dictionary <$> (nubBy ((==) `on` getAttr) <$> arbitrary) <*> pure Map.empty

instance Arbitrary DictionaryEntry where
 arbitrary
  = DictionaryEntry <$> arbitrary <*> (ConcreteDefinition <$> arbitrary <*> (Set.singleton <$> elements viruses))

instance Show a => Testable (Either a Property) where
  property (Right x) = x
  property (Left  x) = (counterexample . show) x $ failed

dropFunctions :: Dictionary -> Dictionary
dropFunctions (Dictionary es _) = Dictionary es Map.empty

getAttr :: DictionaryEntry -> Attribute
getAttr (DictionaryEntry a _) = a

sortD :: Dictionary -> Dictionary
sortD (Dictionary es fs) =
  Dictionary (sortOn getAttr es) fs

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 20 })
