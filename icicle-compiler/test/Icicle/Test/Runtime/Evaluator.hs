{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Evaluator where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (hoist)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Boxed

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Icicle.Compiler as Compiler
import qualified Icicle.Compiler.Source as Compiler
import           Icicle.Data.Fact
import           Icicle.Data.Name
import           Icicle.Dictionary.Data
import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped
import qualified Icicle.Runtime.Evaluator as Runtime
import           Icicle.Test.Gen.Data.Fact
import           Icicle.Test.Gen.Data.Name
import           Icicle.Test.Gen.Runtime.Data (genInputColumn, genEntityKey)

import           P

import qualified Prelude as Savage

import           System.IO (IO)


genDictionaryInput :: Gen DictionaryInput
genDictionaryInput =
  DictionaryInput
    <$> genInputId
    <*> genEncoding
    <*> pure Set.empty
    <*> pure unkeyed

latest :: Int -> DictionaryInput -> Text
latest n input =
  let
    vname =
      case inputEncoding input of
        StructEncoding _ ->
          "fields"
        _ ->
          "value"
  in
    Text.pack $
      "feature " <>
      show (renderInputId (inputId input)) <>
      " ~> latest " <> show n <> " ~> " <>
      vname

fromInputId :: Int -> InputId -> OutputId
fromInputId n iid =
  fromMaybe (Savage.error $ "fromInputId: " <> show iid) .
  parseOutputId .
  (<> "/" <> Text.pack (show n)) $
  renderInputId iid

fromDictionaryInput :: DictionaryInput -> [DictionaryOutput]
fromDictionaryInput input =
  let
    dictionary =
      Dictionary (mapOfInputs [input]) Map.empty []

    oid n =
      fromInputId n $ inputId input

    output n =
      case Compiler.queryOfSource Compiler.defaultCheckOptions dictionary (oid n) $ latest 1000 input of
        Left x ->
          Savage.error $ "genDictionaryOutput: " <> show x -- shouldn't really happen, query is trivial.
        Right x ->
          DictionaryOutput (oid n) x
  in
    [output 0, output 1]

genDictionary :: Gen Dictionary
genDictionary = do
  inputs <- mapOfInputs <$> Gen.list (Range.linear 1 5) genDictionaryInput

  let
    outputs =
      mapOfOutputs . concatMap fromDictionaryInput $ Map.elems inputs

  pure $
    Dictionary inputs outputs []

genDictionaryInputColumn :: Int -> DictionaryInput -> Gen InputColumn
genDictionaryInputColumn n_entities input =
  case Schema.fromEncoding $ inputEncoding input of
    Left x ->
      Savage.error $ "genDictionaryInputColumn: " <> show x
    Right schema ->
      genInputColumn n_entities schema

genInput :: Dictionary -> Gen Input
genInput dictionary = do
  entities <- Boxed.fromList <$> Gen.list (Range.linear 1 5) genEntityKey
  Input entities <$> traverse (genDictionaryInputColumn (Boxed.length entities)) (dictionaryInputs dictionary)

maximumMapSize :: MaximumMapSize
maximumMapSize =
  100

fromInputColumn :: InputColumn -> Striped.Column
fromInputColumn input =
  Striped.Array
    (inputLength input)
    (Striped.Result (inputTombstone input) (inputColumn input))

data EvaluatorTest =
  EvaluatorTest !Dictionary !Input
  deriving (Show)

genEvaluatorTest :: Gen EvaluatorTest
genEvaluatorTest = do
  dictionary <- genDictionary
  EvaluatorTest dictionary <$> genInput dictionary

prop_evaluator :: Property
prop_evaluator =
  withTests 100 . property $ do
    EvaluatorTest dictionary input <- forAll genEvaluatorTest

    --stime <- forAll $ genSnapshotTime

    let
      stime =
        SnapshotTime . packTime $
          UnpackedTime64 3000 1 1 0

      options =
        Compiler.defaultCompileOptions {
            Compiler.icicleFusionOptions =
              Compiler.FusionOptions {
                  Compiler.fusionMaximumPerKernel =
                    1
                }
          }

    avalanche <- evalEither $ Compiler.avalancheOfDictionary options dictionary
    sea <- evalEither . Runtime.compileAvalanche $ Runtime.AvalancheContext "Icicle.Test.Runtime.Evaluator" avalanche
    runtime <- evalExceptT $ Runtime.compileSea Runtime.SkipJetskiCache sea
    output <- evalExceptT . hoist liftIO $ Runtime.snapshotBlock runtime maximumMapSize stime input

    let
      two (k, v) = [
          (fromInputId 0 k, fromInputColumn v)
        , (fromInputId 1 k, fromInputColumn v)
        ]

      inputAsOutput =
        Map.fromList . concatMap two . Map.toList $ inputColumns input

    inputAsOutput === outputColumns output

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
