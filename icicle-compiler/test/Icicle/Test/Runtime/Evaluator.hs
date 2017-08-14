{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Evaluator where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Icicle.Compiler as Compiler
import qualified Icicle.Compiler.Source as Compiler
import           Icicle.Data.Fact
import           Icicle.Data.Name
import           Icicle.Dictionary.Data
import           Icicle.Runtime.Data
import           Icicle.Runtime.Data.Mask
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped
import           Icicle.Runtime.Evaluator (Runtime)
import qualified Icicle.Runtime.Evaluator as Runtime
import           Icicle.Test.Gen.Data.Fact
import           Icicle.Test.Gen.Data.Name
import           Icicle.Test.Gen.Runtime.Data (genEntityInputColumn, genEntityKey)
import           Icicle.Test.Gen.Runtime.Data (genSnapshotTime, genQueryTime)

import           P

import qualified Prelude as Savage

import           System.IO (IO)

import qualified X.Data.Vector.Cons as Cons


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

genDictionaryInputColumn :: DictionaryInput -> Gen InputColumn
genDictionaryInputColumn input =
  case Schema.fromEncoding $ inputEncoding input of
    Left x ->
      Savage.error $ "genDictionaryInputColumn: " <> show x
    Right schema ->
      genEntityInputColumn schema

genInputColumns :: Int -> Dictionary -> Gen (Map InputId [InputColumn])
genInputColumns n_entities dictionary = do
  let
    range_entities =
      Range.singleton n_entities

  traverse (Gen.list range_entities . genDictionaryInputColumn) (dictionaryInputs dictionary)

concatEntities :: [InputColumn] -> InputColumn
concatEntities =
  either (Savage.error . ("genInput: " <>) . show) id . concatInputColumn . Cons.unsafeFromList

maximumMapSize :: MaximumMapSize
maximumMapSize =
  100

fromInputColumn :: InputColumn -> Striped.Column
fromInputColumn input =
  Striped.Array
    (inputLength input)
    (Striped.Result (inputTombstone input) (inputColumn input))

data EvaluatorTest =
  EvaluatorTest {
      testDictionary :: !Dictionary
    , testEntities :: !(Boxed.Vector EntityKey)
    , testInputs :: !(Map InputId [InputColumn])
    } deriving (Show)

genEvaluatorTest :: Gen EvaluatorTest
genEvaluatorTest = do
  dictionary <- genDictionary
  entities <- Boxed.fromList . Set.toList <$> Gen.set (Range.linear 1 5) genEntityKey
  inputs <- genInputColumns (Boxed.length entities) dictionary
  pure $ EvaluatorTest dictionary entities inputs

takeInput :: EvaluatorTest -> Input
takeInput et =
  Input (testEntities et) (fmap concatEntities $ testInputs et)

takeWhileBefore :: QueryTime -> InputColumn -> InputColumn
takeWhileBefore (QueryTime qtime) icolumn =
  if Storable.length (inputLength icolumn) /= 1 then
    Savage.error "takeWhileBefore: this function only works for a single entities"
  else
    let
      time =
        Storable.takeWhile (< InputTime qtime) $ inputTime icolumn

      n =
        Storable.length time

      tombstone =
        Storable.take n $ inputTombstone icolumn

      (column, _) =
        Striped.splitAt n $ inputColumn icolumn
    in
      InputColumn (Storable.singleton $ fromIntegral n) time tombstone column

inputColumnMask :: InputColumn -> Mask
inputColumnMask x =
  if Storable.length (inputLength x) /= 1 then
    Savage.error "inputColumnMask: this function only works for a single entities"
  else if Storable.singleton 0 == inputLength x then
    Drop
  else
    Keep

inputMask :: Map k [InputColumn] -> Boxed.Vector Mask
inputMask =
  Boxed.fromList .
  fmap mconcat .
  List.transpose .
  Map.elems .
  fmap (fmap inputColumnMask)

trimSnapshot :: SnapshotTime -> EvaluatorTest -> Input
trimSnapshot stime0 et =
  let
    stime =
      unSnapshotTime stime0

    inputs =
      fmap (fmap (takeWhileBefore stime)) $
      testInputs et

    mask =
      inputMask inputs
  in
    maskInput mask $
      Input
        (testEntities et)
        (fmap concatEntities inputs)

trimChord :: [Set QueryTime] -> EvaluatorTest -> Input
trimChord qtimes00 et =
  let
    qtimes0 =
      fmap Set.toList qtimes00

    rep qtimes entity =
      List.replicate (length qtimes) entity

    go qtimes column =
      with qtimes $ \qtime ->
        takeWhileBefore qtime column

    inputs =
      fmap (concat . List.zipWith go qtimes0) $
      testInputs et

    mask =
      inputMask inputs
  in
    maskInput mask $
      Input
        (Boxed.fromList . concat . List.zipWith rep qtimes0 . Boxed.toList $ testEntities et)
        (fmap concatEntities inputs)

compileDictionary :: (MonadTest m, MonadIO m) => Dictionary -> m Runtime
compileDictionary dictionary = do
  ccoptions0 <- Runtime.getCompilerOptions

  let
    options =
      Compiler.defaultCompileOptions {
        Compiler.icicleFusionOptions =
          Compiler.FusionOptions {
            Compiler.fusionMaximumPerKernel =
                  1
          }
      }

    ccoptions = [
        "-DICICLE_ASSERT=1"
      , "-DICICLE_ASSERT_MAXIMUM_ARRAY_COUNT=1000000"
      , "-DICICLE_NOINLINE=1"
      , "-O0"
      ] <>
      ccoptions0

  avalanche <- evalEither $ Compiler.avalancheOfDictionary options dictionary
  sea <- evalEither . Runtime.compileAvalanche $ Runtime.AvalancheContext "Icicle.Test.Runtime.Evaluator" avalanche
  evalExceptT $ Runtime.compileSeaWith ccoptions Runtime.SkipJetskiCache sea

prop_evaluator_roundtrip_chord :: Property
prop_evaluator_roundtrip_chord =
  withTests 100 . property $ do
    et@(EvaluatorTest dictionary entities _inputs) <- forAll genEvaluatorTest
    qtimes <- forAll . Gen.list (Range.singleton $ Boxed.length entities) . Gen.set (Range.linear 1 10) $ genQueryTime

    let
      mkLabel qtime =
        Label qtime (Text.encodeUtf8 . renderTime $ unQueryTime qtime)

      chordMap =
        Map.fromList $ List.zip (fmap entityId $ Boxed.toList entities) (fmap (Set.mapMonotonic mkLabel) qtimes)

      descriptor =
        ChordDescriptor $ \eid ->
          fromMaybe Set.empty (Map.lookup eid chordMap)

      input =
        takeInput et

    annotateShow chordMap

    runtime <- compileDictionary dictionary
    output <- evalExceptT . hoist liftIO $ Runtime.chordBlock runtime maximumMapSize descriptor input

    compareInputOutput (trimChord qtimes et) output

prop_evaluator_roundtrip_snapshot :: Property
prop_evaluator_roundtrip_snapshot =
  withTests 100 . property $ do
    et@(EvaluatorTest dictionary _entities _inputs) <- forAll genEvaluatorTest
    stime <- forAll genSnapshotTime

    let
      input =
        takeInput et

    runtime <- compileDictionary dictionary
    output <- evalExceptT . hoist liftIO $ Runtime.snapshotBlock runtime maximumMapSize stime input

    compareInputOutput (trimSnapshot stime et) output

compareInputOutput :: MonadTest m => Input -> Output key -> m ()
compareInputOutput input output = do
  let
    two (k, v) = [
        (fromInputId 0 k, fromInputColumn v)
      , (fromInputId 1 k, fromInputColumn v)
      ]

    inputAsOutput =
      Map.fromList .
      concatMap two .
      Map.toList $
      inputColumns input
  
  annotateShow input
  inputAsOutput === outputColumns output

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
