{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Test.Sea.Psv where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (hoist)

import qualified Data.ByteString.Lazy as L
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.String
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT

import           Disorder.Core.IO

import           Icicle.Data
import           Icicle.Internal.Pretty

import           Icicle.Common.Type
import           Icicle.Common.Eval

import           Icicle.Sea.Eval

import           Icicle.Test.Arbitrary
import           Icicle.Test.Arbitrary.Corpus
import           Icicle.Test.Sea.Utils

import qualified Jetski as J

import           P
import qualified Prelude as Savage

import           System.IO

import           Test.QuickCheck (Arbitrary(..), suchThat, forAll)
import           Test.QuickCheck (Property, (==>), property, counterexample)
import           Test.QuickCheck.Property (succeeded, failed)
import           Test.QuickCheck.Monadic

import           X.Control.Monad.Trans.Either (left)
import           X.Control.Monad.Trans.Either (EitherT, hoistEither, bracketEitherT', runEitherT)


optExpectSuccess =
  TestOpts ExpectSuccess PsvInputSparse

optExpectFailure =
  TestOpts ExpectFailure PsvInputSparse

testForSuccess dup wt psv =
  testIO $ do
   x <- runEitherT . runTest wt psv . optExpectSuccess $ dup
   case x of
     Left err ->
       failWithError wt err
     Right _ ->
       pure (property succeeded)

genWT1 d x =
  validated 10 $
    tryGenWellTypedWithInput d x

genWT2 d x y =
  validated 10 $
    tryGenWellTypedWithInputAndOutput d x y

-- This test shouldn't be necessary. The other properties should cover it.
-- Perhaps useful for debugging only.
zprop_success_output_type_is_time
 | dup <- DoNotAllowDupTime
 =  forAll arbitrary $ \inputType ->
    forAll (genWT2 dup inputType TimeT) $ \wt ->
    forAll (genPsvConstants wt) $ \psvConstants ->
      testForSuccess dup wt psvConstants

prop_success_array_of_struct_input
 | dup <- DoNotAllowDupTime
 = forAll genSupportedArrayStructFactType $ \factType ->
   forAll (genWT1 dup (inputTypeOf factType)) $ \wt ->
   forAll (genPsvConstants wt) $ \psvConstants ->
     testForSuccess dup wt psvConstants

prop_success_psv_corpus
 | dup <- AllowDupTime
 = testAllCorpus dup genPsvConstants $ \wt psv ->
     testForSuccess dup wt psv

prop_success_psv
 | dup <- DoNotAllowDupTime
 = forAll arbitrary $ \inputType ->
   forAll (genWT1 dup inputType) $ \wt ->
   forAll (genPsvConstants wt) $ \psv ->
     testForSuccess dup wt psv

prop_failure_entity_out_of_order
 | dup <- DoNotAllowDupTime
 = forAll arbitrary $ \inputType ->
   forAll (genWT1 dup inputType) $ \wt ->
   List.length (wtFacts wt) > 1 ==>
   forAll (genPsvConstants wt) $ \psv ->
     testIO $ do
       let
         replaceEntity i wtv =
           wtv { eavtEntity = Entity ("entity_" <> T.pack (show i)) }
         wtOutOfOrderEntities =
           wt { wtFacts = List.reverse . List.zipWith replaceEntity [(0::Int)..] . wtFacts $ wt }
       result <-
         runEitherT .
         runTest wtOutOfOrderEntities psv .
         optExpectFailure $
           dup
       expectPsvError wtOutOfOrderEntities result

prop_failure_time_out_of_order
 | dup <- DoNotAllowDupTime
 = forAll arbitrary $ \inputType ->
   forAll (validated 100 $ tryGenAttributeWithInput inputType) $ \wta ->
   forAll (validated 100 $ tryGenWellTypedForSingleAttribute dup wta) $ \wt ->
   List.length (wtFacts wt) > 1 ==>
   forAll (genPsvConstants wt) $ \psv ->
     testIO $ do
       let
         wtOutOfOrderTimes =
           wt { wtFacts = List.reverse . wtFacts $ wt }
       result <-
         runEitherT .
         runTest wtOutOfOrderTimes psv .
         optExpectFailure $
           dup
       expectPsvError wtOutOfOrderTimes result

prop_dup_time
  = forAll (genWellTypedWithDuplicateTimes `suchThat` ((> 1) . List.length . wtFacts)) $ \wt ->
    forAll (genPsvConstants wt) $ \psv ->
      monadicIO $ do
        let wt' = wt { wtFacts = List.head (wtFacts wt) : wtFacts wt }
        a  <- liftIO .  runEitherT . runTest wt' psv . optExpectSuccess $ AllowDupTime
        b  <- liftIO .  runEitherT . runTest wt' psv . optExpectFailure $ DoNotAllowDupTime
        case a of
          Left err -> stop $ testIO $ failWithError wt' err
          Right _  -> stop $ testIO $ expectPsvError wt' b

expectPsvError :: WellTyped -> Either SeaError () -> IO Property
expectPsvError wt = \case
  Left (SeaPsvError msg)
   | "out of order" `T.isInfixOf` msg
   -> pure (property succeeded)

   | otherwise
   -> pure
    $ counterexample "expected error containing 'out of order'"
    $ counterexample ("error was <" <> T.unpack msg <> ">")
    $ failed

  Left err
   -> failWithError wt err

  Right ()
   -> pure
    $ counterexample "data was out of order, but icicle did not give an error"
    $ failed

failWithError :: WellTyped -> SeaError -> IO Property
failWithError = failWithError' id

failWithError' :: (Property -> Property) -> WellTyped -> SeaError -> IO Property
failWithError' prints wt = \case
  SeaJetskiError (J.CompilerError _ src err)
   -> pure
    $ prints
    $ counterexample (show (pretty src))
    $ counterexample (show (pretty err))
    $ counterexample (show (pretty (fmap (pretty . wtCore) (wtAttributes wt))))
    $ failed

  err
   -> pure
    $ prints
    $ counterexample (show (pretty err))
    $ counterexample (show (pretty (fmap (pretty . wtCore) (wtAttributes wt))))
    $ failed

------------------------------------------------------------------------

data Expect = ExpectSuccess | ExpectFailure
  deriving (Eq, Show)

data TestOpts = TestOpts Expect PsvInputFormat InputAllowDupTime
  deriving (Eq, Show)

compileTest :: WellTyped -> TestOpts -> EitherT SeaError  IO (SeaFleet PsvState)
compileTest wt (TestOpts _ inputFormat allowDupTime) = do
  defaultOptions <- getCompilerOptions

  let
    maxArrayCount =
      100 * length (wtFacts wt)

    options =
      [ "-DICICLE_ASSERT=1"
      , "-DICICLE_ASSERT_MAXIMUM_ARRAY_COUNT=" <> T.pack (show maxArrayCount)
      , "-O0"
      , "-DICICLE_NOINLINE=1"
      ] <> defaultOptions

    time =
      evalSnapshotTime (wtEvalContext wt)

    missingValuesFor wta =
      (wtInputId wta, Set.singleton tombstone)

    hasInput =
      HasInput
        (FormatPsv
           (PsvConfig
              (PsvInputConfig
                 (Snapshot time)
                 inputFormat
              (PsvOutputConfig
                 (Snapshot time)
                 PsvOutputSparse
                 defaultOutputMissing
              )
           )
        )
        (InputOpts
           allowDupTime .
           Map.fromList .
           fmap missingValuesFor .
           wtAttributes $
             wt
        )
        ("" :: String)

    programFrom w =
      (wtInputId w, wtAvalancheFlat w :| [])

    programs =
      fmap programFrom . wtAttributes $ wt

    chords =
      Nothing

    cache =
      NoCacheSea

  code <- hoistEither $ codeOfPrograms "Icicle.Test.Sea.Psv.compileTest"  hasInput (fmap fst programs) programs

  -- This test only uses snapshot so we can do this.
  let
    savage =
      textOfDoc . vsep $
        [ "int64_t piano_max_count (piano_t *piano) {"
        , "    return 1;"
        , "}"
        , ""
        , "error_t piano_lookup (piano_t *piano, const uint8_t *needle_id, size_t needle_id_size, int64_t *out_count, const int64_t **out_label_times, const int64_t **out_label_name_offsets, const int64_t **out_label_name_lengths, const uint8_t **out_label_name_data) {"
        , "    return 0;"
        , "}"
        ]
    textCode =
      code <> savage

  seaCreateFleet options (fromCacheSea cache) hasInput chords textCode

wtEntities =
  List.nub . fmap eavtEntity . wtFacts

runTest :: WellTyped -> PsvConstants -> TestOpts -> EitherT SeaError IO ()
runTest wt consts testOpts@(TestOpts yourParents _ _) = do
  let compile =
        compileTest wt testOpts
      release =
        seaRelease
      expectValues =
        evalWellTyped wt (psvFactsLimit consts)
      expectText =
       textOfOutputs expectValues

  bracketEitherT' compile (hoist liftIO . release) $ \fleet -> hoist liftIO $ do

  let install  = liftIO (sfSegvInstall fleet (show consts <> "\n" <> show wt))
      remove _ = liftIO (sfSegvRemove  fleet)
  bracketEitherT' install remove  $ \() -> do

  withSystemTempDirectory "psv-test-" $ \dir -> do
    let source  = J.libSource (sfLibrary fleet)
        program = dir <> "/program.c"
        input   = dir <> "/input.psv"
        output  = dir <> "/output.psv"
        dropped = dir <> "/dropped.txt"
        chords  = Nothing
        discard = FlagUseDropFile

    liftIO (LT.writeFile program (LT.fromStrict source))

    let inputPsv = textOfFacts (wtFacts wt)
    liftIO (L.writeFile input (LT.encodeUtf8 inputPsv))

    result <- liftIO (runEitherT (seaPsvSnapshotFilePath fleet input output dropped chords discard consts))

    let
      beautiful =
        vsep . fmap (text . LT.unpack) . LT.lines

    case result of
      Left err -> do
        when (yourParents == ExpectSuccess) $ do
          liftIO . print . vsep $
            [ "*** You are a disappointment! " <> pretty (show yourParents) <> " ***"
            , "Running PSV Snapshot Failed!"
            , "Error:"
            , indent 2 . pretty $ err
            ]
          liftIO . print . vsep $
            [ "Input: "
            , indent 2 . beautiful $ inputPsv
            ]
        left err
      Right (PsvStats nf ne) -> do
        outputPsv <- liftIO $ LT.readFile output
        dropPsv <- liftIO $ LT.readFile dropped
        when (  (yourParents == ExpectSuccess && outputPsv /= expectText)
             || (yourParents == ExpectFailure)) .
          Savage.error . show . vsep $
            [ "*** You are a disappointment! " <> pretty (show yourParents) <> " ***"
            , "Running PSV Snapshot OK: " <> pretty (show ne) <> " entities, " <> pretty (show nf) <> " facts."
            , "PSV facts limit per entity attribute = " <> pretty (psvFactsLimit consts)
            , "Output types: "
            , indent 2 . vsep . fmap (\(n,t) -> pretty n <> ": " <> pretty t) . List.concat . fmap wtOutputs . wtAttributes $ wt
            , "Expected values from Core:"
            , indent 2 . pretty . Map.toList $ expectValues
            , "Expected PSV:"
            , indent 2 . beautiful $ expectText
            , "Got PSV:"
            , indent 2 . beautiful $ outputPsv
            , "Input: "
            , indent 2 . beautiful $ inputPsv
            , "Dropped: "
            , indent 2 . beautiful $ dropPsv
            ]
        pure ()


longestLine :: WellTyped -> Int
longestLine wt
  | List.null (wtFacts wt)
  = 0
  | otherwise
  = fromIntegral
  $ LT.length
  $ List.maximumBy (compare `on` LT.length)
  $ fmap (LT.intercalate "|")
  $ fmap fieldsOfFact $ wtFacts wt

textOfOutputs :: Map Entity [(OutputName, BaseValue)] -> LT.Text
textOfOutputs =
  LT.unlines . linesOfOutputs

linesOfOutputs :: Map Entity [(OutputName, BaseValue)] -> [LT.Text]
linesOfOutputs =
  let
    lineOf e (n, v) =
      case textOfOutputValue v of
        Nothing ->
          Nothing
        Just u
          | u == LT.fromStrict tombstone ->
              Nothing
          | otherwise ->
              Just . LT.intercalate "|" $
                [ LT.fromStrict . getEntity $ e
                , LT.fromStrict . outputName $ n
                , u ]
  in
    List.concat . fmap (catMaybes . (\(e, vs) -> fmap (lineOf e) vs)) . Map.toList

textOfOutputValue :: BaseValue -> Maybe LT.Text
textOfOutputValue v
 = do v' <- valueFromCore v
      t  <- renderOutputValue v'
      return $ LT.replace "\n" "\\n" $ LT.fromStrict t

textSubstitution :: LT.Text -> LT.Text
textSubstitution = LT.replace "\n" "\\n"


textOfFacts :: [WellTypedValue] -> LT.Text
textOfFacts vs =
  LT.unlines (fmap (LT.intercalate "|") (fmap fieldsOfFact vs))

fieldsOfFact :: WellTypedValue -> [LT.Text]
fieldsOfFact (WellTypedValue e a v) =
  let
    (valueText, timeText) = textsOfValue v
  in [ LT.fromStrict (getEntity e), LT.fromStrict (renderInputName a), valueText, timeText ]

textsOfValue :: AsAt BaseValue -> (LT.Text, LT.Text)
textsOfValue v =
  (textOfValue (atFact v), textOfTime (atTime v))

textOfValue :: BaseValue -> LT.Text
textOfValue
 = LT.replace "\n" "\\n" -- this is the only really special character, not sure how we should deal with this
 . LT.fromStrict
 . renderValue tombstone
 . fromMaybe Tombstone
 . valueFromCore

textOfTime :: Time -> LT.Text
textOfTime = LT.fromStrict . renderTime

withSystemTempDirectory :: FilePath -> (FilePath -> EitherT SeaError IO a) -> EitherT SeaError IO a
withSystemTempDirectory template action = do
  let acquire = liftIO (getTemporaryDirectory >>= \tmp -> createTempDirectory tmp template)
      release = liftIO . removeDirectoryRecursive
  bracketEitherT' acquire release action


denseTextOfFacts :: [Entity] -> [AsAt BaseValue] -> LT.Text
denseTextOfFacts entities vs =
  LT.unlines (fmap (LT.intercalate "|") (denseFieldsOfFacts entities vs))

denseFieldsOfFacts :: [Entity] -> [AsAt BaseValue] -> [[LT.Text]]
denseFieldsOfFacts entities vs
  | Just (AsAt v t) <- sequence' vs
  , Just fs  <- sequence $ fmap (sequence . flip AsAt t . structValues) v
  =  [ [ LT.fromStrict entity, valueText, timeText ]
     | Entity entity         <- entities
     , (valueText, timeText) <- denseTextsOfValues fs ]
  | otherwise
  =  [ [ LT.fromStrict entity, valueText, timeText ]
     | Entity entity         <- entities
     , (valueText, timeText) <- fmap textsOfValue vs ]
  where
    sequence' [] = Nothing
    sequence' (AsAt x t : xs) = Just $ AsAt (x : fmap atFact xs) t
    structValues
      = \case VStruct m -> Just (Map.elems m)
              _         -> Nothing

denseTextsOfValues :: [AsAt [BaseValue]] -> [(LT.Text, LT.Text)]
denseTextsOfValues vs =
  List.zip
    (fmap (LT.intercalate "|" . fmap textOfValue . atFact) vs)
    (fmap (textOfTime . atTime) vs)

denseDictionary :: InputName -> ValType -> Gen (Maybe S.PsvInputDenseDict)
denseDictionary denseName (StructT (StructType m))
  = do missingValue <- genMissingValue
       let n         = renderInputName denseName
       fs           <- mapM (\(t,v) -> pure . (t,) . (,v) =<< arbitrary)
                            (Map.toList $ Map.mapKeys nameOfStructField m)
       return $ Just
              $ S.PsvInputDenseDict
                  (Map.singleton (renderInputName denseName) fs)
                  (maybe Map.empty (Map.singleton n) missingValue)
                  n
denseDictionary _ _ = return Nothing

genMissingValue :: Gen (Maybe Text)
genMissingValue = elements [Nothing, Just "NA", Just ""]

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 100)
