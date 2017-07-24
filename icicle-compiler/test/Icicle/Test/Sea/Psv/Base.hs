{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Icicle.Test.Sea.Psv.Base where

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
import           Icicle.Sea.Data

import           Icicle.Test.Arbitrary
import           Icicle.Test.Sea.Utils

import qualified Jetski as J

import           P

import           System.IO

import           Test.QuickCheck (Gen, Property, property, counterexample)
import           Test.QuickCheck.Property (succeeded, failed)

import           X.Control.Monad.Trans.Either (left)
import           X.Control.Monad.Trans.Either (EitherT, hoistEither, firstEitherT, bracketEitherT', runEitherT)


optExpectSuccess :: InputAllowDupTime -> TestOpts
optExpectSuccess =
  TestOpts ExpectSuccess PsvInputSparse

optExpectFailure :: InputAllowDupTime -> TestOpts
optExpectFailure =
  TestOpts ExpectFailure PsvInputSparse

testForSuccess :: InputAllowDupTime -> WellTyped -> PsvConstants -> Property
testForSuccess dup wt psv =
  testIO . testEitherT . runTest wt psv . optExpectSuccess $ dup

testEitherT :: EitherT String IO () -> IO Property
testEitherT e = do
  x <- runEitherT e
  case x of
    Left str ->
      pure . counterexample str . property $ failed
    Right _ ->
      pure . property $ succeeded

genWT1 :: InputAllowDupTime -> SumErrorFactT -> Gen WellTyped
genWT1 d x =
  -- if this welltyped is discarded, we can try again
  validated tryCountInputType $
    tryGenWellTypedWithInput d x

genWT2 :: InputAllowDupTime -> SumErrorFactT -> ValType-> Gen WellTyped
genWT2 d x y =
  -- if this welltyped is discarded, the output type is no good
  -- there is no point in trying again
  validated tryCountOutputType $
    tryGenWellTypedWithInputAndOutput d x y

genWTA1 :: SumErrorFactT -> Gen WellTypedCluster
genWTA1 x =
  validated tryCountInputType $
    tryGenAttributeWithInput x

wtEntities :: WellTyped -> [Entity]
wtEntities =
  List.nub . fmap eavtEntity . wtFacts

------------------------------------------------------------------------

data Expect = ExpectSuccess | ExpectFailure
  deriving (Eq, Show)

data TestOpts = TestOpts Expect PsvInputFormat InputAllowDupTime
  deriving (Eq, Show)

--
-- FIXME
-- We wouldn't need WellTypedEval to compile the test if the snapshot time
-- wasn't rolled into the C code.
--
compileTest :: WellTypedEval -> WellTyped -> TestOpts -> EitherT SeaError  IO (SeaFleet PsvState)
compileTest wte wt (TestOpts _ inputFormat allowDupTime) = do
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
      evalSnapshotTime . wtEvalContext $ wte

    missingValuesFor wta =
      (clusterInputId . wtCluster $ wta, Set.singleton tombstone)

    hasInput =
      HasInput
        (FormatPsv
           (PsvConfig
              (PsvInputConfig
                 (Snapshot time)
                 inputFormat
              )
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
           wtClusters $
             wt
        )
        ("" :: String)

    programFrom w =
      (clusterInputId . wtCluster $ w, wtAvalancheFlat w :| [])

    programs =
      fmap programFrom . wtClusters $ wt

    chords =
      Nothing

    cache =
      SkipJetskiCache

  code <- hoistEither $ codeOfPrograms "Icicle.Test.Sea.Psv.compileTest" hasInput programs

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

  seaCreateFleet options (fromUseJetskiCache cache) hasInput chords textCode

runTest :: WellTyped -> PsvConstants -> TestOpts -> EitherT String IO ()
runTest wt consts testOpts@(TestOpts yourParents _ _) = do
  let
    evalCtx =
      wellTypedEvalContext (psvFactsLimit consts) (psvMaxMapSize consts)
  bracketEitherT'
    (firstEitherT show $ compileTest evalCtx wt testOpts)
    (hoist liftIO . seaRelease)
    (runCompiledTest yourParents wt consts)

runCompiledTest :: Expect -> WellTyped -> PsvConstants -> SeaFleet PsvState -> EitherT String IO ()
runCompiledTest expect wt consts fleet =
  bracketEitherT'
    (liftIO $ sfSegvInstall fleet "")
    (\_ -> liftIO $ sfSegvRemove fleet)
    (\_ -> runTestWith expect wt consts fleet)

runTestWith :: Expect -> WellTyped -> PsvConstants -> SeaFleet PsvState -> EitherT String IO ()
runTestWith yourParents wt consts fleet =
  withSystemTempDirectory "psv-test-" $ \dir -> do
    let
      expectValues =
        evalWellTyped (wellTypedEvalContext (psvFactsLimit consts) (psvMaxMapSize consts)) wt
      expectText =
       textOfOutputs expectValues
      source =
        J.libSource (sfLibrary fleet)
      program =
        dir <> "/program.c"
      input =
        dir <> "/input.psv"
      output =
        dir <> "/output.psv"
      dropped =
        dir <> "/dropped.txt"
      chords =
        Nothing
      discard =
        FlagUseDropFile
      inputPsv =
        textOfFacts (wtFacts wt)

    result <- liftIO $ do
      LT.writeFile program . LT.fromStrict $ source
      L.writeFile input . LT.encodeUtf8 $ inputPsv
      runEitherT $ seaPsvSnapshotFilePath fleet input output dropped chords discard consts

    let
      beautiful =
        vsep . fmap (text . LT.unpack) . LT.lines

    case result of
      Left err ->
        if (yourParents == ExpectSuccess)
        then
          left . show . vsep $
            [ "*** You are a disappointment! " <> pretty (show yourParents) <> " ***"
            , "Running PSV Snapshot Failed!"
            , "Error:"
            , indent 2 . pretty $ err
            , "Input: "
            , indent 2 . beautiful $ inputPsv
            ]
        else
          pure ()

      Right (PsvStats nf ne) -> do
        outputPsv <- liftIO $ LT.readFile output
        dropPsv <- liftIO $ LT.readFile dropped
        if (  (yourParents == ExpectSuccess && outputPsv /= expectText)
           || (yourParents == ExpectFailure))
        then
          left . show . vsep $
            [ "*** You are a disappointment! " <> pretty (show yourParents) <> " ***"
            , "Running PSV Snapshot OK: " <> pretty (show ne) <> " entities, " <> pretty (show nf) <> " facts"
            , "PSV facts limit = " <> pretty (psvFactsLimit consts)
            , "----------------------------------------"
            , "Output types: "
            , indent 2 . vsep . fmap (\(n,t) -> pretty n <> " :: " <> pretty t) . List.concat . fmap wtOutputs . wtClusters $ wt
            , "----------------------------------------"
            , "Expected values from Core:"
            , indent 2 . pretty . Map.toList $ expectValues
            , "----------------------------------------"
            , "Expected PSV:"
            , indent 2 . beautiful $ expectText
            , "----------------------------------------"
            , "Got PSV:"
            , indent 2 . beautiful $ outputPsv
            , "----------------------------------------"
            , "Input: "
            , indent 2 . beautiful $ inputPsv
            , "----------------------------------------"
            , "Dropped: "
            , indent 2 . beautiful $ dropPsv
            -- , "----------------------------------------"
            -- , "Sea:"
            -- , indent 2 . beautiful . LT.fromStrict $ source
            ]
        else
          pure ()
