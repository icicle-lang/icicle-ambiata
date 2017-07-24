{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Test.Sea.PsvFission where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (hoist)
import           Control.Arrow ((&&&))

import qualified Data.ByteString.Lazy as L
import           Data.List.NonEmpty ( NonEmpty(..) )
import           Data.Maybe
import           Data.String
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT

import qualified Prelude as Savage

import           System.IO

import           P

import           Test.QuickCheck (forAll, discard)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither)
import           X.Control.Monad.Trans.Either (bracketEitherT', firstEitherT, left)

import           Disorder.Core.IO

import qualified Jetski as J

import           Icicle.Internal.Pretty
import           Icicle.Common.Eval
import           Icicle.Sea.Eval (SeaError(..))
import qualified Icicle.Sea.Eval as Sea
import           Icicle.Sea.Data
import           Icicle.Sea.Fleet
import qualified Icicle.Core.Program.Program as C
import           Icicle.Data.Name
import           Icicle.Data
import           Icicle.Test.Arbitrary
import qualified Icicle.Avalanche.Prim.Flat as Flat
import qualified Icicle.Avalanche.Program as Flat
import           Icicle.Common.Annot

import           Icicle.Test.Sea.Psv.Base


prop_psv_fission =
  forAll genSumErrorFactType $ \inputType ->
  forAll (validated tryCountInputType $ tryGenAttributeWithInput inputType) $ \wta1 ->
  forAll (validated tryCountInputType $ tryGenAttributeWithInput inputType) $ \wta2 -> do
    let
      retOf =
        fmap (outputName . fst) . C.returns . wtCore
      ret1 =
        retOf wta1
      ret2 =
        retOf wta2
      intersect =
        any (flip elem ret1) ret2
    case intersect of
     True ->
       discard
     False ->
       forAll (genEAVTs timeOpt [wta1, wta2]) $ \vals ->
         let
           -- sort the facts by time because we are going to run them with
           -- different attributes separately, but they are only guaranteed
           -- to be sorted within one attribute.
           wtvs =
             let
               sortByEAT =
                 List.sortBy (comparing (eavtEntity &&& eavtInputName &&& (atTime . eavtValue)))
               forAttribute1 w =
                  w { eavtInputName = inputName . clusterInputId . wtCluster $ wta1 }
               forAttribute2 w =
                  w { eavtInputName = inputName . clusterInputId . wtCluster $ wta2 }
             in
               sortByEAT $ fmap forAttribute1 vals <> fmap forAttribute2 vals
           dummyWt =
             WellTyped wtvs [wta1]
         in
           forAll (genPsvConstants dummyWt) $ \psvOpts ->
             testIO . testEitherT . firstEitherT show $
               runTwoAsOne psvOpts wta1 wta2 wtvs
                 $ TestOpts ExpectSuccess Sea.PsvInputSparse timeOpt

--------------------------------------------------------------------------------

timeOpt = Sea.AllowDupTime

runTwoAsOne ::
     Sea.PsvConstants
  -> WellTypedCluster
  -> WellTypedCluster
  -> [WellTypedValue]
  -> TestOpts
  -> EitherT SeaError IO ()
runTwoAsOne constants wta1 wta2 wtvs testOpts = do
  let
    -- if dropping occurs, we cannot use the same facts limit for the combined program
    -- and we cannot just double the facts limit (the entities dropped will still be
    -- different) so we just ignore dropping in this test.
    factsLimit =
      1000000000
    psvConstants =
      constants { Sea.psvFactsLimit = factsLimit }
    evalContext =
      wellTypedEvalContext factsLimit (Sea.psvMaxMapSize psvConstants)
    compile =
      compileTwoAsOne
        testOpts
        evalContext
        (clusterInputId . wtCluster $ wta1)
        (wtAvalancheFlat wta1)
        (wtAvalancheFlat wta2)
    release
      = Sea.seaRelease
    inputPsv =
      textOfFacts wtvs

  -- Same input, different attributes (of the same type).
  let
    expect1 =
      evalWellTyped evalContext . WellTyped wtvs $ [wta1]
    expect2 =
      evalWellTyped evalContext . WellTyped wtvs $ [wta2]
    expect =
      fmap (List.sortBy (comparing fst)) $
      Map.unionWith (<>) expect1 expect2
    expectPsv =
      textOfOutputs expect

  bracketEitherT' compile (hoist liftIO . release) $ \fleet -> hoist liftIO $ do
    let
      install =
        liftIO $ Sea.sfSegvInstall fleet ""
      remove _ =
        liftIO $ Sea.sfSegvRemove fleet

    bracketEitherT' install remove  $ \_ -> do
      withSystemTempDirectory "psv-test-" $ \dir -> do
        let
          source =
            J.libSource (Sea.sfLibrary fleet)
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
          beautiful =
            vsep . fmap (text . LT.unpack) . LT.lines

        liftIO (LT.writeFile program (LT.fromStrict source))
        liftIO (L.writeFile input (LT.encodeUtf8 inputPsv))
        result <-
          liftIO .
          runEitherT .
          Sea.seaPsvSnapshotFilePath fleet input output dropped chords Sea.FlagUseDropFile $
            psvConstants

        case result of
          Left err -> do
            left err
          Right (Sea.PsvStats nf ne) -> do
            outputPsv <- liftIO $ LT.readFile output
            dropPsv <- liftIO $ LT.readFile dropped
            when (outputPsv /= expectPsv) $ do
              Savage.error . show . vsep $
                [ "*** You are a disappointment! ***"
                , "Running PSV Snapshot OK: " <> pretty (show ne) <> " entities, " <> pretty (show nf) <> " facts"
                , "PSV facts limit = " <> pretty (Sea.psvFactsLimit psvConstants)
                , "----------------------------------------"
                , "Expected combined PSV:"
                , indent 2 . beautiful $ expectPsv
                , "----------------------------------------"
                , "Expect from evaluating WellTyped 1:"
                , indent 2 . text . show . Map.toList $ expect1
                , "----------------------------------------"
                , "Expect from evaluating WellTyped 2:"
                , indent 2 . text . show . Map.toList $ expect2
                , "----------------------------------------"
                , "Got PSV:"
                , indent 2 . beautiful $ outputPsv
                , "----------------------------------------"
                , "Input: "
                , indent 2 . beautiful $ inputPsv
                , "----------------------------------------"
                , "Dropped: "
                , indent 2 . beautiful $ dropPsv
                , "----------------------------------------"
                ]

compileTwoAsOne ::
     TestOpts
  -> WellTypedEval
  -> InputId
  -> Flat.Program (Annot ()) Var Flat.Prim
  -> Flat.Program (Annot ()) Var Flat.Prim
  -> EitherT SeaError IO (SeaFleet Sea.PsvState)
compileTwoAsOne (TestOpts _ inputFormat allowDupTime) evalContext theAttribute program1 program2 = do
   defaultOptions <- Sea.getCompilerOptions

   let
     options =
       [ "-DICICLE_ASSERT=1"
       , "-DICICLE_ASSERT_MAXIMUM_ARRAY_COUNT=1000000000"
       , "-O0"
       , "-DICICLE_NOINLINE=1"
       ] <> defaultOptions

     theProgram =
       (theAttribute, program1 :| [program2])

     theTime =
       evalSnapshotTime . wtEvalContext $ evalContext

     theInput =
       HasInput
         (Sea.FormatPsv
            (Sea.PsvConfig
               (Sea.PsvInputConfig
                  (Sea.Snapshot theTime)
                  inputFormat
               )
               (Sea.PsvOutputConfig
                  (Sea.Snapshot theTime)
                  Sea.PsvOutputSparse
                  Sea.defaultOutputMissing
               )
            )
         )
         (Sea.InputOpts
            allowDupTime .
            Map.fromList $
             [(theAttribute, Set.singleton tombstone)]
         )
         ("" :: String)

     -- psv now uses piano, so we need this trick for testing.
     piano = T.concat
       [ "int64_t piano_max_count (piano_t *piano) {"
       , "    return 1;"
       , "}"
       , ""
       , "error_t piano_lookup (piano_t *piano, const uint8_t *needle_id, size_t needle_id_size, int64_t *out_count, const int64_t **out_label_times, const int64_t **out_label_name_offsets, const int64_t **out_label_name_lengths, const uint8_t **out_label_name_data) {"
       , "    return 0;"
       , "}"
       ]

   code <- hoistEither $ Sea.codeOfPrograms "Icicle.Test.Sea.PsvFission.compileTwoAsOne" theInput [theProgram]
   Sea.seaCreateFleet options (Sea.fromUseJetskiCache Sea.UseJetskiCache) theInput Nothing (code <> piano)


return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
