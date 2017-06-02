{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Test.Sea.PsvFission where

import           Icicle.Test.Sea.Psv hiding (tests)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Morph (hoist)

import qualified Data.ByteString.Lazy as L
import           Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT

import           Disorder.Core.IO

import           Icicle.Common.Base
import           Icicle.Common.Eval

import           Icicle.Sea.Eval (SeaError(..))
import qualified Icicle.Sea.Eval as Sea
import           Icicle.Sea.Fleet
import qualified Icicle.Core.Program.Program as C
import           Icicle.Data.Name
import           Icicle.Data

import           Icicle.Test.Arbitrary
import           Icicle.Test.Arbitrary.Corpus

import qualified Jetski as J

import           P
import qualified Prelude as Savage

import           System.IO

import           Test.QuickCheck (Arbitrary(..), forAll)
import           Test.QuickCheck (property, discard)
import           Test.QuickCheck.Property (succeeded)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither)
import           X.Control.Monad.Trans.Either (bracketEitherT', left)


prop_psv_fission =
  forAll genMaxMapSize $ \maxMapSize ->
  forAll arbitrary $ \inputType ->
  forAll (validated 100 . tryGenAttributeWithInput $ inputType) $ \wta1 ->
  forAll (validated 100 . tryGenAttributeWithInput $ inputType) $ \wta2 -> do
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
       forAll (genFactsForAttributes timeOpt [wta1]) $ \(wtvs1, chordTime) ->
       forAll (genFactsForAttributes timeOpt [wta2]) $ \(wtvs2, _) ->
         let
           evalContext =
             EvalContext chordTime maxMapSize
           wt1 =
             WellTyped evalContext wtvs1 [wta1]
         in
           forAll (genPsvConstants wt1) $ \psvOpts -> testIO $ do
             x <- runEitherT
                $ runTest2 evalContext psvOpts wta1 wtvs1 wta2 wtvs2
                $ TestOpts ShowInputOnError ShowOutputOnError Sea.PsvInputSparse timeOpt
             case x of
               Left err ->
                 failWithError wt1 err
               Right () ->
                 pure (property succeeded)

--------------------------------------------------------------------------------

timeOpt = Sea.DoNotAllowDupTime

runTest2 ::
     EvalContext
  -> Sea.PsvConstants
  -> WellTypedAttribute
  -> [WellTypedValue]
  -> WellTypedAttribute
  -> [WellTypedValue]
  -> TestOpts
  -> EitherT SeaError IO ()
runTest2 evalContext psvConstants wta1 wtvs1 wta2 wtvs2 testOpts = do
  let
    wtOf =
      WellTyped evalContext
    wt1 =
      wtOf wtvs1 [wta1]
    wt2 =
      wtOf wtvs2 [wta2]
    limit =
      Sea.psvFactsLimit psvConstants
    compile =
      compileTest2 wt1 wt2 testOpts
    release
      = Sea.seaRelease

  let
    expect1 =
      evalWellTyped wt1
    expect2 =
      evalWellTyped wt2
    expect
     | length wtvs1 < limit
     , length wtvs2 < limit
     = textOfOutputs (Map.union expect1 expect2)
     | otherwise
     = ""

  hoist runResourceT $ bracketEitherT' compile (hoist liftIO . release) $ \fleet -> hoist liftIO $ do
    let
      install =
        liftIO . Sea.sfSegvInstall fleet $ show psvConstants <> "\n" <> show wt1 <> "\n" <> show wt2
      remove _ =
        liftIO . Sea.sfSegvRemove $ fleet

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
          inputPsv =
            textOfFacts (wtFacts wt1)

        liftIO (LT.writeFile program (LT.fromStrict source))
        liftIO (L.writeFile input (LT.encodeUtf8 inputPsv))
        result <- liftIO . runEitherT $
                    Sea.seaPsvSnapshotFilePath fleet input output dropped chords Sea.FlagUseDropFile psvConstants

        case result of
          Left err -> do
            left err
          Right _ -> do
            outputPsv <- liftIO $ LT.readFile output
            when (outputPsv /= expect) $ do
              Savage.error ("Expected:\n" <> LT.unpack expect <> "\nGot:\n" <> LT.unpack outputPsv)

compileTest2 ::
     WellTyped
  -> WellTyped
  -> TestOpts
  -> EitherT SeaError (ResourceT IO) (SeaFleet Sea.PsvState)
compileTest2 wt1 wt2 (TestOpts _ _ inputFormat allowDupTime)
  | [wta1] <- wtAttributes wt1
  , [wta2] <- wtAttributes wt2
  = do
      options0 <- Sea.getCompilerOptions
      let
        optionsAssert =
          [ "-DICICLE_ASSERT=1"
          , "-DICICLE_ASSERT_MAXIMUM_ARRAY_COUNT=" <> T.pack (show (100 * (length $ wtFacts wt1))) ]

        options =
          options0 <> ["-O0", "-DICICLE_NOINLINE=1"] <> optionsAssert

        programs =
          Map.singleton (Attribute "huge") (wtAvalancheFlat wta1 :| [wtAvalancheFlat wta2])

        time =
          evalSnapshotTime (wtEvalContext wt1)

        iconfig  =
          Sea.PsvInputConfig
            (Sea.Snapshot time)
            inputFormat

        oconfig =
          Sea.PsvOutputConfig
            (Sea.Snapshot time)
            (Sea.PsvOutputSparse)
            Sea.defaultOutputMissing

        conf     = Sea.PsvConfig iconfig oconfig
        iformat  = Sea.FormatPsv conf
        attr     = wtAttribute wta1
        iopts    = Sea.InputOpts allowDupTime (Map.singleton attr (Set.singleton tombstone))
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

      let input = HasInput iformat iopts "dummy_path"
      code <- hoistEither (Sea.codeOfPrograms input [attr] (Map.toList programs))
      Sea.seaCreateFleet options (Sea.fromCacheSea Sea.NoCacheSea) input Nothing (code <> piano)
  | otherwise =
      Savage.error "Impossible! PSV Fission tests should generate single-attribute programs"


return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
