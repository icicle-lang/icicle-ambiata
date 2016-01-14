{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Psv where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy as L
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT

import           Disorder.Core.IO

import           Icicle.Data (Entity(..), Attribute(..), AsAt(..), Value(..))
import           Icicle.Data.Time (Time, renderTime)
import           Icicle.Encoding (renderValue)
import           Icicle.Internal.Pretty

import           Icicle.Common.Data
import           Icicle.Common.Base

import qualified Icicle.Sea.Eval as S

import           Icicle.Test.Sea.Arbitrary

import qualified Jetski as J

import           P

import           System.IO
import           System.IO.Temp (createTempDirectory)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)

import           Test.QuickCheck hiding (output)
import           Test.QuickCheck.Property hiding (result)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)
import           X.Control.Monad.Trans.Either (bracketEitherT', left)


prop_psv wt = testIO $ do
  x <- runEitherT (runTest ShowDataOnError wt)
  case x of
    Left err -> failWithError wt err
    Right () -> pure (property succeeded)

prop_entity_out_of_order wt =
  List.length (wtEntities wt) > 1 ==>
  List.length (wtFacts    wt) > 0 ==>
  testIO $ do
    let wt' = wt { wtEntities = List.reverse (wtEntities wt) }
    x <- runEitherT (runTest ShowDataOnSuccess wt')
    expectPsvError wt' x

prop_time_out_of_order wt =
  List.length (wtEntities wt) > 0 ==>
  List.length (wtFacts    wt) > 1 ==>
  testIO $ do
    let wt' = wt { wtFacts = List.reverse (wtFacts wt) }
    x <- runEitherT (runTest ShowDataOnSuccess wt')
    expectPsvError wt' x

expectPsvError :: WellTyped -> Either S.SeaError () -> IO Property
expectPsvError wt = \case
  Left (S.SeaPsvError msg)
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

failWithError :: WellTyped -> S.SeaError -> IO Property
failWithError wt = \case
  S.SeaJetskiError (J.CompilerError _ src err)
   -> pure
    $ counterexample (show (pretty src))
    $ counterexample (show (pretty err))
    $ counterexample (show (pretty (wtCore wt)))
    $ failed

  err
   -> pure
    $ counterexample (show (pretty err))
    $ counterexample (show (pretty (wtCore wt)))
    $ failed

------------------------------------------------------------------------

data ShowData = ShowDataOnError | ShowDataOnSuccess
  deriving (Eq)

runTest :: ShowData -> WellTyped -> EitherT S.SeaError IO ()
runTest showData wt = do
  let options  = S.compilerOptions <> ["-O0", "-DICICLE_NOINLINE=1"]
      programs = Map.singleton (wtAttribute wt) (wtAvalanche wt)
      config   = S.PsvConfig (S.PsvSnapshot (wtTime wt))
                             (Map.singleton (wtAttribute wt) (Set.singleton tombstone))

  let compile  = S.seaCompile' options (S.Psv config) programs
      release  = S.seaRelease
  bracketEitherT' compile release $ \fleet -> do

  let install  = liftIO (S.sfSegvInstall fleet (show wt))
      remove _ = liftIO (S.sfSegvRemove  fleet)
  bracketEitherT' install remove  $ \() -> do

  withSystemTempDirectory "psv-test-" $ \dir -> do
    let source  = J.libSource (S.sfLibrary fleet)
        program = dir <> "/program.c"
        input   = dir <> "/input.psv"
        output  = dir <> "/output.psv"
        chords  = Nothing

    liftIO (LT.writeFile program (LT.fromStrict source))

    let inputPsv = textOfFacts (wtEntities wt) (wtAttribute wt) (wtFacts wt)
    liftIO (L.writeFile input (LT.encodeUtf8 inputPsv))

    result <- liftIO (runEitherT (S.seaPsvSnapshotFilePath fleet input output chords))

    case result of
      Left err -> do
        when (showData == ShowDataOnError) $ do
          liftIO (LT.putStrLn "--- input.psv ---")
          liftIO (LT.putStrLn inputPsv)
        left err
      Right _ -> do
        when (showData == ShowDataOnSuccess) $ do
          liftIO (LT.putStrLn "--- input.psv ---")
          liftIO (LT.putStrLn inputPsv)
        pure ()

textOfFacts :: [Entity] -> Attribute -> [AsAt BaseValue] -> LT.Text
textOfFacts entities attribute vs =
  LT.unlines (fmap (LT.intercalate "|") (fieldsOfFacts entities attribute vs))

fieldsOfFacts :: [Entity] -> Attribute -> [AsAt BaseValue] -> [[LT.Text]]
fieldsOfFacts entities (Attribute attribute) vs =
  [ [ LT.fromStrict entity, LT.fromStrict attribute, valueText, timeText ]
  | Entity entity         <- entities
  , (valueText, timeText) <- textsOfValues vs ]

textsOfValues :: [AsAt BaseValue] -> [(LT.Text, LT.Text)]
textsOfValues vs =
  List.zip (fmap (\v -> textOfValue (atFact v)) vs)
           (fmap (\v -> textOfTime  (atTime v)) vs)

textOfValue :: BaseValue -> LT.Text
textOfValue
 = LT.replace "\n" "\\n" -- this is the only really special character, not sure how we should deal with this
 . LT.fromStrict
 . renderValue tombstone
 . fromMaybe Tombstone
 . valueFromCore

textOfTime :: Time -> LT.Text
textOfTime = LT.fromStrict . renderTime

tombstone :: Text
tombstone = "💀"

withSystemTempDirectory :: FilePath -> (FilePath -> EitherT S.SeaError IO a) -> EitherT S.SeaError IO a
withSystemTempDirectory template action = do
  let acquire = liftIO (getTemporaryDirectory >>= \tmp -> createTempDirectory tmp template)
      release = liftIO . removeDirectoryRecursive
  bracketEitherT' acquire release action

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})
