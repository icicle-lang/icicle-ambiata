{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Sea.Psv where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either

import qualified Data.ByteString.Lazy as L
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT

import           Disorder.Core.IO

import           Icicle.Data (Entity(..), Attribute(..), AsAt(..), Value(..))
import           Icicle.Data.DateTime (DateTime, renderDate)
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

import           X.Control.Monad.Catch (bracketEitherT')


prop_psv wt = testIO $ do
  x <- runEitherT (runTest wt)
  case x of
    Right ()
     -> pure (property succeeded)
    Left (S.SeaJetskiError (J.CompilerError _ src err))
     -> pure
      $ counterexample (show $ pretty src)
      $ counterexample (show $ pretty err)
      $ counterexample (show $ pretty (wtCore wt))
      $ failed
    Left err
     -> pure
      $ counterexample (show $ pretty err)
      $ counterexample (show $ pretty (wtCore wt))
      $ failed

------------------------------------------------------------------------

runTest :: WellTyped -> EitherT S.SeaError IO ()
runTest wt = do
  let programs = Map.singleton (wtAttribute wt) (wtAvalanche wt)
      config   = S.PsvConfig (wtDateTime wt) (Map.singleton (wtAttribute wt) (Set.singleton tombstone))

  bracketEitherT' (S.seaCompile (S.Psv config) programs) S.seaRelease $ \fleet -> do
  withSystemTempDirectory "psv-test-" $ \dir -> do

    let source  = J.libSource (S.sfLibrary fleet)
        program = dir <> "/program.c"
        input   = dir <> "/input.psv"
        output  = dir <> "/output.psv"

    liftIO (LT.writeFile program (LT.fromStrict source))

    let inputPsv = textOfFacts (wtEntities wt) (wtAttribute wt) (wtFacts wt)
    liftIO (L.writeFile input (LT.encodeUtf8 inputPsv))

    result <- liftIO (runEitherT (S.seaPsvSnapshotFilePath fleet input output))
    case result of
      Right _ -> pure ()
      Left  _ -> do
        liftIO (LT.putStrLn "--- input.psv ---")
        liftIO (LT.putStrLn inputPsv)

    hoistEither result

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
  List.zip (fmap (\v -> textOfValue (fact v)) vs)
           (fmap (\v -> textOfTime  (time v)) vs)

textOfValue :: BaseValue -> LT.Text
textOfValue
 = LT.replace "\n" "\\n" -- this is the only really special character, not sure how we should deal with this
 . LT.fromStrict
 . renderValue tombstone
 . fromMaybe Tombstone
 . valueFromCore

textOfTime :: DateTime -> LT.Text
textOfTime = LT.fromStrict . renderDate

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
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 10, maxDiscardRatio = 10000})
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})
