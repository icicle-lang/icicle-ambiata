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
import           Icicle.Common.Type

import qualified Icicle.Sea.Eval as S

import           Icicle.Test.Sea.Arbitrary

import qualified Jetski as J

import           P

import           System.IO
import           System.IO.Temp (createTempDirectory)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)

import           Test.QuickCheck (Args(..), Gen, forAllProperties, quickCheckWithResult
                                 ,stdArgs, arbitrary)
import           Test.QuickCheck (Property, (==>), property, counterexample)
import           Test.QuickCheck.Property (succeeded, failed)
import           Test.QuickCheck.Monadic

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)
import           X.Control.Monad.Trans.Either (bracketEitherT', left)

prop_psv wt = testIO $ do
  x <- runEitherT (runTest (TestOpts ShowInputOnError ShowOutputOnError S.PsvInputSparse) wt)
  case x of
    Left err -> failWithError wt err
    Right () -> pure (property succeeded)

prop_entity_out_of_order wt =
  List.length (wtEntities wt) > 1 ==>
  List.length (wtFacts    wt) > 0 ==>
  testIO $ do
    let wt' = wt { wtEntities = List.reverse (wtEntities wt) }
    x <- runEitherT (runTest (TestOpts ShowInputOnSuccess ShowOutputOnSuccess S.PsvInputSparse) wt')
    expectPsvError wt' x

prop_time_out_of_order wt =
  List.length (wtEntities wt) > 0 ==>
  List.length (wtFacts    wt) > 1 ==>
  testIO $ do
    let wt' = wt { wtFacts = List.reverse (wtFacts wt) }
    x <- runEitherT (runTest (TestOpts ShowInputOnSuccess ShowOutputOnSuccess S.PsvInputSparse) wt')
    expectPsvError wt' x

prop_sparse_dense_both_compile
  = monadicIO
  $ do wt <- pick genWellTypedWithStruct
       case denseDictionary (wtAttribute wt) (wtFactType wt) of
         Nothing -> pure
                  $ counterexample ("Cannot create dense dictionary for:")
                  $ counterexample (show (wtFactType wt))
                  $ failed
         Just d  -> do
           e <- liftIO $ runEitherT (runTest (TestOpts ShowInputOnError ShowOutputOnError (S.PsvInputDense d)) wt)
           s <- liftIO $ runEitherT (runTest (TestOpts ShowInputOnError ShowOutputOnError S.PsvInputSparse) wt)
           case (s, e) of
             (Right _, Right _) -> pure (property succeeded)
             (Left err, _)      -> liftIO
                                 $ failWithError'
                                   (counterexample ("==* sparse failed!")) wt err
             (_, Left err)      -> liftIO
                                 $ failWithError'
                                   (counterexample ("==* dense failed!")) wt err

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
failWithError = failWithError' id

failWithError' :: (Property -> Property) -> WellTyped -> S.SeaError -> IO Property
failWithError' prints wt = \case
  S.SeaJetskiError (J.CompilerError _ src err)
   -> pure
    $ prints
    $ counterexample (show (pretty src))
    $ counterexample (show (pretty err))
    $ counterexample (show (pretty (wtCore wt)))
    $ failed

  err
   -> pure
    $ prints
    $ counterexample (show (pretty err))
    $ counterexample (show (pretty (wtCore wt)))
    $ failed

------------------------------------------------------------------------

data ShowInput = ShowInputOnError | ShowInputOnSuccess
  deriving (Eq)

data ShowOutput = ShowOutputOnError | ShowOutputOnSuccess
  deriving (Eq)


data TestOpts = TestOpts ShowInput ShowOutput S.PsvInputFormat

runTest :: TestOpts -> WellTyped -> EitherT S.SeaError IO ()
runTest (TestOpts showInput showOutput inputFormat) wt = do
  options0 <- S.getCompilerOptions

  let options  = options0 <> ["-O0", "-DICICLE_NOINLINE=1"]
      programs = Map.singleton (wtAttribute wt) (wtAvalanche wt)
      iconfig  = S.PsvInputConfig
                (S.PsvSnapshot (wtTime wt))
                (Map.singleton (wtAttribute wt) (Set.singleton tombstone))
                 inputFormat
      oconfig  = S.PsvOutputConfig
                (S.PsvSnapshot (wtTime wt))
                (S.PsvOutputSparse)

  let compile  = S.seaCompile' options (S.Psv iconfig oconfig) programs
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
        when (showInput == ShowInputOnError) $ do
          liftIO (LT.putStrLn "--- input.psv ---")
          liftIO (LT.putStrLn inputPsv)
        when (showOutput == ShowOutputOnError) $ do
          outputPsv <- liftIO $ LT.readFile output
          liftIO (LT.putStrLn "--- output.psv ---")
          liftIO (LT.putStrLn outputPsv)
        left err
      Right _ -> do
        when (showInput == ShowInputOnSuccess) $ do
          liftIO (LT.putStrLn "--- input.psv ---")
          liftIO (LT.putStrLn inputPsv)
        when (showOutput == ShowOutputOnSuccess) $ do
          outputPsv <- liftIO $ LT.readFile output
          liftIO (LT.putStrLn "--- output.psv ---")
          liftIO (LT.putStrLn outputPsv)
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


-- If the input are structs, we can pretend it's a dense value
-- We can't treat other values as a single-field dense struct because the
-- generated programs do not treat them as such.

genWellTypedWithStruct :: Gen WellTyped
genWellTypedWithStruct = validated 10 $ do
  st <- arbitrary :: Gen StructType
  tryGenWellTypedWith (StructT st)

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
     , (valueText, timeText) <- textsOfValues vs ]
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

denseDictionary :: Attribute -> ValType -> Maybe S.PsvInputDenseDict
denseDictionary denseName (StructT (StructType m))
  = Just
  $ S.PsvInputDenseDict
  $ Map.singleton (getAttribute denseName)
  $ Map.toList
  $ Map.mapKeys nameOfStructField m
denseDictionary _ _ = Nothing

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 500, maxSize = 10, maxDiscardRatio = 10000})
-- tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000, maxSize = 10, maxDiscardRatio = 10000})
