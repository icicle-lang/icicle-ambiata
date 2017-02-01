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

import qualified Data.ByteString.Lazy as L
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
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

import           Icicle.Test.Arbitrary

import qualified Jetski as J

import           P

import           System.IO
import           System.IO.Temp (createTempDirectory)
import           System.Directory (getTemporaryDirectory, removeDirectoryRecursive)

import           Test.QuickCheck (Gen, Arbitrary(..), elements, suchThat)
import           Test.QuickCheck (getPositive)
import           Test.QuickCheck (Property, (==>), property, counterexample)
import           Test.QuickCheck.Property (succeeded, failed)
import           Test.QuickCheck.Monadic

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)
import           X.Control.Monad.Trans.Either (bracketEitherT', left)

prop_psv (WellTypedPsv wt psv) = testIO $ do
  x <- runEitherT
     $ runTest wt psv
     $ TestOpts ShowInputOnError ShowOutputOnError S.PsvInputSparse S.DoNotAllowDupTime
  case x of
    Left err -> failWithError wt err
    Right () -> pure (property succeeded)

prop_entity_out_of_order (WellTypedPsv wt psv) =
  List.length (wtEntities wt) > 1 ==>
  List.length (wtFacts    wt) > 0 ==>
  testIO $ do
    let wt' = wt { wtEntities = List.reverse (wtEntities wt) }
    x <- runEitherT
        $ runTest wt' psv
        $ TestOpts ShowInputOnSuccess ShowOutputOnSuccess S.PsvInputSparse S.DoNotAllowDupTime
    expectPsvError wt' x

prop_time_out_of_order (WellTypedPsv wt psv) =
  List.length (wtEntities wt) > 0 ==>
  List.length (wtFacts    wt) > 1 ==>
  testIO $ do
    let wt' = wt { wtFacts = List.reverse (wtFacts wt) }
    x <- runEitherT
       $ runTest wt' psv
       $ TestOpts ShowInputOnSuccess ShowOutputOnSuccess S.PsvInputSparse S.DoNotAllowDupTime
    expectPsvError wt' x

prop_dup_time
  = monadicIO
  $ do wt <- pick $ genWellTypedWithDuplicateTimes
                      `suchThat` (\x -> List.length (wtEntities x) > 0
                                     && List.length (wtFacts    x) > 1 )
       let wt' = wt { wtFacts = List.head (wtFacts wt) : wtFacts wt }

       psv <- pick $ genPsvOpts wt'

       a  <- liftIO
           $ runEitherT
           $ runTest wt' psv
           $ TestOpts ShowInputOnError ShowOutputOnError S.PsvInputSparse S.AllowDupTime
       b  <- liftIO
           $ runEitherT
           $ runTest wt' psv
           $ TestOpts ShowInputOnSuccess ShowOutputOnSuccess S.PsvInputSparse S.DoNotAllowDupTime
       case a of
         Left err -> stop $ testIO $ failWithError wt' err
         Right _  -> stop $ testIO $ expectPsvError wt' b

prop_sparse_dense_both_compile
  = monadicIO
  $ do wt <- pick $ genWellTypedWithStruct S.DoNotAllowDupTime
       psv <- pick $ genPsvOpts wt
       dict <- pick (denseDictionary (wtAttribute wt) (wtFactType wt))
       case dict of
         Nothing -> pure
                  $ counterexample ("Cannot create dense dictionary for:")
                  $ counterexample (show (wtFactType wt))
                  $ failed
         Just d  -> do
           e <- liftIO
              $ runEitherT
              $ runTest wt psv
              $ TestOpts ShowInputOnError
                         ShowOutputOnError
                         (S.PsvInputDense d (getAttribute (wtAttribute wt)))
                         S.DoNotAllowDupTime
           s <- liftIO
              $ runEitherT
              $ runTest wt psv
              $ TestOpts ShowInputOnError
                         ShowOutputOnError
                         S.PsvInputSparse
                         S.DoNotAllowDupTime
           case (s, e) of
             (Right _, Right _) -> pure (property succeeded)
             (Left err, _)      -> stop
                                 $ testIO
                                 $ failWithError'
                                   (counterexample ("==* sparse failed!")) wt err
             (_, Left err)      -> stop
                                 $ testIO
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
  deriving (Eq, Show)

data ShowOutput = ShowOutputOnError | ShowOutputOnSuccess
  deriving (Eq, Show)

data PsvOpts  = PsvOpts Int Int Int
  deriving (Show)

data TestOpts = TestOpts ShowInput ShowOutput S.PsvInputFormat S.InputAllowDupTime
  deriving (Show)

data WellTypedPsv = WellTypedPsv WellTyped PsvOpts
  deriving (Show)

instance Arbitrary WellTypedPsv where
  arbitrary = do
    wt <- arbitrary
    psv <- genPsvOpts wt
    return $ WellTypedPsv wt psv

genPsvOpts :: WellTyped -> Gen PsvOpts
genPsvOpts wt = do
  -- maximum number of rows to read before compute
  let row x = x + 1
  maxRowCount <- row . getPositive <$> arbitrary
  -- the buffer needs to be at least as large as a single line
  let str x = x + longestLine wt + 4
  inputBuf <- str . getPositive <$> arbitrary
  let outputBuf = inputBuf
  return $ PsvOpts maxRowCount inputBuf outputBuf

runTest :: WellTyped -> PsvOpts -> TestOpts -> EitherT S.SeaError IO ()
runTest wt (PsvOpts psvMaxRowCount psvInputBufferSize psvOutputBufferSize) (TestOpts showInput showOutput inputFormat allowDupTime) = do
  options0 <- S.getCompilerOptions

  let options  = options0 <> ["-O0", "-DICICLE_NOINLINE=1"]
      programs = Map.singleton (wtAttribute wt) (wtAvalancheFlat wt)
      iconfig  = S.PsvInputConfig
                (S.Snapshot (wtTime wt))
                 inputFormat
      oconfig  = S.PsvOutputConfig
                (S.Snapshot (wtTime wt))
                (S.PsvOutputSparse)
                (S.defaultOutputMissing)
      conf     = S.PsvConfig iconfig oconfig psvMaxRowCount psvInputBufferSize psvOutputBufferSize
      iformat  = S.FormatPsv conf
      iopts    = S.InputOpts allowDupTime (Map.singleton (wtAttribute wt) (Set.singleton tombstone))
      attrs    = [wtAttribute wt]

  let compile  = S.seaCompileFleet options S.NoCacheSea (S.HasInput iformat iopts "dummy_path") attrs programs Nothing
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
        dropped = dir <> "/dropped.txt"
        chords  = Nothing
        limit   = 4096
        discard = S.FlagUseDropFile

    liftIO (LT.writeFile program (LT.fromStrict source))

    let inputPsv = textOfFacts (wtEntities wt) (wtAttribute wt) (wtFacts wt)
    liftIO (L.writeFile input (LT.encodeUtf8 inputPsv))

    result <- liftIO (runEitherT (S.seaPsvSnapshotFilePath fleet input output dropped chords limit discard))

    case result of
      Left err -> do
        when (showInput == ShowInputOnError) $ do
          liftIO (LT.putStrLn "--- input.psv ---")
          liftIO (LT.putStrLn inputPsv)
        when (showOutput == ShowOutputOnError) $ do
          outputPsv <- liftIO $ LT.readFile output
          liftIO (LT.putStrLn "--- output.psv ---")
          liftIO (LT.putStrLn outputPsv)
          dropPsv <- liftIO $ LT.readFile dropped
          liftIO (LT.putStrLn "--- drop.txt ---")
          liftIO (LT.putStrLn dropPsv)
        left err
      Right stats -> do
        when (showInput == ShowInputOnSuccess) $ do
          liftIO (LT.putStrLn "--- input.psv ---")
          liftIO (LT.putStrLn inputPsv)
          liftIO (LT.putStrLn "--- stats ---")
          liftIO (putStrLn ("facts read:    " <> show (S.psvFactsRead stats)))
          liftIO (putStrLn ("entities read: " <> show (S.psvEntitiesRead stats) <> "\n"))
        when (showOutput == ShowOutputOnSuccess) $ do
          outputPsv <- liftIO $ LT.readFile output
          liftIO (LT.putStrLn "--- output.psv ---")
          liftIO (LT.putStrLn outputPsv)
          dropPsv <- liftIO $ LT.readFile dropped
          liftIO (LT.putStrLn "--- drop.txt ---")
          liftIO (LT.putStrLn dropPsv)
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
  $ fieldsOfFacts (wtEntities wt) (wtAttribute wt) (wtFacts wt)

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

denseDictionary :: Attribute -> ValType -> Gen (Maybe S.PsvInputDenseDict)
denseDictionary denseName (StructT (StructType m))
  = do missingValue <- genMissingValue
       let n         = getAttribute denseName
       fs           <- mapM (\(t,v) -> pure . (t,) . (,v) =<< arbitrary)
                            (Map.toList $ Map.mapKeys nameOfStructField m)
       return $ Just
              $ S.PsvInputDenseDict
                  (Map.singleton (getAttribute denseName) fs)
                  (maybe Map.empty (Map.singleton n) missingValue)
                  n
denseDictionary _ _ = return Nothing

genMissingValue :: Gen (Maybe Text)
genMissingValue = elements [Nothing, Just "NA", Just ""]

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = $checkAllWith TestRunNormal (checkArgsSized 10)
