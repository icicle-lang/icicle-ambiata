{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Load (
    loadFile
  , showDictionary
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.State (modify, gets)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text.Encoding as Text

import qualified Icicle.Compiler.Source as Source
import           Icicle.Dictionary
import qualified Icicle.Internal.Pretty as Pretty
import           Icicle.Repl.Data
import           Icicle.Repl.Monad
import           Icicle.Repl.Pretty
import           Icicle.Repl.Source
import qualified Icicle.Runtime.Serial.Zebra as Runtime
import qualified Icicle.Storage.Dictionary.Toml as Toml

import           P

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.IO (FilePath)
import qualified System.IO as IO

import           X.Control.Monad.Trans.Either (runEitherT, firstJoin)

import qualified Zebra.Serial.Binary as Binary
import qualified Zebra.Table.Schema as Schema
import qualified Zebra.X.ByteStream as ByteStream


data LoadType =
    LoadDictionary
  | LoadFunctions
  | LoadData
    deriving (Eq, Ord, Show)

isToml :: FilePath -> Bool
isToml =
  (== ".toml") . FilePath.takeExtension

isIcicle :: FilePath -> Bool
isIcicle =
  (== ".icicle") . FilePath.takeExtension

detectLoadType :: MonadIO m => FilePath -> m LoadType
detectLoadType path =
  if isToml path then
    pure LoadDictionary
  else if isIcicle path then
    pure LoadFunctions
  else do
    pure LoadData

showDictionary :: Repl ()
showDictionary = do
  dictionary <- gets stateDictionary
  putSection "Dictionary" $
    prettyDictionarySummary dictionary

setDictionary :: Dictionary -> Repl ()
setDictionary dictionary = do
  liftIO . IO.putStrLn $
     "OK, loaded dictionary with " <>
     show (length $ dictionaryInputs dictionary) <> " inputs, " <>
     show (length $ dictionaryOutputs dictionary) <> " outputs, " <>
     show (length $ dictionaryFunctions dictionary) <> " functions."

  modify $ \s ->
    s { stateDictionary = dictionary }

loadDictionary :: FilePath -> Repl ()
loadDictionary path = do
  options <- getCheckOptions
  edictionary <- liftIO . runEitherT $ Toml.loadDictionary options Toml.ImplicitPrelude path
  case edictionary of
    Left err -> do
      putPretty $ Pretty.vsep [
          "Dictionary load error:"
        , Pretty.indent 2 $ Pretty.pretty err
        ]

    Right dictionary ->
      setDictionary dictionary

loadFunctions :: FilePath -> Repl ()
loadFunctions path = do
  src <- Text.decodeUtf8 <$> liftIO (ByteString.readFile path)
  loadFunctionsFrom path src

loadFunctionsFrom :: FilePath -> Text -> Repl ()
loadFunctionsFrom path src = do
  case Source.readIcicleLibrary "<interactive>" path src of
   Left err ->
     putPretty err

   Right functions0 -> do
     liftIO . IO.putStrLn $
       "OK, loaded " <> show (length functions0) <> " functions from " <> path

     dictionary <- gets stateDictionary

     -- Merge in the source file with new functions taking precedence over existing ones
     let
       functions =
         List.nubBy ((==) `on` functionName) $
           Toml.fromFunEnv functions0 <>
           dictionaryFunctions dictionary

     modify $ \s ->
       s { stateDictionary = dictionary { dictionaryFunctions = functions } }

tryLoadZebra :: MonadIO m => FilePath -> m (Maybe Schema.Table)
tryLoadZebra path =
  liftIO . fmap rightToMaybe . runResourceT . runEitherT $ do
    -- FIXME zebra should have a faster way to get the schema
    (schema, _) <-
      firstJoin show .
        Binary.decodeLogical .
      hoist (firstT show) $
        ByteStream.readFile path
    pure schema

loadFile :: FilePath -> Repl ()
loadFile path = do
  ok <- liftIO $ Directory.doesFileExist path

  if not ok then
    liftIO . IO.putStrLn $ "Failed, file does not exist: " <> path

  else do
    typ <- detectLoadType path
    case typ of
      LoadDictionary ->
        loadDictionary path

      LoadFunctions ->
        loadFunctions path

      LoadData -> do
        mschema <- tryLoadZebra path
        case mschema of
          Nothing -> do
            liftIO . IO.putStrLn $ "OK, selected psv file as input data: " <> path
            modify $ \s ->
              s { stateInput = InputPsv path }

          Just schema ->
            case Runtime.resolveDictionary schema of
              Left err ->
                putPretty (show err) -- FIXME pretty

              Right dictionary -> do
                setDictionary dictionary
                traverse_ (uncurry loadFunctionsFrom) Toml.prelude
                liftIO . IO.putStrLn $ "OK, selected zebra file as input data: " <> path
                modify $ \s ->
                  s { stateInput = InputZebra path }
