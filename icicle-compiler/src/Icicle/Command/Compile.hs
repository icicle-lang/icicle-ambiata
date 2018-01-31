{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Command.Compile (
    Compile(..)
  , Check(..)

  , Fingerprint(..)
  , MaximumQueriesPerKernel(..)
  , InputDictionaryToml(..)
  , OutputDictionarySea(..)

  , icicleCompile
  , icicleCheck

  , CompileError(..)
  , renderCompileError
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as ByteString
import qualified Data.IORef as IORef
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Icicle.Avalanche.Prim.Flat as Avalanche
import qualified Icicle.Avalanche.Program as Avalanche
import           Icicle.Command.Timer
import           Icicle.Common.Annot (Annot)
import qualified Icicle.Compiler as Compiler
import qualified Icicle.Compiler.Source as Source
import           Icicle.Dictionary.Data (Dictionary, prettyDictionarySummary)
import qualified Icicle.Internal.Pretty as Pretty
import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Evaluator as Runtime
import           Icicle.Sea.Header
import qualified Icicle.Storage.Dictionary.Toml as Toml

import           P

import           System.IO (IO, FilePath)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)


data Compile =
  Compile {
      compileFingerprint :: !Fingerprint
    , compileMaximumQueriesPerKernel :: !MaximumQueriesPerKernel
    , compileInputDictionary :: !InputDictionaryToml
    , compileOutputDictionary :: !OutputDictionarySea
    } deriving (Eq, Ord, Show)

data Check =
  Check {
      checkInputDictionary :: !InputDictionaryToml
    } deriving (Eq, Ord, Show)

newtype InputDictionaryToml =
  InputDictionaryToml {
      unInputDictionaryToml :: FilePath
    } deriving (Eq, Ord, Show)

newtype OutputDictionarySea =
  OutputDictionarySea {
      unOutputDictionarySea :: FilePath
    } deriving (Eq, Ord, Show)

newtype MaximumQueriesPerKernel =
  MaximumQueriesPerKernel {
      unMaximumQueriesPerKernel :: Int
    } deriving (Eq, Ord, Show)

data CompileError =
    CompileLoadDictionaryError !Toml.DictionaryImportError
  | CompileDictionaryError !(Compiler.ErrorCompile Source.Var)
  | CompileAvalancheError !Runtime.RuntimeError
    deriving (Show)

renderCompileError :: CompileError -> Text
renderCompileError = \case
  CompileLoadDictionaryError x ->
    "Failed to load dictionary: " <> Text.pack (show (Pretty.pretty x))
  CompileDictionaryError x ->
    "Failed to compile dictionary: " <> Text.pack (show (Pretty.pretty x))
  CompileAvalancheError x ->
    "Failed to compile avalanche: " <> Runtime.renderRuntimeError x

loadDictionary :: InputDictionaryToml -> EitherT CompileError IO Dictionary
loadDictionary (InputDictionaryToml path) = do
  firstT CompileLoadDictionaryError $
    Toml.loadDictionary Source.defaultCheckOptions Toml.ImplicitPrelude path

compileDictionary ::
     Monad m
  => (Compiler.CompilationStatus -> m ())
  -> Dictionary
  -> MaximumQueriesPerKernel
  -> EitherT CompileError m (Map InputId (NonEmpty (Avalanche.Program (Annot Source.AnnotUnit) Source.Var Avalanche.Prim)))
compileDictionary updateUI dictionary maxQueries =
  let
    fusion =
      Source.FusionOptions {
          Source.fusionMaximumPerKernel =
            unMaximumQueriesPerKernel maxQueries
        }

    options =
      Source.defaultCompileOptions {
          Source.icicleFusionOptions =
            fusion
        }
  in
    firstT CompileDictionaryError $
      Compiler.avalancheOfDictionaryM updateUI options dictionary

writeSeaDictionary :: OutputDictionarySea -> Runtime.SeaContext -> IO ()
writeSeaDictionary (OutputDictionarySea path) context =
  ByteString.writeFile path . Text.encodeUtf8 $
    Runtime.seaCode context

icicleCompile :: Compile -> EitherT CompileError IO ()
icicleCompile compile = do
  finishAll <- startTimer_ "Compiling TOML -> C"

  finishDictionary <- startTimer_ "Parsing TOML"
  dictionary <- loadDictionary (compileInputDictionary compile)
  finishDictionary

  ref <- liftIO . IORef.newIORef $ pure ()

  let
    updateUI = \case
      Compiler.CompileBegin Compiler.PhaseSourceToCore ->
        liftIO . IORef.writeIORef ref =<< startTimer_ "Compiling Source -> Core"
      Compiler.CompileBegin Compiler.PhaseFuseCore ->
        liftIO . IORef.writeIORef ref =<< startTimer_ "Fusing Compute Kernels"
      Compiler.CompileBegin Compiler.PhaseCoreToAvalanche ->
        liftIO . IORef.writeIORef ref =<< startTimer_ "Compiling Core -> Avalanche"
      Compiler.CompileEnd _ -> do
        end <- liftIO $ IORef.readIORef ref
        end

  avalanche <- compileDictionary updateUI dictionary (compileMaximumQueriesPerKernel compile)

  finishSea <- startTimer_ "Compiling Avalanche -> C"
  sea <-
    hoistEither . first CompileAvalancheError . Runtime.compileAvalanche $
      Runtime.AvalancheContext (compileFingerprint compile) avalanche
  finishSea

  liftIO $ writeSeaDictionary (compileOutputDictionary compile) sea

  finishAll

----------------
-- Check Only --
----------------

icicleCheck :: Check -> EitherT CompileError IO ()
icicleCheck check = do
  dictionary <- loadDictionary (checkInputDictionary check)
  let
    summary
      = prettyDictionarySummary dictionary

  liftIO $ Pretty.putDoc summary
