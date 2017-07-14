{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Command.Compile (
    Compile(..)

  , Fingerprint(..)
  , MaximumQueriesPerKernel(..)
  , InputDictionaryToml(..)
  , OutputDictionarySea(..)

  , icicleCompile

  , CompileError(..)
  , renderCompileError
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as ByteString
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map (Map)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Icicle.Avalanche.Prim.Flat as Avalanche
import qualified Icicle.Avalanche.Program as Avalanche
import           Icicle.Common.Annot (Annot)
import qualified Icicle.Compiler as Compiler
import qualified Icicle.Compiler.Source as Source
import           Icicle.Dictionary.Data (Dictionary)
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
     Dictionary
  -> MaximumQueriesPerKernel
  -> Either CompileError (Map InputId (NonEmpty (Avalanche.Program (Annot Source.AnnotUnit) Source.Var Avalanche.Prim)))
compileDictionary dictionary maxQueries =
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
    first CompileDictionaryError $
      Compiler.avalancheOfDictionary options dictionary

writeSeaDictionary :: OutputDictionarySea -> Runtime.SeaContext -> IO ()
writeSeaDictionary (OutputDictionarySea path) context =
  ByteString.writeFile path . Text.encodeUtf8 $
    Runtime.seaCode context

icicleCompile :: Compile -> EitherT CompileError IO ()
icicleCompile compile = do
  dictionary <-
    loadDictionary (compileInputDictionary compile)

  avalanche <-
    hoistEither $
      compileDictionary dictionary (compileMaximumQueriesPerKernel compile)

  sea <-
    hoistEither . first CompileAvalancheError . Runtime.compileAvalanche $
      Runtime.AvalancheContext (compileFingerprint compile) avalanche

  liftIO $ writeSeaDictionary (compileOutputDictionary compile) sea
