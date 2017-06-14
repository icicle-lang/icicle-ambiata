{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.FileEmbed (embedFile)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Icicle.Compiler (ErrorCompile, avalancheOfDictionary)
import           Icicle.Compiler.Source (defaultCompileOptions)
import qualified Icicle.Compiler.Source as Source
import           Icicle.Data.Time (timeOfText)
import           Icicle.Dictionary (tombstonesOfDictionary, orderedConcreteFeaturesIn)
import           Icicle.Sea.Eval
import           Icicle.Source.Checker (optionSmallData)
import           Icicle.Storage.Dictionary.Toml (DictionaryImportError, ImplicitPrelude(..))
import           Icicle.Storage.Dictionary.Toml (loadDictionary)

import           Options.Generic

import           P

import           System.Directory
import           System.FilePath
import           System.IO

import           X.Control.Monad.Trans.Either


mainCode :: ByteString
mainCode =
  $(embedFile "data/make/main.c")

data Options =
  Options {
      dictionary :: FilePath <?> "The icicle dictionary (in .toml format)"
    , output :: FilePath <?> "The output directory"
    , date :: Text <?> "The snapshot date YYYY-MM-DD"
    } deriving (Generic, Show)

data MakeError =
    MakeDictionaryImportError !DictionaryImportError
  | MakeCompilerError !(ErrorCompile Source.Var)
  | MakeSeaError !SeaError
  | MakeInvalidDate !Text
    deriving (Generic, Show)

instance ParseRecord Options

run :: EitherT MakeError IO () -> IO ()
run io = do
  e <- runEitherT io
  case e of
    Left err ->
      print err
    Right ()  ->
      pure ()

renderArgs :: [Text] -> Text
renderArgs =
  let
    render opt =
      if " " `T.isInfixOf` opt then
        "\"" <> opt <> "\""
      else
        opt
  in
    T.unwords . fmap render

main :: IO ()
main =
  run $ do
    options <- liftIO $ getRecord "Icicle Make"

    loadedDictionary <- firstT MakeDictionaryImportError $
      loadDictionary optionSmallData ImplicitPrelude (unHelpful $ dictionary options)

    avalanche <- hoistEither . first MakeCompilerError $
      avalancheOfDictionary defaultCompileOptions loadedDictionary

    let
      attrs =
        orderedConcreteFeaturesIn loadedDictionary

    let
      dateText =
        unHelpful $ date options

    snapshotDate <- hoistMaybe (MakeInvalidDate dateText) $ timeOfText dateText

    let
      mode =
        Snapshot snapshotDate

      inputConfig =
        PsvInputConfig mode PsvInputSparse

      outputConfig =
        PsvOutputConfig mode PsvOutputSparse defaultOutputMissing

      format =
        FormatPsv (PsvConfig inputConfig outputConfig)

      inputOptions =
        InputOpts AllowDupTime $ tombstonesOfDictionary loadedDictionary

      input =
        HasInput format inputOptions ()

    code <- hoistEither . first MakeSeaError .
      codeOfPrograms input attrs $ Map.toList avalanche

    compilerOptions <- liftIO $ getCompilerOptions

    let
      dir =
        unHelpful $ output options

    liftIO $ createDirectoryIfMissing True dir

    liftIO $ T.writeFile (dir </> "lib.c") code
    liftIO $ B.writeFile (dir </> "main.c") mainCode

    liftIO . T.writeFile (dir </> "Makefile") $
      T.unlines [
          "CC=gcc"
        , "CFLAGS=" <> renderArgs compilerOptions
        , ""
        , ".PHONY: default clean"
        , ""
        , "default: icicle"
        , ""
        , "clean:"
        , "\trm -f *.o"
        , "\trm -f icicle"
        , ""
        , "lib.o: lib.c"
        , "\t$(CC) -c lib.c $(CFLAGS)"
        , ""
        , "main.o: main.c"
        , "\t$(CC) -c main.c $(CFLAGS)"
        , ""
        , "icicle: main.o lib.o"
        , "\t$(CC) -o icicle main.o lib.o $(CFLAGS)"
        ]

    pure ()

