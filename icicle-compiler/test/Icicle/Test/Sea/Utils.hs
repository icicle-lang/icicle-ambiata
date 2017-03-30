{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Sea.Utils (
    codeOfDoc
  , textOfDoc
  , readLibrary
  , readLibraryWith
  , releaseLibraryAfterTests
  ) where

import qualified Icicle.Internal.Pretty       as PP
import           Icicle.Sea.Eval              (getCompilerOptions)
import           Icicle.Sea.Preamble          (seaPreamble)
import           Icicle.Test.Arbitrary        ()

import           Control.Exception            (finally)
import           Control.Monad.IO.Class       (liftIO)

import           Data.IORef                   (IORef, newIORef, readIORef,
                                               writeIORef)
import qualified Data.Text                    as T

import           Jetski

import           P

import           System.IO
import           System.IO.Unsafe             (unsafePerformIO)

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

codeOfDoc :: PP.Doc -> SourceCode
codeOfDoc code = textOfDoc (PP.vsep ["#define ICICLE_NO_INPUT 1", seaPreamble, code])

textOfDoc :: PP.Doc -> T.Text
textOfDoc doc = T.pack (PP.displayS (PP.renderPretty 0.8 80 (PP.pretty doc)) "")

------------------------------------------------------------------------

libraryRef :: IORef (Maybe (Either JetskiError Library))
libraryRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE libraryRef #-}

readLibrary :: SourceCode -> EitherT JetskiError IO Library
readLibrary code =
  flip readLibraryWith code =<< liftIO getCompilerOptions

readLibraryWith :: [CompilerOption] -> SourceCode -> EitherT JetskiError IO Library
readLibraryWith opts code = do
  mlib <- liftIO (readIORef libraryRef)
  case mlib of
    Just elib -> hoistEither elib
    Nothing   -> do
      elib <- liftIO (runEitherT (compileLibrary NoCacheLibrary opts code))
      liftIO (writeIORef libraryRef (Just elib))
      hoistEither elib

releaseLibraryRef :: IO ()
releaseLibraryRef = do
  elib <- readIORef libraryRef
  case elib of
    Nothing          -> pure ()
    Just (Left  _)   -> pure ()
    Just (Right lib) -> do
      writeIORef libraryRef Nothing
      releaseLibrary lib

releaseLibraryAfterTests :: IO a -> IO a
releaseLibraryAfterTests = flip finally releaseLibraryRef
