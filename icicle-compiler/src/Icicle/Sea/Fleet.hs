{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Icicle.Sea.Fleet (
    SeaState
  , SeaFleet(..)
  , Input(..)
  , seaCreateFleet
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString.Char8 as ByteString
import           Data.String (String)

import           Foreign.C.String (withCStringLen)
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr, nullPtr)

import           Icicle.Sea.Error (SeaError (..))
import           Icicle.Sea.IO

import           Jetski

import           P                            hiding (count)

import           Piano

import           System.IO (FilePath, IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)


data Input a =
    NoInput
  | HasInput IOFormat InputOpts a
    deriving (Eq, Show, Functor)

data SeaState

data SeaFleet config =
  SeaFleet {
      sfLibrary :: Library
    , sfSnapshot :: Ptr config -> EitherT SeaError IO ()
    , sfSegvInstall :: String -> IO ()
    , sfSegvRemove :: IO ()
    }

------------------------------------------------------------------------

readChordDescriptor :: Maybe FilePath -> EitherT SeaError IO ForeignPiano
readChordDescriptor = \case
  Nothing ->
    ForeignPiano <$> liftIO (newForeignPtr_ nullPtr)

  Just path -> do
    bs <- liftIO $ ByteString.readFile path
    piano <- hoistEither . first SeaPianoError $ parsePiano bs
    liftIO $ newForeignPiano piano

initPsvSnapshot ::
     Library
  -> ForeignPiano
  -> EitherT SeaError IO (Ptr state -> EitherT SeaError IO ())
initPsvSnapshot library piano = do
  c_psv_snapshot <-
    firstT SeaJetskiError $ function library "psv_snapshot" retVoid

  pure $ \state ->
    liftIO . withCPiano piano $ \cpiano ->
      c_psv_snapshot [ argPtr (unCPiano cpiano), argPtr state ]

initSnapshot ::
     Library
  -> ForeignPiano
  -> Input FilePath
  -> EitherT SeaError IO (Ptr config -> EitherT SeaError IO ())
initSnapshot library piano = \case
  NoInput -> do
    pure $ \_config ->
      pure ()

  HasInput (FormatPsv _) _ _ ->
    initPsvSnapshot library piano

seaCreateFleet ::
     [CompilerOption]
  -> CacheLibrary
  -> Input FilePath
  -> Maybe FilePath
  -> Text
  -> EitherT SeaError IO (SeaFleet config)
seaCreateFleet options cache input chordDescriptor code = do
  library <-
    firstT SeaJetskiError $ compileLibrary cache options code

  c_segv_install_handler <-
    firstT SeaJetskiError $ function library "segv_install_handler" retVoid

  c_segv_remove_handler <-
    firstT SeaJetskiError $ function library "segv_remove_handler"  retVoid

  piano <-
    readChordDescriptor chordDescriptor

  snapshot <-
    initSnapshot library piano input

  pure SeaFleet {
      sfLibrary =
        library

    , sfSnapshot =
        snapshot

    , sfSegvInstall = \str ->
        withCStringLen str $ \(ptr, len) ->
          c_segv_install_handler [ argPtr ptr, argCSize (fromIntegral len) ]

    , sfSegvRemove =
        c_segv_remove_handler []
    }
