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

import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Monad.Catch (bracket)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)

import qualified Data.ByteString.Char8 as ByteString
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Vector as Boxed

import           Foreign.C.String (peekCString, withCStringLen)
import           Foreign.C.Types (CChar)
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr, nullPtr)

import           Icicle.Sea.Error (SeaError (..))
import           Icicle.Sea.IO

import           Jetski

import           P                            hiding (count)

import           Piano

import           System.IO (FilePath, IO)

import qualified Zebra.Factset.Block as Zebra
import qualified Zebra.Factset.Entity as Zebra
import qualified Zebra.Factset.Table as Zebra
import qualified Zebra.Foreign.Entity as Zebra
import qualified Zebra.Foreign.Block as Zebra
import qualified Zebra.Serial.Binary as Binary
import qualified Zebra.Table.Striped as Striped
import qualified Zebra.X.ByteStream as ByteStream
import           Zebra.X.Stream (Stream, Of)
import qualified Zebra.X.Stream as Stream

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT)
import           X.Control.Monad.Trans.Either (hoistEither, runEitherT, firstJoin)


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

initZebraSnapshotStep ::
     Library
  -> EitherT SeaError IO (Ptr config -> CPiano -> Zebra.CEntity -> EitherT SeaError IO ())
initZebraSnapshotStep library = do
  c_zebra_alloc_state <-
    firstT SeaJetskiError $ function library "zebra_alloc_state" (retPtr retVoid)

  c_zebra_collect_state <-
    firstT SeaJetskiError $ function library "zebra_collect_state" retVoid

  c_zebra_snapshot_step <-
    firstT SeaJetskiError $ function library "zebra_snapshot_step" (retPtr retCChar)

  pure $ \config cpiano centity ->
    let
      allocState :: IO (Ptr ())
      allocState =
        c_zebra_alloc_state [
            argPtr (unCPiano cpiano)
          , argPtr config
          ]

      collectState :: Ptr () -> IO ()
      collectState state =
        c_zebra_collect_state [
            argPtr config
          , argPtr state
          ]

      snapshotStep :: Ptr () -> Zebra.CEntity -> IO (Ptr CChar)
      snapshotStep state centity =
        c_zebra_snapshot_step [
            argPtr (unCPiano cpiano)
          , argPtr state
          , argPtr (Zebra.unCEntity centity)
          ]
    in
      EitherT . bracket allocState collectState $ \state -> do
        --centity <-
        --  Zebra.foreignOfEntity pool entity

        errorPtr <-
          snapshotStep state centity

        if errorPtr == nullPtr then
          pure $ Right ()
        else do
          err <- peekCString errorPtr
          pure . Left . SeaExternalError $
            "zebra_snapshot_step failed: " <> Text.pack err

initZebraSnapshot ::
     Library
  -> ForeignPiano
  -> FilePath
  -> EitherT SeaError IO (Ptr config -> EitherT SeaError IO ())
initZebraSnapshot library piano input = do
  step <-
    initZebraSnapshotStep library

  pure $ \config ->
    EitherT . withCPiano piano $ \cpiano -> runResourceT . runEitherT $
      let
        tables :: Stream (Of Striped.Table) (EitherT SeaError (ResourceT IO)) ()
        tables =
          hoist (firstJoin SeaZebraDecodeError) .
            Binary.decodeStriped .
          hoist (firstT SeaZebraIOError) $
            ByteStream.readFile input
      in
        flip Stream.mapM_ tables $ \table ->
          hoist lift . EitherT . bracket Mempool.create Mempool.free $ \pool ->
            runEitherT $ do
              block <-
                hoistEither . first SeaZebraBlockTableError $ Zebra.blockOfTable table

              cblock <-
                firstT SeaZebraEntityError $ Zebra.foreignOfBlock pool block

              centities <-
                firstT SeaZebraForeignError $ Zebra.foreignEntitiesOfBlock pool cblock

              Boxed.mapM_ (step config cpiano) centities

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

  HasInput (FormatZebra _ _ _) _ input ->
    initZebraSnapshot library piano input

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
