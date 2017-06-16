{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Sea.Fleet (
    SeaState
  , SeaFleet (..)
  , Input(..)
  , seaCreateFleet
  ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.Resource
import           Control.Monad.Morph

import qualified Data.ByteString.Char8        as Strict
import           Data.String                  (String)
import qualified Data.Text                    as T
import qualified Data.Vector                  as VB

import           Foreign.C.String             (peekCString, withCStringLen)
import           Foreign.ForeignPtr           (newForeignPtr_)
import           Foreign.Ptr                  (Ptr, nullPtr)

import           System.IO                    (FilePath, IO)

import           Icicle.Sea.Error             (SeaError (..))
import           Icicle.Sea.IO

import           Piano

import qualified Anemone.Foreign.Mempool as Mempool

import qualified Zebra.Factset.Block          as Zebra
import qualified Zebra.Foreign.Entity         as Zebra
import qualified Zebra.Factset.Table          as Zebra
import qualified Zebra.Serial.Binary          as Binary
import qualified Zebra.X.Stream as Stream
import qualified Zebra.X.ByteStream as ByteStream

import           Jetski

import           P                            hiding (count)

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (firstEitherT, hoistEither, left, runEitherT, bracketEitherT')


data Input a
  = NoInput
  | HasInput IOFormat InputOpts a
    deriving (Eq, Show, Functor)

data SeaState

data SeaFleet st = SeaFleet {
    sfLibrary     :: Library
  , sfSnapshot    :: Ptr st -> EitherT SeaError IO ()
  , sfSegvInstall :: String -> IO ()
  , sfSegvRemove  :: IO ()
  }

------------------------------------------------------------------------

seaCreateFleet ::
     [CompilerOption]
  -> CacheLibrary
  -> Input FilePath
  -> Maybe FilePath
  -> Text
  -> EitherT SeaError (ResourceT IO) (SeaFleet st)
seaCreateFleet options cache input chords code = do
  lib                  <- firstEitherT SeaJetskiError (compileLibrary cache options code)
  segv_install_handler <- firstEitherT SeaJetskiError (function lib "segv_install_handler" retVoid)
  segv_remove_handler  <- firstEitherT SeaJetskiError (function lib "segv_remove_handler"  retVoid)

  play <- case chords of
    Nothing -> do
      n <- liftIO $ newForeignPtr_ nullPtr
      return $ \f ptr -> withCPiano (ForeignPiano n) (runResourceT . f ptr)

    Just file -> do
      bs <- liftIO $ Strict.readFile file

      case parsePiano bs of
        Left e ->
          left . SeaExternalError . T.pack $ "piano: " <> show e

        Right p -> do
          fp <- liftIO $ newForeignPiano p
          return $ \f ptr -> withCPiano fp (runResourceT . f ptr)

  take_snapshot <- case input of
    NoInput -> do
      return $ \_ -> return (Right ())

    HasInput (FormatPsv _) _ _ -> do
      fn <- firstEitherT SeaJetskiError (function lib "psv_snapshot" retVoid)
      return $ play $ \ptr cpiano -> Right <$> liftIO (fn [ argPtr (unCPiano cpiano), argPtr ptr ])

    HasInput (FormatZebra _ _ _) _ input_path -> do
      entityStep <- firstEitherT SeaJetskiError (function lib "zebra_snapshot_step" (retPtr retCChar))
      entityInit <- firstEitherT SeaJetskiError (function lib "zebra_alloc_state" (retPtr retVoid))
      entityEnd <- firstEitherT SeaJetskiError (function lib "zebra_collect_state" (retPtr retVoid))

      let
        tables =
          hoist (firstEitherT SeaZebraDecodeError) .
          Binary.decodeStriped .
          ByteStream.readFile $
            input_path

        withPianoReadEntity :: Ptr a -> CPiano -> ResourceT IO (Either SeaError ())
        withPianoReadEntity cfg cpiano = do
          let
            piano =
              unCPiano cpiano


          let
            readCEntity :: Ptr a -> Zebra.CEntity -> EitherT SeaError (ResourceT IO) ()
            readCEntity state entity = do
              s <- liftIO $ entityStep [ argPtr piano, argPtr state, argPtr (Zebra.unCEntity entity) ]
              if s /= nullPtr
              then do
                msg <- liftIO $ peekCString s
                left . SeaExternalError . T.pack $ "error step: " <> show msg
              else return ()

          runEitherT $
            bracketEitherT'
              (liftIO Mempool.create)
              (liftIO . Mempool.free)
              (\pool -> squash . Stream.effects . Stream.for (hoist (hoist (firstT SeaZebraIOError)) tables) $
                 \table -> do
                    block <- lift . hoistEither . first SeaZebraBlockTableError . Zebra.blockOfTable $ table
                    entities <- lift . hoistEither . first SeaZebraEntityError . Zebra.entitiesOfBlock $ block
                    lift . hoist lift . VB.forM entities $ \entity -> do
                      bracketEitherT'
                        (liftIO $ entityInit [ argPtr piano, argPtr cfg ])
                        (\state -> liftIO $ entityEnd [ argPtr cfg , argPtr state ])
                        (\state -> do
                           centity <- Zebra.foreignOfEntity pool entity
                           readCEntity state centity)
              )

      return $ play withPianoReadEntity


  return SeaFleet {
      sfLibrary     = lib
    , sfSnapshot    = \ptr -> liftIO (take_snapshot ptr) >>= hoistEither
    , sfSegvInstall = \str -> withCStringLen str $ \(ptr, len) ->
                      segv_install_handler [argPtr ptr, argCSize (fromIntegral len)]
    , sfSegvRemove  = segv_remove_handler  []
    }
