{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Icicle.Sea.Fleet (
    Mempool
  , SeaState
  , SeaFleet (..)
  , Input(..)
  , Output(..)
  , seaCreateFleet
  ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Morph
import           Control.Monad.Trans.Resource

import qualified Data.ByteString.Char8        as Strict
import           Data.String                  (String)
import qualified Data.Text                    as T
import qualified Data.Vector                  as VB
import           Data.Void                    (Void)

import           Foreign.C.String             (peekCString, withCStringLen)
import           Foreign.ForeignPtr           (newForeignPtr_)
import           Foreign.Ptr                  (Ptr, castPtr, nullPtr)

import           System.IO                    (FilePath, IO)

import           Icicle.Sea.Error             (SeaError (..))
import           Icicle.Sea.IO (InputFormat(..), InputOpts(..), OutputFormat(..), foreignSchemaOfFleetOutput)
import           Icicle.Sea.FromAvalanche.State (SeaProgramAttribute)

import           Piano

import           Anemone.Foreign.Mempool      (Mempool(..))

import           Zebra.Factset.Block

import           Zebra.Foreign.Entity
import           Zebra.Merge.BlockC
import           Zebra.Merge.Puller.File

import           Jetski
import           Jetski.Foreign.Binding

import           P                            hiding (count)

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (firstEitherT, hoistEither,
                                               joinErrors, left, runEitherT, bracketEitherT')


data Input a
  = NoInput
  | HasInput InputFormat InputOpts a
    deriving (Eq, Show, Functor)

data Output
  = NoOutput
  | HasOutput OutputFormat

data SeaState

data SeaFleet st = SeaFleet {
    sfLibrary     :: Library
  , sfCreatePool  :: IO (Ptr Mempool)
  , sfReleasePool :: Ptr Mempool  -> IO ()
  , sfGetPool     :: Ptr st       -> IO (Ptr Void)
  , sfSnapshot    :: Ptr st       -> EitherT SeaError IO ()
  , sfSegvInstall :: String       -> IO ()
  , sfSegvRemove  :: IO ()
  }

------------------------------------------------------------------------

seaCreateFleet ::
     [CompilerOption]
  -> CacheLibrary
  -> Input FilePath
  -> Output
  -> Maybe FilePath
  -> Text
  -> [SeaProgramAttribute]
  -> EitherT SeaError (ResourceT IO) (SeaFleet st)
seaCreateFleet options cache input output chords code states = do
  lib                  <- firstEitherT SeaJetskiError (compileLibrary cache options code)
  imempool_create      <- firstEitherT SeaJetskiError (function lib "anemone_mempool_create" (retPtr retVoid))
  imempool_free        <- firstEitherT SeaJetskiError (function lib "anemone_mempool_free"   retVoid)
  imempool_get         <- firstEitherT SeaJetskiError (function lib "fleet_get_mempool" retMempool)
  segv_install_handler <- firstEitherT SeaJetskiError (function lib "segv_install_handler" retVoid)
  segv_remove_handler  <- firstEitherT SeaJetskiError (function lib "segv_remove_handler"  retVoid)

  play <- case chords of
    Nothing -> do
      n <- liftIO $ newForeignPtr_ nullPtr
      let
        fun f ptr = withCPiano (ForeignPiano n) (runResourceT . f ptr)
      return fun

    Just file -> do
      bs <- liftIO $ Strict.readFile file
      case parsePiano bs of
        Left e ->
          left . SeaExternalError . T.pack $ "piano: " <> show e
        Right p -> do
          fp <- liftIO $ newForeignPiano p
          let
            fun f ptr = withCPiano fp (runResourceT . f ptr)
          return fun

  take_snapshot <- case input of
    NoInput -> do
      return $ \_ -> return (Right ())

    HasInput (InputFormatPsv _) _ _ -> do
      fn <- firstEitherT SeaJetskiError (function lib "psv_snapshot" retVoid)
      let
        withPiano :: Ptr a -> CPiano -> ResourceT IO (Either SeaError ())
        withPiano ptr cpiano =
          Right <$> liftIO (fn [ argPtr (unCPiano cpiano), argPtr ptr ])
      return $ play withPiano

    HasInput (InputFormatZebra _ _) _ input_path -> do
      init <- firstEitherT SeaJetskiError (function lib "zebra_alloc_state" (retPtr retVoid))
      end  <- firstEitherT SeaJetskiError (function lib "zebra_collect_state" (retPtr retVoid))

      (puller, pullid) <- firstEitherT (SeaExternalError . T.pack . show)
                        $ blockChainPuller (VB.singleton input_path)

      let withPiano :: Ptr a -> CPiano -> ResourceT IO (Either SeaError ())
          withPiano ptr cpiano = runEitherT $ do
            let
              piano =
                unCPiano cpiano

            state <- liftIO $ init [ argPtr piano, argPtr ptr ]

            step <- hoist liftIO $ case output of
              NoOutput ->
                return $ \_ -> return nullPtr

              HasOutput (OutputFormatPsv _) ->
                firstEitherT SeaJetskiError (function lib "zebra_snapshot_step_output_psv" (retPtr retCChar))

              HasOutput OutputFormatZebra -> do
                pool <- liftIO $ imempool_get [ argPtr state ]
                ctable <- firstEitherT (SeaZebraError . T.pack . show) $ foreignSchemaOfFleetOutput (Mempool pool) states
                fun <- firstEitherT SeaJetskiError (function lib "zebra_snapshot_step_output_zebra" (retPtr retCChar))
                return $ \args -> fun (args <> [argPtr ctable])

            let step' :: CEntity -> EitherT SeaError (ResourceT IO) ()
                step' e = do
                  s <- liftIO $ step [ argPtr piano, argPtr state, argPtr (unCEntity e) ]
                  if s /= nullPtr
                  then do
                    msg <- liftIO $ peekCString s
                    left . SeaExternalError . T.pack $ "error step: " <> show msg
                  else return ()

            let puller' :: PullId -> EitherT SeaError (ResourceT IO) (Maybe Block)
                puller' = firstEitherT (SeaExternalError . T.pack . show) . puller

            bracketEitherT'
               (pure ())
               (const (liftIO (end [ argPtr ptr, argPtr state ])))
               (const (joinErrors (SeaExternalError . T.pack  . show) id
                          (mergeBlocks (MergeOptions puller' step' 100) pullid)))

      return $ play withPiano

  return SeaFleet {
      sfLibrary     = lib
    , sfCreatePool  = castPtr <$> imempool_create []
    , sfReleasePool = \ptr -> imempool_free [argPtr ptr]
    , sfGetPool     = \ptr -> imempool_get [argPtr ptr]
    , sfSnapshot    = \ptr -> liftIO (take_snapshot ptr) >>= hoistEither
    , sfSegvInstall = \str -> withCStringLen str $ \(ptr, len) ->
                      segv_install_handler [argPtr ptr, argCSize (fromIntegral len)]
    , sfSegvRemove  = segv_remove_handler  []
    }

retMempool :: Return (Ptr Void)
retMempool =
  storableReturn ffi_type_pointer
