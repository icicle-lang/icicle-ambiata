{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Sea.Fleet (
    MemPool
  , SeaState
  , SeaFleet (..)
  , Input(..)
  , seaCreateFleet
  ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Morph

import qualified Data.ByteString.Char8        as Strict
import           Data.String                  (String)
import qualified Data.Text                    as T
import qualified Data.Vector                  as VB

import           Foreign.C.String             (peekCString, withCStringLen)
import           Foreign.ForeignPtr           (newForeignPtr_)
import           Foreign.Ptr                  (Ptr, castPtr, nullPtr)

import           System.IO                    (FilePath, IO)

import           Icicle.Sea.Error             (SeaError (..))
import           Icicle.Sea.IO

import           Piano

import           Zebra.Data
import           Zebra.Schema (Schema)
import           Zebra.Foreign.Entity
import           Zebra.Merge.BlockC
import           Zebra.Merge.Puller.File

import           Jetski

import           P                            hiding (count)

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (firstEitherT, hoistEither,
                                               joinErrors, left, runEitherT, bracketEitherT')


data Input a
  = NoInput
  | HasInput IOFormat InputOpts a
    deriving (Eq, Show, Functor)

data MemPool
data SeaState

data SeaFleet st = SeaFleet {
    sfLibrary     :: Library
  , sfCreatePool  :: IO (Ptr MemPool)
  , sfReleasePool :: Ptr MemPool  -> IO ()
  , sfSnapshot    :: Ptr st       -> EitherT SeaError IO ()
  , sfSegvInstall :: String       -> IO ()
  , sfSegvRemove  :: IO ()
  }

------------------------------------------------------------------------

seaCreateFleet ::
     (MonadIO m)
  => [CompilerOption]
  -> CacheLibrary
  -> Input FilePath
  -> Maybe FilePath
  -> Text
  -> EitherT SeaError m (SeaFleet st)
seaCreateFleet options cache input chords code = do
  lib                  <- firstEitherT SeaJetskiError (compileLibrary cache options code)
  imempool_create      <- firstEitherT SeaJetskiError (function lib "anemone_mempool_create" (retPtr retVoid))
  imempool_free        <- firstEitherT SeaJetskiError (function lib "anemone_mempool_free"   retVoid)
  segv_install_handler <- firstEitherT SeaJetskiError (function lib "segv_install_handler" retVoid)
  segv_remove_handler  <- firstEitherT SeaJetskiError (function lib "segv_remove_handler"  retVoid)

  play <- case chords of
    Nothing -> do
      n <- liftIO $ newForeignPtr_ nullPtr
      return $ \f ptr -> withCPiano (ForeignPiano n) (f ptr)

    Just file -> do
      bs <- liftIO $ Strict.readFile file

      case parsePiano bs of
        Left e ->
          left . SeaExternalError . T.pack $ "piano: " <> show e

        Right p -> do
          fp <- liftIO $ newForeignPiano p
          return $ \f ptr -> withCPiano fp (f ptr)

  take_snapshot <- case input of
    NoInput -> do
      return $ \_ -> return (Right ())

    HasInput (FormatPsv _) _ _ -> do
      fn <- firstEitherT SeaJetskiError (function lib "psv_snapshot" retVoid)
      return $ play $ \ptr cpiano -> Right <$> fn [ argPtr (unCPiano cpiano), argPtr ptr ]

    HasInput (FormatZebra _ _) _ input_path -> do
      step <- firstEitherT SeaJetskiError (function lib "zebra_snapshot_step" (retPtr retCChar))
      init <- firstEitherT SeaJetskiError (function lib "zebra_alloc_state" (retPtr retVoid))
      end  <- firstEitherT SeaJetskiError (function lib "zebra_collect_state" (retPtr retVoid))

      (puller, pullid) <- hoist liftIO
                        $ firstEitherT (SeaExternalError . T.pack . show)
                        $ blockChainPuller (VB.singleton input_path)

      let withPiano :: Ptr a -> CPiano -> IO (Either SeaError ())
          withPiano ptr cpiano = do
            let piano = unCPiano cpiano

            state <- init [ argPtr piano, argPtr ptr ]

            let step' :: CEntity -> EitherT SeaError IO ()
                step' e = do
                  s <- liftIO $ step [ argPtr piano, argPtr state, argPtr (unCEntity e) ]
                  if s /= nullPtr
                  then do
                    msg <- liftIO $ peekCString s
                    left . SeaExternalError . T.pack $ "error step: " <> show msg
                  else return ()

            let puller' :: PullId -> EitherT SeaError IO (Maybe (Block Schema))
                puller' = firstEitherT (SeaExternalError . T.pack . show) . puller

            runEitherT
              $ bracketEitherT'
                  (pure ())
                  (const (liftIO (end [ argPtr ptr, argPtr state ])))
                  (const (joinErrors (SeaExternalError . T.pack  . show) id
                             (mergeBlocks (MergeOptions puller' step' 100) pullid)))

      return $ play withPiano


  return SeaFleet {
      sfLibrary     = lib
    , sfCreatePool  = castPtr <$> imempool_create []
    , sfReleasePool = \ptr -> imempool_free [argPtr ptr]
    , sfSnapshot    = \ptr -> liftIO (take_snapshot ptr) >>= hoistEither
    , sfSegvInstall = \str -> withCStringLen str $ \(ptr, len) ->
                      segv_install_handler [argPtr ptr, argCSize (fromIntegral len)]
    , sfSegvRemove  = segv_remove_handler  []
    }
