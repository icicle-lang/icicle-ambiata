{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Sea.Eval (
    module Icicle.Sea.Eval.Base
  , module Icicle.Sea.IO
  , FlagUseDrop (..)
  , ZebraState
  , ZebraStats (..)
  , seaZebraSnapshotFd
  , seaZebraSnapshotFilePath
  , PsvState
  , PsvStats (..)
  , seaPsvSnapshotFilePath
  , seaPsvSnapshotFd
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as T
import           Data.Typeable

import           Foreign.C.String (peekCString, newCString)
import           Foreign.Marshal (free)
import           Foreign.Ptr (nullPtr)

import           Icicle.Sea.IO
import           Icicle.Sea.Eval.Base

import           P hiding (count)

import           System.IO (IO, FilePath)
import qualified System.Posix as Posix

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (bracketEitherT')
import           X.Control.Monad.Trans.Either (left)


data FlagUseDrop = FlagUseDropFile | FlagNoUseDropFile
  deriving (Eq, Show)

data ZebraState
  deriving (Typeable)

data ZebraStats = ZebraStats {
    zebraFactsRead    :: Int64
  , zebraEntitiesRead :: Int64
  } deriving (Eq, Ord, Show)

data PsvState
  deriving (Typeable)

data PsvStats = PsvStats {
    psvFactsRead    :: Int64
  , psvEntitiesRead :: Int64
  } deriving (Eq, Ord, Show)


seaZebraSnapshotFilePath :: SeaFleet ZebraState
                         -> FilePath
                         -> FilePath
                         -> Maybe FilePath
                         -> EitherT SeaError IO ZebraStats
seaZebraSnapshotFilePath fleet input output mchords = do
  bracketEitherT' (liftIO $ Posix.createFile output (Posix.CMode 0O644))
                  (liftIO . Posix.closeFd) $ \ofd -> do
  bracketEitherT' (liftIO $ maybeOpen mchords)
                  (liftIO . maybeClose) $ \mcfd -> do
  seaZebraSnapshotFd fleet input ofd mcfd


seaZebraSnapshotFd :: SeaFleet ZebraState
                   -> FilePath
                   -> Posix.Fd
                   -> Maybe Posix.Fd
                   -> EitherT SeaError IO ZebraStats
seaZebraSnapshotFd fleet input output mchords = do
  withWords 6 $ \pState -> do
  a <- liftIO $ newCString input
  pokeWordOff pState 0 a
  pokeWordOff pState 1 output
  pokeWordOff pState 2 (fromMaybe 0 mchords)

  liftIO (sfSnapshot fleet pState)

  pError       <- peekWordOff pState 3
  factsRead    <- peekWordOff pState 4
  entitiesRead <- peekWordOff pState 5

  when (pError /= nullPtr) $ do
    msg <- liftIO (peekCString pError)
    liftIO (free pError)
    left (SeaZebraError (T.pack msg))

  return ZebraStats {
      zebraFactsRead = factsRead
    , zebraEntitiesRead = entitiesRead
    }


seaPsvSnapshotFilePath :: SeaFleet PsvState
                       -> FilePath
                       -> FilePath
                       -> FilePath
                       -> Maybe FilePath
                       -> Int
                       -> FlagUseDrop
                       -> EitherT SeaError IO PsvStats
seaPsvSnapshotFilePath fleet input output dropped mchords limit discard = do
  bracketEitherT' (liftIO $ Posix.openFd input Posix.ReadOnly Nothing Posix.defaultFileFlags)
                  (liftIO . Posix.closeFd) $ \ifd -> do
  bracketEitherT' (liftIO $ Posix.createFile output (Posix.CMode 0O644))
                  (liftIO . Posix.closeFd) $ \ofd -> do
  bracketEitherT' (liftIO $ Posix.createFile dropped (Posix.CMode 0O644))
                  (liftIO . Posix.closeFd) $ \dfd -> do
  bracketEitherT' (liftIO $ maybeOpen mchords)
                  (liftIO . maybeClose) $ \mcfd -> do
  seaPsvSnapshotFd fleet ifd ofd dfd mcfd limit discard



seaPsvSnapshotFd :: SeaFleet PsvState
                 -> Posix.Fd
                 -> Posix.Fd
                 -> Posix.Fd
                 -> Maybe Posix.Fd
                 -> Int
                 -> FlagUseDrop
                 -> EitherT SeaError IO PsvStats
seaPsvSnapshotFd fleet input output dropped mchords limit discard =
  withWords 9 $ \pState -> do

  pokeWordOff pState 0 input
  pokeWordOff pState 1 output
  pokeWordOff pState 2 dropped
  pokeWordOff pState 3 (fromMaybe 0 mchords)
  pokeWordOff pState 7 limit
  pokeWordOff pState 8 (discard == FlagUseDropFile)

  liftIO (sfSnapshot fleet pState)

  pError       <- peekWordOff pState 4
  factsRead    <- peekWordOff pState 5
  entitiesRead <- peekWordOff pState 6

  when (pError /= nullPtr) $ do
    msg <- liftIO (peekCString pError)
    liftIO (free pError)
    left (SeaPsvError (T.pack msg))

  return PsvStats {
      psvFactsRead = factsRead
    , psvEntitiesRead = entitiesRead
    }


maybeOpen :: Maybe FilePath -> IO (Maybe Posix.Fd)
maybeOpen mpath =
 case mpath of
   Nothing   -> pure Nothing
   Just path -> Just <$> Posix.openFd path Posix.ReadOnly Nothing Posix.defaultFileFlags


maybeClose :: Maybe Posix.Fd -> IO ()
maybeClose mfd =
  case mfd of
    Nothing -> pure ()
    Just fd -> Posix.closeFd fd
