{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Sea.Eval (
    module Icicle.Sea.Eval.Base
  , module Icicle.Sea.Eval.Psv
  , module Icicle.Sea.IO
  , seaPsvSnapshotFilePath
  , seaPsvSnapshotFd
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as T

import           Foreign.C.String (peekCString)
import           Foreign.Marshal (free)
import           Foreign.Ptr (nullPtr)

import           Icicle.Sea.IO
import           Icicle.Sea.Eval.Base
import           Icicle.Sea.Eval.Psv

import           P hiding (count)

import           System.IO (IO, FilePath)
import qualified System.Posix as Posix

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (bracketEitherT')
import           X.Control.Monad.Trans.Either (left)

------------------------------------------------------------------------

seaPsvSnapshotFilePath
  :: SeaFleet PsvState
  -> FilePath
  -> FilePath
  -> Maybe FilePath
  -> EitherT SeaError IO PsvStats
seaPsvSnapshotFilePath fleet input output mchords = do
  let mopen mpath =
       case mpath of
         Nothing   -> pure Nothing
         Just path -> Just <$> Posix.openFd path Posix.ReadOnly Nothing Posix.defaultFileFlags

      mclose mfd =
        case mfd of
          Nothing -> pure ()
          Just fd -> Posix.closeFd fd

  bracketEitherT' (liftIO $ Posix.openFd input Posix.ReadOnly Nothing Posix.defaultFileFlags)
                  (liftIO . Posix.closeFd) $ \ifd -> do
  bracketEitherT' (liftIO $ Posix.createFile output (Posix.CMode 0O644))
                  (liftIO . Posix.closeFd) $ \ofd -> do
  bracketEitherT' (liftIO $ mopen mchords)
                  (liftIO . mclose) $ \mcfd -> do
  seaPsvSnapshotFd fleet ifd ofd mcfd


seaPsvSnapshotFd
  :: SeaFleet PsvState
  -> Posix.Fd
  -> Posix.Fd
  -> Maybe Posix.Fd
  -> EitherT SeaError IO PsvStats
seaPsvSnapshotFd fleet input output mchords =
  withWords 6 $ \pState -> do

  pokeWordOff pState 0 input
  pokeWordOff pState 1 output
  pokeWordOff pState 2 (fromMaybe 0 mchords)

  liftIO (sfSnapshot fleet pState)

  pError       <- peekWordOff pState 3
  factsRead    <- peekWordOff pState 4
  entitiesRead <- peekWordOff pState 5

  when (pError /= nullPtr) $ do
    msg <- liftIO (peekCString pError)
    liftIO (free pError)
    left (SeaPsvError (T.pack msg))

  return PsvStats {
      psvFactsRead = factsRead
    , psvEntitiesRead = entitiesRead
    }
