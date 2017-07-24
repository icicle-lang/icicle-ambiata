{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Command.Timer (
    startTimer
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as Text
import           Data.Time (getCurrentTime, diffUTCTime)

import           P

import           System.IO (putStrLn)

import           Text.Printf (printf)


startTimer :: MonadIO m => Text -> m (m ())
startTimer title = do
  liftIO . putStrLn $ "icicle: " <> Text.unpack title
  time0 <- liftIO getCurrentTime
  pure $ do
    time1 <- liftIO getCurrentTime

    let
      seconds =
        realToFrac (time1 `diffUTCTime` time0) :: Double

    liftIO $ printf "icicle: %s (took %.2fs)\n" (Text.unpack title) seconds
