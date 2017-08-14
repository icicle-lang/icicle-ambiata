{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Command.Timer (
    TimerDuration(..)
  , renderTimerDuration

  , startTimer
  , startTimer_
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time (getCurrentTime, diffUTCTime)

import           P

import           Text.Printf (printf)


newtype TimerDuration =
  TimerDuration {
      timerDurationSeconds :: Double
    } deriving (Eq, Ord, Show)

renderTimerDuration :: TimerDuration -> Text
renderTimerDuration =
  Text.pack . printf "%.2fs" . timerDurationSeconds

startTimer :: MonadIO m => Text -> m (m TimerDuration)
startTimer title = do
  liftIO . Text.putStrLn $ "icicle: " <> title
  time0 <- liftIO getCurrentTime
  pure $ do
    time1 <- liftIO getCurrentTime

    let
      duration =
        TimerDuration $
          realToFrac (time1 `diffUTCTime` time0)

    liftIO . Text.putStrLn $ "icicle: " <> title <> " (took " <> renderTimerDuration duration <> ")"
    pure duration

startTimer_ :: MonadIO m => Text -> m (m ())
startTimer_ =
  fmap (() <$) . startTimer
