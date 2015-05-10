-- | Quick and dirty date stuff
-- TODO support times as well
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Data.DateTime (
    DateTime (..)
  , renderDate
  , dateOfYMD
  , dateOfDays
  , withinWindow
  ) where

import qualified Data.Dates         as D
import qualified Data.Time.Calendar as C

import           Data.Text  as T

import           P


data DateTime =
  DateTime {
      getDateTime :: D.DateTime
    } deriving (Eq, Ord, Show)


renderDate  :: DateTime -> Text
renderDate d
 = -- if   D.hour d + D.minute d + D.second d == 0
   -- then T.pack (show (D.year d) <> "-" <>
   T.pack dateStr
 where
  d' = getDateTime d
  dateStr
   = show (D.year d') <> "-" <> show (D.month d') <> "-" <> show (D.day d')


dateOfYMD :: Int -> Int -> Int -> DateTime
dateOfYMD y m d
 = DateTime
 $ D.DateTime y m d 0 0 0

dateOfDays :: Int -> DateTime
dateOfDays d
 = DateTime
 $ D.dayToDateTime
 $ C.ModifiedJulianDay
 $ toInteger d

-- | Check whether two given dates are within a days window
-- TODO: datesDifference calculates the absolute difference but we don't want abs.
withinWindow :: DateTime -> DateTime -> Int -> Bool
withinWindow fact now window
 = let diff = getDateTime fact `D.datesDifference` getDateTime now
   in  diff <= fromIntegral window

