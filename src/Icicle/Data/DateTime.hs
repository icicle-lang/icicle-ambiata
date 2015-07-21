-- | Quick and dirty date stuff
-- TODO support times as well
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Data.DateTime (
    DateTime (..)
  , renderDate
  , dateOfYMD
  , dateOfDays
  , daysOfDate
  , withinWindow
  , daysDifference
  ) where

import           Control.Lens  (view)
import           Data.Maybe
import           Data.Text     as T
import qualified Data.Thyme    as H
import qualified System.Locale as L

import           P


-- | Gregorian calendar date
--
data DateTime =
  DateTime {
      getDateTime :: H.YearMonthDay
    } deriving (Eq, Ord, Show)


renderDate  :: DateTime -> Text
renderDate (DateTime ymd)
  = T.pack
  $ H.formatTime L.defaultTimeLocale "%Y-%-m-%-d" ymd

dateOfYMD :: Int -> Int -> Int -> DateTime
dateOfYMD y m d
 = DateTime $ H.YearMonthDay y m d

-- | Convert Julian days to our Gregorian calendar date
dateOfDays :: Int -> DateTime
dateOfDays
 = DateTime
 . view H.gregorian
 . H.ModifiedJulianDay

-- | Convert our Gregorian date to Julian days,
--   assuming it's within Julian range.
daysOfDate :: DateTime -> Int
daysOfDate = fromJust . daysOfDate_

daysOfDate_ :: DateTime -> Maybe Int
daysOfDate_
 = fmap (view H.modifiedJulianDay)
 . H.gregorianValid
 . getDateTime

-- | Check whether two given dates are within a days window
withinWindow :: DateTime -> DateTime -> Int -> Bool
withinWindow fact now window
 = let diff =  daysDifference fact now
   in  diff <= window

-- | Find number of days between to dates
daysDifference :: DateTime -> DateTime -> Int
daysDifference fact now
 = daysOfDate now - daysOfDate fact

