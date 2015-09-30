-- | Quick and dirty date stuff
-- TODO support times as well
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Data.DateTime (
    DateTime
  , renderDate
  , dateOfYMD
  , dateOfDays
  , daysOfDate
  , withinWindow
  , daysDifference
  , minusMonths
  , minusDays
  , unsafeDateOfYMD
  , pDate
  ) where
import           Data.Attoparsec.Text

import qualified Data.Dates         as D
import qualified Data.Time.Calendar as C

import           Data.Text  as T

import           P

data DateTime =
  DateTime {
      getDateTime :: D.DateTime
    } deriving (Eq, Ord, Show)


renderDate  :: DateTime -> Text
renderDate
 = -- if   D.hour d + D.minute d + D.second d == 0
   -- then T.pack (show (D.year d) <> "-" <>
   T.pack . C.showGregorian . D.dateTimeToDay . getDateTime

pDate :: Parser DateTime
pDate
 = (maybe (fail "Invalid date") pure) =<< dateOfYMD <$> decimal <* dash <*> decimal <* dash <*> decimal
   where
    dash :: Parser ()
    dash = () <$ char '-'

unsafeDateOfYMD :: Integer -> Int -> Int -> DateTime
unsafeDateOfYMD y m d
 = DateTime
 $ D.dayToDateTime
 $ C.fromGregorian y m d

dateOfYMD :: Integer -> Int -> Int -> Maybe DateTime
dateOfYMD y m d
 =   DateTime
  .  D.dayToDateTime
 <$> C.fromGregorianValid y m d

dateOfDays :: Int -> DateTime
dateOfDays d
 = DateTime
 $ D.dayToDateTime
 $ C.ModifiedJulianDay
 $ toInteger d

daysOfDate :: DateTime -> Int
daysOfDate d
 = fromInteger
 $ C.toModifiedJulianDay
 $ D.dateTimeToDay
 $ getDateTime d

-- | Check whether two given dates are within a days window
withinWindow :: DateTime -> DateTime -> Int -> Bool
withinWindow fact now window
 = let diff =  daysDifference fact now
   in  diff <= window

-- | Find number of days between to dates
daysDifference :: DateTime -> DateTime -> Int
daysDifference fact now
 = daysOfDate now - daysOfDate fact

minusDays :: DateTime -> Integer -> DateTime
minusDays d i
 = DateTime
 $ D.dayToDateTime
 $ C.addDays (negate i)
 $ D.dateTimeToDay
 $ getDateTime d

minusMonths :: DateTime -> Integer -> DateTime
minusMonths d i
 = DateTime
 $ D.dayToDateTime
 $ C.addGregorianMonthsClip (negate i)
 $ D.dateTimeToDay
 $ getDateTime d

