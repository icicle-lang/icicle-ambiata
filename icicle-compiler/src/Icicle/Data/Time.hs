{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Icicle.Data.Time (
    Time(..)
  -- * Extract and conversion
  , julianDay
  , gregorianDay
  , timeOfDay
  , localHour
  , localMinute
  , localSecond
  , daysCountJulian
  , secondsCountJulian
  , timeOfText
  , timeOfYMD
  , timeOfDays
  , withinWindow
  , unsafeTimeOfYMD
  , dayOf
  , monthOf
  , yearOf

  -- * Operations
  , daysDifference
  , secondsDifference
  , minusSeconds
  , minusMonths
  , minusDays
  , packedOfTime
  , timeOfPacked

  -- * Parsing and Printing
  , renderTime
  , pTime
  ) where
import           Data.Attoparsec.Text

import qualified Data.Time.Calendar as C
import qualified Data.Thyme         as Thyme
import qualified Data.Thyme.Time    as Thyme
import           Data.AffineSpace
import           Data.Text  as T
import           Data.Word (Word64)
import           Data.Bits

import           Control.Lens ((^.))

import           P

newtype Time =
  Time {
      getDateTime :: Thyme.UTCTime
    } deriving (Eq, Ord)

-- deepseq stops here, it shouldn't matter too much, we don't particularly
-- care about values
instance NFData Time where rnf _ = ()

instance Show Time where
 showsPrec p x
  = let g = gregorianDay x
        h = timeOfDay    x
    in showParen (p > 10)
     $ showString "Time ("
     . showsPrec 11 (g ^. Thyme._ymdYear)
     . showString " "
     . showsPrec 11 (g ^. Thyme._ymdMonth)
     . showString " "
     . showsPrec 11 (g ^. Thyme._ymdDay)
     . showString " "
     . showsPrec 11 (h ^. Thyme._todHour)
     . showString " "
     . showsPrec 11 (h ^. Thyme._todMin)
     . showString " "
     . showsPrec 11 (h ^. Thyme._todSec)
     . showString ")"

--------------------------------------------------------------------------------

julianDay :: Time -> Thyme.Day
julianDay x
  = getDateTime x ^. Thyme._utctDay

gregorianDay :: Time -> Thyme.YearMonthDay
gregorianDay x
  = julianDay x ^. Thyme.gregorian

diffTime :: Time -> Thyme.DiffTime
diffTime x
  = getDateTime x ^. Thyme._utctDayTime

timeOfDay :: Time -> Thyme.TimeOfDay
timeOfDay x
  = diffTime x ^. Thyme.timeOfDay

dayOf :: Time -> Int
dayOf x = gregorianDay x ^. Thyme._ymdDay

monthOf :: Time -> Int
monthOf x = gregorianDay x ^. Thyme._ymdMonth

yearOf :: Time -> Int
yearOf x = gregorianDay x ^. Thyme._ymdYear

localHour :: Time -> Int
localHour x = timeOfDay x ^. Thyme._todHour

localMinute :: Time -> Int
localMinute x = timeOfDay x ^. Thyme._todMin

localSecond :: Time -> Int
localSecond x = timeOfDay x ^. Thyme._todHour

-- | Number of days since Julian zero, 1858-11-17T00:00:00Z
daysCountJulian :: Time -> Int
daysCountJulian
  = Thyme.toModifiedJulianDay . julianDay

-- | Number of seconds since Julian zero, 1858-11-17T00:00:00Z
--   Hope it's not too big.
secondsCountJulian :: Time -> Int
secondsCountJulian t
  = let days  = daysCountJulian t
        hours = localHour       t
        mins  = localMinute     t
        secs  = localSecond     t
    in (days * 24 * 3600) + (hours * 3600) + (mins * 60) + secs

--------------------------------------------------------------------------------

timeOfDays :: Int -> Time
timeOfDays d
 = Time
 $ flip Thyme.mkUTCTime 0
 $ Thyme.ModifiedJulianDay d

unsafeTimeOfYMD :: Int -> Int -> Int -> Time
unsafeTimeOfYMD y m d
 = Time
 $ flip Thyme.mkUTCTime 0
 $ Thyme.fromGregorian y m d

timeOfYMD :: Int -> Int -> Int -> Maybe Time
timeOfYMD y m d
 =   Time
 .   flip Thyme.mkUTCTime 0
 <$> Thyme.fromGregorianValid y m d

timeOfYMDHMS :: Int -> Int -> Int -> Int -> Int -> Int -> Time
timeOfYMDHMS year month day hour minute sec
  = let ymd = Thyme.fromGregorian year month day
        hms = Thyme.timeOfDayToTime
            $ Thyme.TimeOfDay hour minute
            $ Thyme.secondsToDiffTime
            $ fromIntegral sec
    in Time $ Thyme.mkUTCTime ymd hms

-- Unpack the word into an icicle Time
timeOfPacked :: Word64 -> Time
timeOfPacked d
 = let y  = shift (fromIntegral d) (-48)
       m  = shift (fromIntegral d) (-40) .&. 0xff
       d' = shift (fromIntegral d) (-32) .&. 0xff
       i  =       (fromIntegral d)       .&. 0xffffffff
       h  = i `quot` 3600
       m' = i `rem`  3600 `quot` 60
       s  = i `rem`  60
   in timeOfYMDHMS y m d' h m' s

-- Pack into Ivory's date/time format (for use in Sea evaluation).
-- A packed long
--   16 bits: year represented as a short
--   8 bits:  month represented as a byte
--   8 bits:  day represented as a byte
--   32 bits: seconds since start of day
packedOfTime :: Time -> Word64
packedOfTime t@(gregorianDay -> d)
  =  shift (fromIntegral (Thyme.ymdYear  d)) 48
 .|. shift (fromIntegral (Thyme.ymdMonth d)) 40
 .|. shift (fromIntegral (Thyme.ymdDay   d)) 32
 .|. (fromIntegral (3600 * localHour t + 60 * localMinute t + localSecond t))

--------------------------------------------------------------------------------

renderTime  :: Time -> Text
renderTime = T.pack . C.showGregorian . Thyme.fromThyme . julianDay

pTime :: Parser Time
pTime
 = (maybe (fail "Invalid time") pure) =<< timeOfYMD <$> decimal <* dash <*> decimal <* dash <*> decimal
   where
    dash :: Parser ()
    dash = () <$ char '-'

timeOfText :: Text -> Maybe Time
timeOfText txt =
  case parseOnly pTime txt of
    Left  _ -> Nothing
    Right x -> Just x

-- | Check whether two given times are within a days window
withinWindow :: Time -> Time -> Int -> Bool
withinWindow fact now window
 = let diff =  daysDifference fact now
   in  diff <= window

-- | Find number of days between two times
daysDifference :: Time -> Time -> Int
daysDifference fact now
  = daysCountJulian now - daysCountJulian fact

-- | Numer of seconds between two times
secondsDifference :: Time -> Time -> Int
secondsDifference fact now
  = let t :: Double
        t = Thyme.toSeconds $ getDateTime now .-. getDateTime fact
    in round t

minusSeconds :: Time -> Int -> Time
minusSeconds d i
 = Time $ getDateTime d .-^ Thyme.fromSeconds i

minusDays :: Time -> Int -> Time
minusDays d i
 = Time
 $ flip Thyme.mkUTCTime (Thyme.utctDayTime view)
 $ Thyme.addDays (-i)   (Thyme.utctDay     view)
 where
   view = getDateTime d ^. Thyme.utcTime

minusMonths :: Time -> Int -> Time
minusMonths d i
 = Time
 $ flip Thyme.mkUTCTime              (Thyme.utctDayTime view)
 $ Thyme.addGregorianMonthsClip (-i) (Thyme.utctDay     view)
 where
   view = getDateTime d ^. Thyme.utcTime
