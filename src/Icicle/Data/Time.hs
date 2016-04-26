-- | Quick and dirty date stuff
-- TODO support times as well
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Data.Time (
    Time(..)
  , renderTime
  , timeOfText
  , timeOfYMD
  , timeOfDays
  , daysOfTime
  , withinWindow
  , daysDifference
  , minusMonths
  , minusDays
  , unsafeTimeOfYMD
  , pTime
  , packedOfTime
  , timeOfPacked
  ) where
import           Data.Attoparsec.Text

import qualified Data.Dates         as D
import qualified Data.Time.Calendar as C

import           Data.Text  as T
import           Data.Word (Word64)
import           Data.Bits

import           P

newtype Time =
  Time {
      getDateTime :: D.DateTime
    } deriving (Eq, Ord)

-- deepseq stops here, it shouldn't matter too much, we don't particularly
-- care about values
instance NFData Time where rnf _ = ()

instance Show Time where
 showsPrec p (Time x)
  = showParen (p > 10)
  $ showString "Time (D.DateTime "
  . showsPrec 11 (D.year x)
  . showString " "
  . showsPrec 11 (D.month x)
  . showString " "
  . showsPrec 11 (D.day x)
  . showString " "
  . showsPrec 11 (D.hour x)
  . showString " "
  . showsPrec 11 (D.minute x)
  . showString " "
  . showsPrec 11 (D.second x)
  . showString ")"


renderTime  :: Time -> Text
renderTime
 = -- if   D.hour d + D.minute d + D.second d == 0
   -- then T.pack (show (D.year d) <> "-" <>
   T.pack . C.showGregorian . D.dateTimeToDay . getDateTime

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

unsafeTimeOfYMD :: Integer -> Int -> Int -> Time
unsafeTimeOfYMD y m d
 = Time
 $ D.dayToDateTime
 $ C.fromGregorian y m d

timeOfYMD :: Integer -> Int -> Int -> Maybe Time
timeOfYMD y m d
 =   Time
 .   D.dayToDateTime
 <$> C.fromGregorianValid y m d

timeOfDays :: Int -> Time
timeOfDays d
 = Time
 $ D.dayToDateTime
 $ C.ModifiedJulianDay
 $ toInteger d

daysOfTime :: Time -> Int
daysOfTime d
 = fromInteger
 $ C.toModifiedJulianDay
 $ D.dateTimeToDay
 $ getDateTime d

-- | Check whether two given times are within a days window
withinWindow :: Time -> Time -> Int -> Bool
withinWindow fact now window
 = let diff =  daysDifference fact now
   in  diff <= window

-- | Find number of days between to tiems
daysDifference :: Time -> Time -> Int
daysDifference fact now
 = daysOfTime now - daysOfTime fact

minusDays :: Time -> Int -> Time
minusDays d i
 = Time
 $ D.dayToDateTime
 $ C.addDays (negate $ toInteger i)
 $ D.dateTimeToDay
 $ getDateTime d

minusMonths :: Time -> Int -> Time
minusMonths d i
 = Time
 $ D.dayToDateTime
 $ C.addGregorianMonthsClip (negate $ toInteger i)
 $ D.dateTimeToDay
 $ getDateTime d

-- Pack into Ivory's date/time format (for use in Sea evaluation).
-- A packed long
--   16 bits: year represented as a short
--   8 bits:  month represented as a byte
--   8 bits:  day represented as a byte
--   32 bits: seconds since start of day
packedOfTime :: Time -> Word64
packedOfTime (Time d)
  =  shift (fromIntegral (D.year  d)) 48
 .|. shift (fromIntegral (D.month d)) 40
 .|. shift (fromIntegral (D.day d))   32
 .|. (fromIntegral (3600 * D.hour d + 60 * D.minute d + D.second d))

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
   in Time $ D.DateTime y m d' h m' s
