-- | Quick and dirty date stuff
-- TODO support times as well
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Data.DateTime (
    DateTime(..)
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
  , packWord64
  , unpackWord64
  ) where
import           Data.Attoparsec.Text

import qualified Data.Dates         as D
import qualified Data.Time.Calendar as C

import           Data.Text  as T
import           Data.Word (Word64)
import           Data.Bits

import           P

newtype DateTime =
  DateTime {
      getDateTime :: D.DateTime
    } deriving (Eq, Ord)

instance Show DateTime where
 showsPrec p (DateTime x)
  = showParen (p > 10)
  $ showString "DateTime (D.DateTime "
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

minusDays :: DateTime -> Int -> DateTime
minusDays d i
 = DateTime
 $ D.dayToDateTime
 $ C.addDays (negate $ toInteger i)
 $ D.dateTimeToDay
 $ getDateTime d

minusMonths :: DateTime -> Int -> DateTime
minusMonths d i
 = DateTime
 $ D.dayToDateTime
 $ C.addGregorianMonthsClip (negate $ toInteger i)
 $ D.dateTimeToDay
 $ getDateTime d

-- Pack into Ivory's DateTime (for use in Sea evaluation).
-- A packed long
--   16 bits: year represented as a short
--   8 bits:  month represented as a byte
--   8 bits:  day represented as a byte
--   32 bits: seconds since start of day
packWord64 :: DateTime -> Word64
packWord64 (DateTime d)
  =  shift (fromIntegral (D.year  d)) 48
 .|. shift (fromIntegral (D.month d)) 40
 .|. shift (fromIntegral (D.day d))   32
 .|. (fromIntegral (3600 * D.hour d + 60 * D.minute d + D.second d))

unpackWord64 :: Word64 -> DateTime
unpackWord64 d
 = let y  = shift (fromIntegral d) (-48)
       m  = shift (fromIntegral d) (-40) .&. 0xff
       d' = shift (fromIntegral d) (-32) .&. 0xff
       i  =       (fromIntegral d)       .&. 0xffffffff
       h  = i `quot` 3600
       m' = i `rem` 3600 `quot` 60
       s  = i `rem` 60
  in DateTime $ D.DateTime y m d' h m' s
