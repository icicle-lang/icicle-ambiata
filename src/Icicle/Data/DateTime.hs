-- | Quick and dirty date stuff
-- TODO support times as well
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , seaDateFunctions
  ) where
import           Data.Attoparsec.Text

import qualified Data.Dates         as D
import qualified Data.Time.Calendar as C

import           Data.Text  as T
import           Data.Word (Word64)
import           Data.Bits

import           P
import qualified Icicle.Internal.Pretty as PP

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

-- Unpack the word into an icicle DateTime
unpackWord64 :: Word64 -> DateTime
unpackWord64 d
 = let y  = shift (fromIntegral d) (-48)
       m  = shift (fromIntegral d) (-40) .&. 0xff
       d' = shift (fromIntegral d) (-32) .&. 0xff
       i  =       (fromIntegral d)       .&. 0xffffffff
       h  = i `quot` 3600
       m' = i `rem`  3600 `quot` 60
       s  = i `rem`  60
   in DateTime $ D.DateTime y m d' h m' s

-- C functions for fast date manipulations.
seaDateFunctions :: PP.Doc
seaDateFunctions = PP.vsep
  [ "// Number of days since 1600-03-01 (see Ivory DateTime)."
  , "iint_t idate_to_epoch (idate_t x)"
  , "  { "
  , "    int64_t y = (x >> 48) - 1600;"
  , "    int64_t m = (x >> 40) & 0xff;"
  , "    int64_t d = (x >> 32) & 0xff;"
  , "    m = (m + 9) % 12;"
  , "    y = y - m/10;"
  , "    return (365*y + y/4 - y/100 + y/400 + (m*306 + 5)/10 + ( d - 1 ));"
  , "  }"
  , ""
  , "iint_t idate_from_epoch (iint_t g)"
  , "  {"
  , "    int64_t y = ((10000*g + 14780)/3652425);"
  , "    int64_t ddd = g - (365*y + y/4 - y/100 + y/400);"
  , "    if (ddd < 0) {"
  , "      y = y - 1;"
  , "      ddd = g - (365*y + y/4 - y/100 + y/400);"
  , "    }"
  , "    int64_t mi = (100*ddd + 52)/3060;"
  , "    int64_t mm = (mi + 2)%12 + 1;"
  , "    y = y + (mi + 2)/12;"
  , "    int64_t dd = ddd - (mi*306 + 5)/10 + 1;"
  , "    return ((y + 1600) << 48 | mm << 40 | dd << 32);"
  , "  }"
  , ""
  , "iint_t idate_days_diff (idate_t x, idate_t y)"
  , "  {"
  , "     return (idate_to_epoch(y) - idate_to_epoch(x));"
  , "  }"
  , ""
  , "iint_t idate_minus_days (idate_t x, iint_t y)"
  , "  {"
  , "     return (idate_from_epoch(idate_to_epoch(x) - y));"
  , "  }"
  , ""
  , "static bool INLINE isLeapYear (int64_t y)"
  , "  {"
  , "     return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;"
  , "  }"
  , ""
  , "iint_t idate_minus_months (idate_t x, iint_t offset)"
  , "  {"
  , "    int64_t monthLengths[] = {31,28,31,30,31,30,31,31,30,31,30,31};"
  , "    int64_t y = (x >> 48) - 1600;"
  , "    int64_t m = (x >> 40) & 0xff;"
  , "    int64_t d = (x >> 32) & 0xff;"
  , "    bool prevYear = (offset > 0 && offset >= m);"
  , "    bool succYear = (offset < 0 && -offset > 12 - m);"
  , "    if ( prevYear ) {"
  , "      y = y - ((12 + offset - m) / 12);"
  , "      m = 12 + ((m - offset) % 12);"
  , "    } else if ( succYear ) {"
  , "      y = y + ((m - offset - 1) / 12);"
  , "      m = ((m - offset) % 12);"
  , "      if (m == 0) m = 12;"
  , "    } else {"
  , "      m = m - offset;"
  , "    }"
  , "    if (m == 2 && d > 28 && isLeapYear(y)) {"
  , "      d = 29;"
  , "    } else {"
  , "      int64_t maxMonth = monthLengths[m-1];"
  , "      if (d > maxMonth) {"
  , "        d = maxMonth;"
  , "      }"
  , "    }"
  , "    return ((y + 1600) << 48 | m << 40 | d << 32);"
  , "  }"
  ]
