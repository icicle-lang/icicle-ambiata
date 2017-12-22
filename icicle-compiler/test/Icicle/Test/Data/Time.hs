{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Data.Time where

import           Icicle.Data.Time
import qualified Icicle.Internal.Pretty as PP
import           Icicle.Test.Sea.Utils
import           Icicle.Test.Arbitrary

import           Control.Monad.IO.Class (liftIO)

import           Disorder.Core.IO

import           Jetski

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property

import           X.Control.Monad.Trans.Either

prop_packed_symmetry :: TimeWithTime -> Property
prop_packed_symmetry (TimeWithTime d) =
  d === timeOfPacked (packedOfTime d)

prop_time_sea_to_days :: TimeWithTime -> Property
prop_time_sea_to_days (TimeWithTime d)
  = testIO $ do
  let epochTime = unsafeTimeOfYMD 1600 03 01
  let expected  = daysDifference epochTime d

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_to_epoch_days" retInt64
    r <- liftIO $ f [argWord64 $ packedOfTime d]
    pure $ expected === fromIntegral r

prop_time_sea_to_seconds :: TimeWithTime -> Property
prop_time_sea_to_seconds (TimeWithTime d)
  = testIO $ do
  let epochTime = unsafeTimeOfYMD 1600 03 01
  let expected  = secondsDifference epochTime d

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_to_epoch_seconds" retInt64
    r <- liftIO $ f [argWord64 $ packedOfTime d]
    pure $ expected === fromIntegral r

prop_time_sea_from_days :: TimeWithTime -> Property
prop_time_sea_from_days (TimeWithTime d)
  = testIO $ do
  let epochTime = unsafeTimeOfYMD 1600 03 01
  let epochDiff = daysDifference epochTime d
  let timeH     = localHour   d
  let timeM     = localMinute d
  let timeS     = localSecond d

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_from_epoch_days" retInt64
    r <- liftIO $ f [ argWord64 $ fromIntegral epochDiff
                    , argWord64 $ fromIntegral timeH
                    , argWord64 $ fromIntegral timeM
                    , argWord64 $ fromIntegral timeS ]
    pure $ d === timeOfPacked (fromIntegral r)

prop_time_sea_from_seconds :: TimeWithTime -> Property
prop_time_sea_from_seconds (TimeWithTime d)
  = testIO $ do
  let epochTime = unsafeTimeOfYMD 1600 03 01
  let epochDiff = secondsDifference epochTime d

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_from_epoch_seconds" retInt64
    r <- liftIO $ f [argWord64 $ fromIntegral epochDiff]
    pure $ d === timeOfPacked (fromIntegral r)

prop_time_symmetry_sea_days :: TimeWithTime -> TimeWithTime -> Property
prop_time_symmetry_sea_days (TimeWithTime d1) (TimeWithTime d2)
  = testIO $ do
  let expected  = daysDifference d1 d2

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_days_diff" retInt64
    r <- liftIO $ f [argWord64 (packedOfTime d1), argWord64 (packedOfTime d2)]
    pure $ expected === fromIntegral r

prop_time_symmetry_sea_seconds :: TimeWithTime -> TimeWithTime -> Property
prop_time_symmetry_sea_seconds (TimeWithTime d1) (TimeWithTime d2)
  = testIO $ do
  let expected  = secondsDifference d1 d2

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_seconds_diff" retInt64
    r <- liftIO $ f [argWord64 (packedOfTime d1), argWord64 (packedOfTime d2)]
    pure $ expected === fromIntegral r

prop_time_minus_days :: TimeWithTime -> Int -> Property
prop_time_minus_days (TimeWithTime d) num
  = testIO $ do
  -- Add or subtract only a few years.
  let num' = num `rem` 3650
  let expected  = minusDays d num'

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_minus_days" retInt64
    r <- liftIO $ f [argWord64 $ packedOfTime d, argWord64 (fromIntegral num')]
    pure $ expected === timeOfPacked (fromIntegral r)

prop_time_minus_seconds :: TimeWithTime -> Int -> Property
prop_time_minus_seconds (TimeWithTime d) num
  = testIO $ do
  -- Add or subtract only a few years.
  let num' = num `rem` 3650
  let expected  = minusSeconds d num'

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_minus_seconds" retInt64
    r <- liftIO $ f [argWord64 $ packedOfTime d, argWord64 (fromIntegral num')]
    pure $ expected === timeOfPacked (fromIntegral r)

prop_time_minus_months :: TimeWithTime -> Int -> Property
prop_time_minus_months (TimeWithTime d) num
  = testIO $ do
  -- Add or subtract only a few years.
  let num' = num `rem` 120
  let expected = minusMonths d num'

  runRight $ do
    library <- readLibrary seaTestables
    f <- function library "testable_itime_minus_months" retInt64
    r <- liftIO $ f [argWord64 $ packedOfTime d, argWord64 (fromIntegral num')]
    pure $ expected === timeOfPacked (fromIntegral r)


runRight :: (Monad m, Show a) => EitherT a m Property -> m Property
runRight a = do
  e <- runEitherT a
  case e of
    Left  x -> return (counterexample (show x) failed)
    Right x -> return x

seaTestables :: SourceCode
seaTestables = codeOfDoc $ PP.vsep
  [ "iint_t testable_itime_to_epoch_days        (itime_t x)            { return itime_to_epoch_days      (x);    }"
  , "iint_t testable_itime_to_epoch_seconds     (itime_t x)            { return itime_to_epoch_seconds   (x);    }"
  , "iint_t testable_itime_from_epoch_days      (iint_t g, iint_t h, iint_t m, iint_t s)"
                                                                   <> "{ return itime_from_epoch_days (g,h,m,s); }"
  , "iint_t testable_itime_from_epoch_seconds   (iint_t g)             { return itime_from_epoch_seconds (g);    }"
  , "iint_t testable_itime_days_diff            (itime_t x, itime_t y) { return itime_days_diff          (x, y); }"
  , "iint_t testable_itime_seconds_diff         (itime_t x, itime_t y) { return itime_seconds_diff       (x, y); }"
  , "iint_t testable_itime_minus_days           (itime_t x, iint_t y)  { return itime_minus_days         (x, y); }"
  , "iint_t testable_itime_minus_seconds        (itime_t x, iint_t y)  { return itime_minus_seconds      (x, y); }"
  , "iint_t testable_itime_minus_months         (itime_t x, iint_t y)  { return itime_minus_months       (x, y); }"
  ]

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunMore checkArgs
