{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Data.DateTime where

import           Icicle.Data.DateTime
import           Icicle.Test.Arbitrary ()
import qualified Icicle.Internal.Pretty as PP

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either

import           P
import           Disorder.Core.IO

import           System.IO
import qualified Data.Text as T
import           Test.QuickCheck
import           Test.QuickCheck.Property

import           Jetski

prop_date_symmetry :: DateTime -> Property
prop_date_symmetry d =
  d === unpackWord64 (packWord64 d)

prop_date_sea_to_days :: DateTime -> Property
prop_date_sea_to_days d
  = testIO $ do
  let epochDate = unsafeDateOfYMD 1600 03 01
  let expected  = daysDifference epochDate d

  runRight $ withLibrary [] code $ \library -> do
    f <- function library "idate_to_epoch" retInt
    r <- liftIO $ f [argWord64 $ packWord64 d]
    pure $ expected === r

prop_date_sea_from_days :: DateTime -> Property
prop_date_sea_from_days d
  = testIO $ do
  let epochDate = unsafeDateOfYMD 1600 03 01
  let epochDiff = daysDifference epochDate d

  runRight $ withLibrary [] code $ \library -> do
    f <- function library "idate_from_epoch" retInt
    r <- liftIO $ f [argWord64 $ fromIntegral epochDiff]
    pure $ d === unpackWord64 (fromIntegral r)

prop_date_symmetry_sea :: DateTime -> DateTime -> Property
prop_date_symmetry_sea d1 d2
  = testIO $ do
  let expected  = daysDifference d1 d2

  runRight $ withLibrary [] code $ \library -> do
    f <- function library "idate_days_diff" retInt
    r <- liftIO $ f [argWord64 (packWord64 d1), argWord64 (packWord64 d2)]
    pure $ expected === fromIntegral r

prop_date_minus_days :: DateTime -> Int -> Property
prop_date_minus_days d num
  = testIO $ do
  -- Add or subtract only a few years.
  let num' = num `rem` 3650
  let expected  = minusDays d num'

  runRight $ withLibrary [] code $ \library -> do
    f <- function library "idate_minus_days" retInt
    r <- liftIO $ f [argWord64 $ packWord64 d, argWord64 (fromIntegral num')]
    pure $ expected === unpackWord64 (fromIntegral r)

prop_date_minus_months :: DateTime -> Int -> Property
prop_date_minus_months d num
  = testIO $ do
  -- Add or subtract only a few years.
  let num' = num `rem` 120
  let expected = minusMonths d num'

  runRight $ withLibrary [] code $ \library -> do
    f <- function library "idate_minus_months" retInt
    r <- liftIO $ f [argWord64 $ packWord64 d, argWord64 (fromIntegral num')]
    pure $ expected === unpackWord64 (fromIntegral r)


runRight :: (Functor m, Show a)
         => EitherT a m Property
         -> m Property
runRight a =
  (\case
    Right x -> x
    Left  x -> (counterexample . show) x $ failed
  ) <$> (runEitherT a)

code :: T.Text
code = textOfDoc $ seaTypes PP.</> seaDateFunctions

seaTypes :: PP.Doc
seaTypes = PP.vsep
  [ "#include <stdbool.h>"
  , "#include <stdint.h>"
  , "#include <math.h>"
  , ""
  , "typedef uint64_t ierror_t;"
  , "typedef uint64_t iunit_t;"
  , "typedef uint64_t ibool_t;"
  , "typedef int64_t iint_t;"
  , "typedef double idouble_t;"
  , "typedef int64_t idate_t;"
  , ""
  , "static const ierror_t ierror_tombstone              = 0;"
  , "static const ierror_t ierror_fold1_no_value         = 1;"
  , "static const ierror_t ierror_variable_not_available = 2;"
  , ""
  , "static const iunit_t iunit  = 0x1c1c13;"
  , "static const ibool_t ifalse = 0;"
  , "static const ibool_t itrue  = 1;"
  , ""
  , "#define INLINE __attribute__((always_inline))"
  , ""
  ]

textOfDoc :: PP.Doc -> T.Text
textOfDoc doc = T.pack (PP.displayS (PP.renderPretty 0.8 80 (PP.pretty doc)) "")

return []
tests :: IO Bool
-- tests = $quickCheckAll
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 20 })
