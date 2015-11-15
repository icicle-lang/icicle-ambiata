{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Data.DateTime where

import           Icicle.Data.DateTime
import qualified Icicle.Internal.Pretty as PP
import           Icicle.Sea.Preamble (seaPreamble)
import           Icicle.Sea.Eval (compilerOptions)
import           Icicle.Test.Arbitrary ()

import           Control.Exception (finally)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either

import qualified Data.Text as T
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)

import           Disorder.Core.IO

import           Jetski

import           P

import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           Test.QuickCheck
import           Test.QuickCheck.Property

prop_date_symmetry :: DateTime -> Property
prop_date_symmetry d =
  d === dateOfPacked (packedOfDate d)

prop_date_sea_to_days :: DateTime -> Property
prop_date_sea_to_days d
  = testIO $ do
  let epochDate = unsafeDateOfYMD 1600 03 01
  let expected  = daysDifference epochDate d

  runRight $ do
    library <- readLibraryRef
    f <- function library "testable_idate_to_epoch" retInt
    r <- liftIO $ f [argWord64 $ packedOfDate d]
    pure $ expected === r

prop_date_sea_from_days :: DateTime -> Property
prop_date_sea_from_days d
  = testIO $ do
  let epochDate = unsafeDateOfYMD 1600 03 01
  let epochDiff = daysDifference epochDate d

  runRight $ do
    library <- readLibraryRef
    f <- function library "testable_idate_from_epoch" retInt
    r <- liftIO $ f [argWord64 $ fromIntegral epochDiff]
    pure $ d === dateOfPacked (fromIntegral r)

prop_date_symmetry_sea :: DateTime -> DateTime -> Property
prop_date_symmetry_sea d1 d2
  = testIO $ do
  let expected  = daysDifference d1 d2

  runRight $ do
    library <- readLibraryRef
    f <- function library "testable_idate_days_diff" retInt
    r <- liftIO $ f [argWord64 (packedOfDate d1), argWord64 (packedOfDate d2)]
    pure $ expected === fromIntegral r

prop_date_minus_days :: DateTime -> Int -> Property
prop_date_minus_days d num
  = testIO $ do
  -- Add or subtract only a few years.
  let num' = num `rem` 3650
  let expected  = minusDays d num'

  runRight $ do
    library <- readLibraryRef
    f <- function library "testable_idate_minus_days" retInt
    r <- liftIO $ f [argWord64 $ packedOfDate d, argWord64 (fromIntegral num')]
    pure $ expected === dateOfPacked (fromIntegral r)

prop_date_minus_months :: DateTime -> Int -> Property
prop_date_minus_months d num
  = testIO $ do
  -- Add or subtract only a few years.
  let num' = num `rem` 120
  let expected = minusMonths d num'

  runRight $ do
    library <- readLibraryRef
    f <- function library "testable_idate_minus_months" retInt
    r <- liftIO $ f [argWord64 $ packedOfDate d, argWord64 (fromIntegral num')]
    pure $ expected === dateOfPacked (fromIntegral r)


runRight :: (Monad m, Show a) => EitherT a m Property -> m Property
runRight a = do
  e <- runEitherT a
  case e of
    Left  x -> return (counterexample (show x) failed)
    Right x -> return x

code :: T.Text
code = textOfDoc (PP.vsep ["#define ICICLE_NO_PSV 1", seaPreamble, seaTestables])

seaTestables :: PP.Doc
seaTestables = PP.vsep
  [ "iint_t testable_idate_to_epoch     (idate_t x)            { return idate_to_epoch     (x);    }"
  , "iint_t testable_idate_from_epoch   (iint_t g)             { return idate_from_epoch   (g);    }"
  , "iint_t testable_idate_days_diff    (idate_t x, idate_t y) { return idate_days_diff    (x, y); }"
  , "iint_t testable_idate_minus_days   (idate_t x, iint_t y)  { return idate_minus_days   (x, y); }"
  , "iint_t testable_idate_minus_months (idate_t x, iint_t y)  { return idate_minus_months (x, y); }"
  ]

-- These C testing utils should perhaps be generalised and placed in their own module.

textOfDoc :: PP.Doc -> T.Text
textOfDoc doc = T.pack (PP.displayS (PP.renderPretty 0.8 80 (PP.pretty doc)) "")

libraryRef :: IORef (Maybe (Either JetskiError Library))
libraryRef = unsafePerformIO (newIORef Nothing)

readLibraryRef :: EitherT JetskiError IO Library
readLibraryRef = do
  mlib <- liftIO (readIORef libraryRef)
  case mlib of
    Just elib -> hoistEither elib
    Nothing   -> do
      elib <- liftIO (runEitherT (compileLibrary compilerOptions code))
      liftIO (writeIORef libraryRef (Just elib))
      hoistEither elib

releaseLibraryRef :: IO ()
releaseLibraryRef = do
  elib <- readIORef libraryRef
  case elib of
    Nothing          -> pure ()
    Just (Left  _)   -> pure ()
    Just (Right lib) -> do
      writeIORef libraryRef Nothing
      releaseLibrary lib

return []
tests :: IO Bool
tests = flip finally releaseLibraryRef $ do
  -- $quickCheckAll
  $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
