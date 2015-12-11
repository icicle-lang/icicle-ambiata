{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Icicle.Test.Sea.Text where

import qualified Icicle.Internal.Pretty as PP
import           Icicle.Test.Sea.Utils
import           Icicle.Test.Arbitrary ()

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either

import           Data.String (String)
import           Data.Word (Word64)

import           Disorder.Core.IO (testIO)

import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable

import           Jetski

import           P
import           Prelude (minBound, maxBound)

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           Test.QuickCheck (forAllProperties, quickCheckWithResult, stdArgs)
import           Test.QuickCheck (maxSuccess, forAll, choose)
import           Test.QuickCheck.Property (Property, (===), counterexample, failed, property)

import           X.Control.Monad.Trans.Either (firstEitherT)


prop_text_read_int :: Int64 -> Property
prop_text_read_int expected
 = testRead "testable_text_read_iint" (show expected) $ \actual ->
     actual === expected

prop_text_read_double :: Property
prop_text_read_double
 = forAll (choose (minBound, maxBound)) $ \(expected64 :: Word64) ->
   let expected = fromWord64 expected64
   in testRead "testable_text_read_idouble" (show expected) $ \actual ->
     let epsilon = if expected /= 0
                   then abs expected * 1.0e-14
                   else 0

         diff    = actual - expected

         equal   = isNaN expected && isNaN actual
                || expected == actual
                || diff < epsilon

     in counterexample ("expected   =  " <> show expected)
      $ counterexample ("actual     =  " <> show actual)
      $ counterexample ("difference =  " <> show diff)
      $ counterexample ("epsilon    =  " <> show epsilon)
      $ property equal

seaTestables :: SourceCode
seaTestables = codeOfDoc $ PP.vsep
  [ "ierror_msg_t from_loc (char *ps, char *pe, ierror_loc_t loc) {"
  , "    if (!loc) return 0;"
  , "    loc->line_start = ps;"
  , "    loc->line_end   = pe;"
  , "    return ierror_loc_pretty (loc, 1);"
  , "}"
  , "ierror_msg_t testable_text_read_iint (char *p, size_t n, iint_t *output_ptr) {"
  , "    return from_loc (p, p+n, text_read_iint (&p, p+n, output_ptr));"
  , "}"
  , "ierror_msg_t testable_text_read_idouble (char *p, size_t n, idouble_t *output_ptr) {"
  , "    segv_install_handler (0, 0);"
  , "    return from_loc (p, p+n, text_read_idouble (&p, p+n, output_ptr));"
  , "}"
  ]

------------------------------------------------------------------------

data TestError
 = SeaError    String
 | JetskiError JetskiError

testRead :: Storable a => Symbol -> String -> (a -> Property) -> Property
testRead symbol input onOutput = testIO . liftIO . runRight $ do
  lib <- firstEitherT JetskiError (readLibrary seaTestables)
  fn  <- firstEitherT JetskiError (function lib symbol (retPtr retCChar))

  (hoistEither =<<) . liftIO $
    withCString input $ \p -> do
    alloca $ \(outputPtr :: Ptr a) -> do
      let n = fromIntegral (length input)
      errorPtr <- liftIO $ fn [argPtr p, argCSize n, argPtr outputPtr]

      if errorPtr /= nullPtr
      then do
        msg <- liftIO (peekCString errorPtr)
        liftIO (free errorPtr)
        return (Left (SeaError msg))
      else do
        output <- peek outputPtr
        return (Right (counterexample ("input      = " <> show input)
                                      (onOutput output)))

runRight :: Monad m => EitherT TestError m Property -> m Property
runRight a = do
  e <- runEitherT a
  case e of
    Left (SeaError    x) -> return (counterexample x        failed)
    Left (JetskiError x) -> return (counterexample (show x) failed)
    Right x              -> return x

fromWord64 :: Word64 -> Double
fromWord64 w64 =
  unsafePerformIO . alloca $ \(ptr :: Ptr Double) ->
    poke (castPtr ptr) w64 >> peek ptr

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  -- $quickCheckAll
  $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10000 })
