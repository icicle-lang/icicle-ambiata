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

import           Disorder.Core.IO

import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable

import           Jetski

import           P

import           System.IO

import           Test.QuickCheck (forAllProperties, quickCheckWithResult, stdArgs, maxSuccess)
import           Test.QuickCheck.Property (Property, (===), counterexample, failed, property)

import           X.Control.Monad.Trans.Either (firstEitherT)


prop_text_read_int :: Int64 -> Property
prop_text_read_int expected
 = testRead "testable_text_read_iint" (show expected)
 $ \actual -> actual === expected

prop_text_read_double :: Double -> Property
prop_text_read_double expected
 = testRead "testable_text_read_idouble" (show expected)
 $ \actual -> let diff    = actual - expected
                  epsilon = 0.000000001
              in counterexample ("expected   = " <> show expected)
               $ counterexample ("actual     = " <> show actual)
               $ counterexample ("difference = " <> show diff)
               $ property (diff < epsilon)

seaTestables :: SourceCode
seaTestables = codeOfDoc $ PP.vsep
  [ "ierror_msg_t testable_text_read_iint (char *p, size_t n, iint_t *output_ptr) {"
  , "    return text_read_iint (&p, p + n, output_ptr);"
  , "}"
  , "ierror_msg_t testable_text_read_idouble (char *p, size_t n, idouble_t *output_ptr) {"
  , "    return text_read_idouble (&p, p + n, output_ptr);"
  , "}"
  ]

------------------------------------------------------------------------

data TestError
 = SeaError   String
 | JetskiError JetskiError

testRead :: Storable a => Symbol -> String -> (a -> Property) -> Property
testRead symbol input onOutput = testIO . runRight $ do
  lib <- firstEitherT JetskiError (readLibrary seaTestables)
  fn  <- firstEitherT JetskiError (function lib symbol (retPtr retCChar))

  res <- liftIO $
    withCStringLen input $ \(p, n) -> do
    alloca $ \(outputPtr :: Ptr a) -> do
      errorPtr <- liftIO $ fn [argPtr p, argCSize (fromIntegral n), argPtr outputPtr]

      if errorPtr /= nullPtr
      then do
        msg <- liftIO (peekCString errorPtr)
        liftIO (free errorPtr)
        return (Left (SeaError msg))
      else do
        output <- peek outputPtr
        return (Right (counterexample ("input = " <> show input) (onOutput output)))

  hoistEither res

runRight :: Monad m => EitherT TestError m Property -> m Property
runRight a = do
  e <- runEitherT a
  case e of
    Left (SeaError    x) -> return (counterexample x        failed)
    Left (JetskiError x) -> return (counterexample (show x) failed)
    Right x              -> return x

------------------------------------------------------------------------

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  -- $quickCheckAll
  $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 })
