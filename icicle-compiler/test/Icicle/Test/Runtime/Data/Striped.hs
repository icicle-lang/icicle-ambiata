{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Data.Striped where

import qualified Anemone.Foreign.Mempool as Mempool
import qualified Anemone.Foreign.Segv as Segv

import           Control.Monad.Catch (bracket)

import qualified Data.Vector as Boxed

import           Disorder.Core.IO (testIO)
import           Disorder.Jack

import qualified Icicle.Runtime.Data.Striped as Striped
import           Icicle.Test.Arbitrary.Run
import           Icicle.Test.Gen.Runtime.Data

import           P

import           System.IO (IO)

import           Text.Show.Pretty (ppShow)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither)


runEitherIO :: Show x => EitherT x IO a -> IO a
runEitherIO io = do
  e <- runEitherT io
  case e of
    Left err ->
      fail (show err)
    Right x ->
      pure x

prop_roundtrip_striped_logical :: Property
prop_roundtrip_striped_logical =
  gamble genSchema $ \schema ->
  gamble (listOf $ genValue schema) $ \values0 ->
  testIO $ do
    column <- runEitherIO . hoistEither $ Striped.fromLogical schema (Boxed.fromList values0)
    values <- runEitherIO . hoistEither $ Striped.toLogical column
    pure $
      values0 === Boxed.toList values

prop_roundtrip_striped_arrays :: Property
prop_roundtrip_striped_arrays =
  gamble genSchema $ \schema ->
  gamble (genColumn schema) $ \column0 ->
  testIO .
  Segv.withSegv (ppShow column0) .
  bracket Mempool.create Mempool.free $ \pool -> do
    arrays <- runEitherIO $ Striped.toArrays pool column0
    column <- runEitherIO $ Striped.fromArrays pool schema arrays
    pure $
      column0 === column

prop_roundtrip_striped_anys :: Property
prop_roundtrip_striped_anys =
  gamble genSchema $ \schema ->
  gamble (genSingleton schema) $ \(_, column0) ->
  testIO .
  Segv.withSegv (ppShow column0) .
  bracket Mempool.create Mempool.free $ \pool -> do
    anys <- runEitherIO $ Striped.headAnys pool column0
    column <- runEitherIO $ Striped.fromAnys pool schema anys
    pure $
      column0 === column

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore checkArgs
