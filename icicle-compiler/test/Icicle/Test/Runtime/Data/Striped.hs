{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Runtime.Data.Striped where

import qualified Anemone.Foreign.Mempool as Mempool
import qualified Anemone.Foreign.Segv as Segv

import           Control.Monad.Catch (bracket)

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Foreign.Storable (Storable)

import           Disorder.Core.IO (testIO)
import           Disorder.Corpus (boats)
import           Disorder.Jack

import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Logical as Logical
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped
import           Icicle.Test.Arbitrary.Run

import           P

import           System.IO (IO)

import           Text.Show.Pretty (ppShow)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither)
import qualified X.Data.Vector.Cons as Cons


runEitherIO :: Show x => EitherT x IO a -> IO a
runEitherIO io = do
  e <- runEitherT io
  case e of
    Left err ->
      fail (show err)
    Right x ->
      pure x

storableOf :: Storable a => Jack a -> Jack (Storable.Vector a)
storableOf =
  fmap Storable.fromList . listOf

genBool64 :: Jack Bool64
genBool64 =
  elements [False64, True64]

genTime64 :: Jack Time64
genTime64 =
  Time64 <$> sizedBounded

genError64 :: Jack Error64
genError64 =
  elements [Tombstone64, Fold1NoValue64, CannotCompute64]

genField :: Jack a -> Jack (Field a)
genField g =
  Field <$> elements boats <*> g

genSchema :: Jack Schema
genSchema =
  oneOfRec [
      pure Schema.Unit
    , pure Schema.Bool
    , pure Schema.Int
    , pure Schema.Double
    , pure Schema.Time
    , pure Schema.String
    ] [
      Schema.Sum <$> genSchema <*> genSchema
    , Schema.Option <$> genSchema
    , Schema.Result <$> genSchema
    , Schema.Pair <$> genSchema <*> genSchema
    , Schema.Struct . Cons.unsafeFromList <$> listOfN 1 5 (genField genSchema)
    , Schema.Array <$> genSchema
    , Schema.Map <$> genSchema <*> genSchema
    ]

genValue :: Schema -> Jack Value
genValue = \case
  Schema.Unit ->
    pure Logical.Unit
  Schema.Bool ->
    Logical.Bool <$> genBool64
  Schema.Int ->
    Logical.Int <$> sizedBounded
  Schema.Double ->
    Logical.Double <$> arbitrary
  Schema.Time ->
    Logical.Time <$> genTime64
  Schema.String ->
    Logical.String . Char8.pack . filter (/= '\0') <$> arbitrary
  Schema.Sum x y ->
    oneOf [
        Logical.Left <$> genValue x
      , Logical.Right <$> genValue y
      ]
  Schema.Option x ->
    oneOf [
        pure Logical.None
      , Logical.Some <$> genValue x
      ]
  Schema.Result x ->
    oneOf [
        Logical.Error <$> genError64
      , Logical.Success <$> genValue x
      ]
  Schema.Pair x y ->
    Logical.Pair <$> genValue x <*> genValue y
  Schema.Struct fields ->
    Logical.Struct <$> traverse (genValue . fieldData) fields
  Schema.Array x ->
    Logical.Array . Boxed.fromList <$> listOfN 0 10 (genValue x)
  Schema.Map k v ->
    Logical.Map . Map.fromList <$> listOfN 0 10 ((,) <$> genValue k <*> genValue v)

genColumn :: Schema -> Jack Column
genColumn schema =
  justOf $
    rightToMaybe . Striped.fromLogical schema . Boxed.fromList <$> listOfN 0 20 (genValue schema)

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

return []
tests :: IO Bool
tests =
  $checkAllWith TestRunMore checkArgs
