{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module Icicle.Test.Gen.Runtime.Data where

import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Foreign.Storable (Storable)

import           Disorder.Corpus (boats)
import           Disorder.Jack

import           Icicle.Runtime.Data
import qualified Icicle.Runtime.Data.Logical as Logical
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped

import           P

import qualified X.Data.Vector.Cons as Cons


storableVectorOf :: Storable a => Jack a -> Jack (Storable.Vector a)
storableVectorOf =
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
    Logical.String . Char8.pack . filter (/= '\0') <$> listOfN 0 20 arbitrary
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

genSingleton :: Schema -> Jack (Value, Column)
genSingleton schema =
  justOf $ do
    x <- genValue schema
    pure . fmap (x,) . rightToMaybe . Striped.fromLogical schema $ Boxed.singleton x
