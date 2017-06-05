{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Icicle.Test.Arbitrary.NanEq (
    (=~=)

  , NanEq(..)
  , defaultNanEq

  , GNanEq(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map.Lazy as Map

import           Disorder.Jack.Property (Property, counterexample)
import           Disorder.Jack.Property.Diff (renderDiffs)

import           Icicle.Common.Base (OutputName, BaseValue, StructField, ExceptionInfo, FactIdentifier)
import           Icicle.Data (Time, Namespace)

import           GHC.Generics
import           GHC.Exts

import           P

import           Text.Show.Pretty (ppShow)
import qualified Text.Show.Pretty as Pretty

--------------------------------------------------------------------------------

class GNanEq f where
  gnanEq :: f a -> f a -> Bool

class NanEq a where
  nanEq :: a -> a -> Bool

  default nanEq :: (Generic a, GNanEq (Rep a)) => a -> a -> Bool
  nanEq =
    defaultNanEq

defaultNanEq :: (Generic a, GNanEq (Rep a)) => a -> a -> Bool
defaultNanEq x y =
  gnanEq (from x) (from y)

--------------------------------------------------------------------------------
-- GNanEq

instance GNanEq U1 where
  gnanEq _ _ =
    True

instance (NanEq c) => GNanEq (K1 i c) where
  gnanEq (K1 x) (K1 y) =
    nanEq x y

instance (GNanEq a) => GNanEq (M1 i c a) where
  gnanEq (M1 x) (M1 y) =
    gnanEq x y

instance (GNanEq a, GNanEq b) => GNanEq (a :+: b) where
  gnanEq (L1 x) (L1 y) =
    gnanEq x y

  gnanEq (R1 x) (R1 y) =
    gnanEq x y

  gnanEq _ _ =
    False

instance (GNanEq a, GNanEq b) => GNanEq (a :*: b) where
  gnanEq (x1 :*: x2) (y1 :*: y2) =
    gnanEq x1 y1 &&
    gnanEq x2 y2

--------------------------------------------------------------------------------
-- NanEq

--
-- Add more instances here as necessary. If you can derive Generic on your data
-- type, then it's simply a matter of 'instance NanEq XYZ' where XYZ is your
-- data type.
--

instance (NanEq a, NanEq b) => NanEq (a, b)

instance NanEq a => NanEq [a]

instance (NanEq k, NanEq v) => NanEq (Map k v) where
  nanEq xs ys =
    nanEq (Map.toList xs) (Map.toList ys)

instance NanEq Bool where
  nanEq =
    (==)

instance NanEq Int where
  nanEq =
    (==)

-- It's all for this :)
instance NanEq Double where
  nanEq x y =
    x == y || (isNaN x && isNaN y)

instance NanEq Text where
  nanEq =
    (==)

instance NanEq Time where
  nanEq =
    (==)

instance NanEq OutputName

instance NanEq Namespace where
  nanEq =
    (==)

instance NanEq ExceptionInfo

instance NanEq StructField

instance NanEq FactIdentifier

instance NanEq BaseValue

--------------------------------------------------------------------------------
-- QuickCheck / Jack

infix 4 =~=

(=~=) :: (NanEq a, Show a) => a -> a -> Property
(=~=) x0 y0 =
  let
    render =
      case (Pretty.reify x0, Pretty.reify y0) of
        (Just x, Just y) ->
          renderDiffs x y
        _ ->
          ppShow x0 <>
          " =/= " <>
          ppShow y0
  in
    counterexample "=== Not NaN equal ===" $
    counterexample render (nanEq x0 y0)
