{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Icicle.Common.NanEq (
    NanEq(..)
  , defaultNanEq

  , GNanEq(..)
  ) where

import           Data.Map (Map)
import qualified Data.Map.Lazy as Map

import           GHC.Generics
import           GHC.Exts

import           P

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
-- Add more instances here, or at the type definition. If you can derive Generic on your data
-- type, then it's simply a matter of 'instance NanEq XYZ' where XYZ is your
-- data type.
--

instance NanEq ()

instance (NanEq a, NanEq b) => NanEq (a, b)

instance NanEq a => NanEq (Maybe a)

instance NanEq a => NanEq [a]

instance (NanEq k, NanEq v) => NanEq (Map k v) where
  nanEq xs ys =
    nanEq (Map.toList xs) (Map.toList ys)

instance NanEq Char where
  nanEq =
    (==)

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

instance (NanEq a, NanEq b) => NanEq (Either a b)

