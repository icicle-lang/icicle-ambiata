{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Runtime.Data.Mask (
    Mask(..)
  , isDrop
  , isKeep
  , maskList
  , maskBoxed
  , maskStorable
  , maskByteString
  , replicateMask
  ) where

import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Foreign.Storable (Storable)

import           P


data Mask =
    Drop
  | Keep
    deriving (Eq, Ord, Show)

instance Monoid Mask where
  mempty =
    Drop
  {-# INLINABLE mempty #-}

  mappend x y =
    if isDrop x && isDrop y then
      Drop
    else
      Keep
  {-# INLINABLE mappend #-}

isDrop :: Mask -> Bool
isDrop = \case
  Drop ->
    True
  Keep ->
    False
{-# INLINABLE isDrop #-}

isKeep :: Mask -> Bool
isKeep =
  not . isDrop
{-# INLINABLE isKeep #-}

maskList :: Boxed.Vector Mask -> [a] -> [a]
maskList mask xs =
  Boxed.toList . Boxed.map snd . Boxed.filter (isKeep . fst) $ Boxed.zip mask (Boxed.fromList xs)
{-# INLINABLE maskList #-}

maskBoxed :: Boxed.Vector Mask -> Boxed.Vector a -> Boxed.Vector a
maskBoxed mask xs =
  Boxed.map snd . Boxed.filter (isKeep . fst) $ Boxed.zip mask xs
{-# INLINABLE maskBoxed #-}

maskStorable :: Storable a => Boxed.Vector Mask -> Storable.Vector a -> Storable.Vector a
maskStorable mask xs =
  Storable.convert . Boxed.map snd . Boxed.filter (isKeep . fst) $ Boxed.zip mask (Boxed.convert xs)
{-# INLINABLE maskStorable #-}

maskByteString :: Boxed.Vector Mask -> ByteString -> ByteString
maskByteString mask (PS fp0 off0 len0) =
  let
    (fp, off, len) =
      Storable.unsafeToForeignPtr .
      maskStorable mask $
      Storable.unsafeFromForeignPtr fp0 off0 len0
  in
    PS fp off len
{-# INLINABLE maskByteString #-}

replicateMask :: Storable.Vector Int64 -> Boxed.Vector Mask -> Boxed.Vector Mask
replicateMask ns mask =
  Boxed.concatMap id $
  Boxed.zipWith Boxed.replicate (Storable.convert $ Storable.map fromIntegral ns) mask
{-# INLINABLE replicateMask #-}
