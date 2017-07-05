{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Runtime.Data.Any (
    Any64(..)
  , from
  , read
  , unsafeFrom
  , unsafeRead
  , toString
  , toArray
  , fromString
  , fromArray

  , AnyError(..)
  , renderAnyError
  ) where

import           Anemone.Foreign.Mempool (Mempool)
import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as ByteString
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.Text as Text
import           Data.Word (Word8, Word64)

import           Foreign.C.Types (CSize(..))
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (Storable(..))

import           GHC.Generics (Generic)

import           Icicle.Runtime.Data.Array (Array)

import           P hiding (any)

import qualified Prelude as Savage

import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           X.Text.Show (gshowsPrec)


-- | The @iany_t@ runtime type.
--
newtype Any64 =
  Any64 {
      unAny64 :: Word64
    } deriving (Eq, Ord, Generic, Storable)

instance Show Any64 where
  showsPrec =
    gshowsPrec

data AnyError =
    Any64MustBeWordSize !Int
    deriving (Eq, Ord, Show)

renderAnyError :: AnyError -> Text
renderAnyError = \case
  Any64MustBeWordSize n ->
    "Value for any must be exactly 8 bytes, found value with <" <> Text.pack (show n) <> " bytes>"

valueSize :: Num a => a
valueSize =
  8
{-# INLINE valueSize #-}

checkValueSize :: Storable a => a -> Either AnyError ()
checkValueSize x = do
 if sizeOf x /= valueSize then
   Left $ Any64MustBeWordSize (sizeOf x)
 else
   pure ()
{-# INLINE checkValueSize #-}

unsafeFrom :: Storable a => a -> Any64
unsafeFrom !x =
  Any64 $! unsafePerformIO $! alloca $ \ptr -> do
    poke (castPtr ptr) x
    peek ptr
{-# INLINE unsafeFrom #-}

from :: forall a. Storable a => a -> Either AnyError Any64
from !x = do
  checkValueSize (Savage.undefined :: a)
  pure $! unsafeFrom x
{-# INLINE from #-}

unsafeRead :: Storable a => Any64 -> a
unsafeRead (Any64 any) =
  unsafePerformIO $! alloca $ \ptr -> do
    poke ptr any
    peek (castPtr ptr)
{-# INLINE unsafeRead #-}

read :: forall a. Storable a => Any64 -> Either AnyError a
read !any = do
  checkValueSize (Savage.undefined :: a)
  pure $! unsafeRead any
{-# INLINE read #-}

toString :: Any64 -> IO ByteString
toString any = do
  liftIO $! ByteString.packCString $! unsafeRead any
{-# INLINE toString #-}

fromString :: Mempool -> ByteString -> IO Any64
fromString pool (PS fp off len) =
  withForeignPtr fp $ \src -> do
    let
      !len_csize =
        fromIntegral len

    dst <- Mempool.allocBytes pool (len_csize + 1)

    c_memcpy dst (src `plusPtr` off) len_csize
    pokeByteOff dst len (0 :: Word8)

    pure $! unsafeFrom dst
{-# INLINE fromString #-}

toArray :: Any64 -> Array
toArray any =
  unsafeRead any
{-# INLINE toArray #-}

fromArray :: Array -> Any64
fromArray any =
  unsafeFrom any
{-# INLINE fromArray #-}

foreign import ccall unsafe "string.h memcpy"
  c_memcpy :: Ptr a -> Ptr a -> CSize -> IO ()
