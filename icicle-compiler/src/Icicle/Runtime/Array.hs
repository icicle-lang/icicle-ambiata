{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Runtime.Array (
    Array(..)
  , ArrayIndex(..)
  , ArrayLength(..)
  , ArrayCapacity(..)
  , ArrayFootprint(..)

  , new
  , length
  , read
  , write
  , grow
  , fromVector
  , fromList
  , toVector
  , toList

  , unsafeRead
  , unsafeWrite
  , unsafeFromMVector
  , unsafeFromVector
  , unsafeToMVector
  , unsafeToVector

  , ArrayError(..)
  , renderArrayError

  -- * Internal
  , headerSize
  , elementSize
  , calculateCapacity
  , calculateFootprint
  , unsafeWriteLength
  , unsafeLengthPtr
  , unsafeElementPtr
  , unsafeAllocate
  , checkElementSize
  , checkBounds
  ) where

import           Anemone.Foreign.Mempool (Mempool)
import qualified Anemone.Foreign.Mempool as Mempool

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Primitive (PrimState)

import           Data.Bits (countLeadingZeros, shiftL)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Storable.Mutable as MStorable
import           Data.Void (Void)

import           Foreign.C.Types (CSize(..))
import           Foreign.ForeignPtr (newForeignPtr_)
import           Foreign.Ptr (Ptr, plusPtr, castPtr)
import           Foreign.Storable (Storable(..))

import           GHC.Generics (Generic)

import           P hiding (length, toList)

import qualified Prelude as Savage

import           System.IO (IO)

import           X.Text.Show (gshowsPrec)
import           X.Control.Monad.Trans.Either (EitherT, left)


-- | An Icicle runtime array.
--
newtype Array =
  Array {
      unArray :: Ptr Void
    } deriving (Eq, Ord, Generic, Storable)

-- | An index in to an array.
--
newtype ArrayIndex =
  ArrayIndex {
      unArrayIndex :: Int64
    } deriving (Eq, Ord, Enum, Num, Real, Integral, Generic, Storable)

-- | The number of elements in the array.
--
newtype ArrayLength =
  ArrayLength {
      unArrayCount :: Int64
    } deriving (Eq, Ord, Enum, Num, Real, Integral, Generic, Storable)

-- | The number of elements the array can store without needing to allocate.
--
newtype ArrayCapacity =
  ArrayCapacity {
      unArrayCapacity :: Int64
    } deriving (Eq, Ord, Enum, Num, Real, Integral, Generic, Storable)

-- | The number of bytes the array occupies in memory, including the header.
--
newtype ArrayFootprint =
  ArrayFootprint {
      unArrayFootprint :: Int64
    } deriving (Eq, Ord, Enum, Num, Real, Integral, Generic, Storable)

data ArrayError =
    ArrayElementsMustBeWordSize !Int
  | ArrayIndexOutOfBounds !ArrayIndex !ArrayLength
    deriving (Eq, Ord, Show)

renderArrayError :: ArrayError -> Text
renderArrayError = \case
  ArrayElementsMustBeWordSize n ->
    "Array elements must be exactly 8 bytes, found element with <" <> Text.pack (show n) <> " bytes>"
  ArrayIndexOutOfBounds (ArrayIndex ix) (ArrayLength len) ->
    "Array index <" <> Text.pack (show ix) <> "> was out of bounds [0, " <> Text.pack (show len) <> ")"

instance Show Array where
  showsPrec =
    gshowsPrec

instance Show ArrayIndex where
  showsPrec =
    gshowsPrec

instance Show ArrayLength where
  showsPrec =
    gshowsPrec

instance Show ArrayCapacity where
  showsPrec =
    gshowsPrec

instance Show ArrayFootprint where
  showsPrec =
    gshowsPrec

headerSize :: Num a => a
headerSize =
  8
{-# INLINE headerSize #-}

elementSize :: Num a => a
elementSize =
  8
{-# INLINE elementSize #-}

-- | Calculate an array's capacity, from its current element count.
--
calculateCapacity :: ArrayLength -> ArrayCapacity
calculateCapacity (ArrayLength len) =
  if len < 4 then
    4
  else
    let
      !bits =
        64 - countLeadingZeros (len - 1)
    in
      ArrayCapacity (1 `shiftL` bits)
{-# INLINE calculateCapacity #-}

calculateFootprint :: ArrayCapacity -> ArrayFootprint
calculateFootprint (ArrayCapacity capacity) =
  ArrayFootprint $
    headerSize + elementSize * capacity
{-# INLINE calculateFootprint #-}

unsafeWriteLength :: Array -> ArrayLength -> IO ()
unsafeWriteLength (Array ptr) (ArrayLength len) =
  pokeByteOff ptr 0 len
{-# INLINE unsafeWriteLength #-}

unsafeLengthPtr :: Array -> Ptr Int64
unsafeLengthPtr (Array ptr) =
  castPtr ptr
{-# INLINE unsafeLengthPtr #-}

unsafeElementPtr :: Array -> Ptr a
unsafeElementPtr (Array ptr) =
  ptr `plusPtr` headerSize
{-# INLINE unsafeElementPtr #-}

unsafeAllocate :: Mempool -> ArrayFootprint -> IO Array
unsafeAllocate pool bytes =
  Array <$> Mempool.callocBytes pool (fromIntegral bytes) 1
{-# INLINE unsafeAllocate #-}

-- | Allocate an initialised array of the specified length.
--
new :: Mempool -> ArrayLength -> IO Array
new pool len = do
  let
    !bytes =
      calculateFootprint (calculateCapacity len)

  array <- unsafeAllocate pool bytes
  unsafeWriteLength array len

  pure array
{-# INLINABLE new #-}

length :: Array -> IO ArrayLength
length =
  fmap ArrayLength . peek . unsafeLengthPtr
{-# INLINE length #-}

checkElementSize :: Storable a => a -> EitherT ArrayError IO ()
checkElementSize x = do
 if sizeOf x /= elementSize then
   left $ ArrayElementsMustBeWordSize (sizeOf x)
 else
   pure ()
{-# INLINE checkElementSize #-}

checkBounds :: Array -> ArrayIndex -> EitherT ArrayError IO ()
checkBounds array (ArrayIndex ix) = do
  ArrayLength n <- liftIO $ length array
  if ix >= 0 && ix < n then
    pure ()
  else
    left $ ArrayIndexOutOfBounds (ArrayIndex ix) (ArrayLength n)
{-# INLINE checkBounds #-}

unsafeRead :: Storable a => Array -> ArrayIndex -> IO a
unsafeRead array (ArrayIndex ix) =
  peekByteOff (unsafeElementPtr array) (fromIntegral (elementSize * ix))
{-# INLINE unsafeRead #-}

read :: forall a. Storable a => Array -> ArrayIndex -> EitherT ArrayError IO a
read array ix = do
  checkElementSize (Savage.undefined :: a)
  checkBounds array ix
  liftIO $ unsafeRead array ix
{-# INLINE read #-}

unsafeWrite :: Storable a => Array -> ArrayIndex -> a -> IO ()
unsafeWrite array (ArrayIndex ix) x = do
  pokeByteOff (unsafeElementPtr array) (fromIntegral (elementSize * ix)) x
{-# INLINE unsafeWrite #-}

write :: forall a. Storable a => Array -> ArrayIndex -> a -> EitherT ArrayError IO ()
write array ix x = do
  checkElementSize x
  checkBounds array ix
  liftIO $ unsafeWrite array ix x
{-# INLINE write #-}

grow :: Mempool -> Array -> ArrayLength -> IO Array
grow pool arrayOld@(Array ptrOld) lengthNew = do
  lengthOld <- length arrayOld

  let
    !capacityOld =
      calculateCapacity lengthOld

    !capacityNew =
      calculateCapacity lengthNew

  if capacityOld >= capacityNew then do
    unsafeWriteLength arrayOld lengthNew
    pure arrayOld
  else do
    let
      !bytesOld =
        calculateFootprint capacityOld

      !bytesNew =
        calculateFootprint capacityNew

    arrayNew@(Array ptrNew) <- unsafeAllocate pool bytesNew

    c_memcpy ptrNew ptrOld (fromIntegral bytesOld)
    unsafeWriteLength arrayNew lengthNew

    pure arrayNew
{-# INLINABLE grow #-}

unsafeToMVector :: Storable a => Array -> IO (MStorable.MVector (PrimState IO) a)
unsafeToMVector array = do
  !n <- length array
  !fp <- newForeignPtr_ (unsafeElementPtr array)
  pure $! MStorable.unsafeFromForeignPtr0 fp (fromIntegral n)
{-# INLINE unsafeToMVector #-}

unsafeToVector :: Storable a => Array -> IO (Storable.Vector a)
unsafeToVector array =
  Storable.unsafeFreeze =<<! unsafeToMVector array
{-# INLINE unsafeToVector #-}

toVector :: forall a. Storable a => Array -> EitherT ArrayError IO (Storable.Vector a)
toVector array = do
  checkElementSize (Savage.undefined :: a)
  liftIO $!
    Storable.freeze =<<! unsafeToMVector array
{-# INLINE toVector #-}

seqList :: [a] -> b -> b
seqList xs0 o =
  case xs0 of
    [] ->
      o
    x : xs ->
      x `seq` xs `seqList` o
{-# INLINE seqList #-}

unsafeToList :: Storable a => Array -> IO [a]
unsafeToList array = do
  xs <- Storable.toList <$> unsafeToVector array
  xs `seqList` pure xs
{-# INLINE unsafeToList #-}

toList :: forall a. Storable a => Array -> EitherT ArrayError IO [a]
toList array = do
  checkElementSize (Savage.undefined :: a)
  liftIO $! unsafeToList array
{-# INLINE toList #-}

unsafeFromMVector :: Storable a => Mempool -> MStorable.MVector (PrimState IO) a -> IO Array
unsafeFromMVector pool vector = do
  let
    !n =
      MStorable.length vector

  !array <- new pool (fromIntegral n)

  let
    !dst =
      unsafeElementPtr array

  MStorable.unsafeWith vector $ \src ->
    c_memcpy dst src (fromIntegral (n * elementSize))

  pure array
{-# INLINABLE unsafeFromMVector #-}

unsafeFromVector :: Storable a => Mempool -> Storable.Vector a -> IO Array
unsafeFromVector pool vector =
  unsafeFromMVector pool =<<! Storable.unsafeThaw vector
{-# INLINE unsafeFromVector #-}

fromVector :: forall a. Storable a => Mempool -> Storable.Vector a -> EitherT ArrayError IO Array
fromVector pool vector = do
  checkElementSize (Savage.undefined :: a)
  liftIO $! unsafeFromVector pool vector
{-# INLINE fromVector #-}

fromList :: forall a. Storable a => Mempool -> [a] -> EitherT ArrayError IO Array
fromList pool xs =
  fromVector pool $! Storable.fromList xs
{-# INLINE fromList #-}

foreign import ccall unsafe "string.h memcpy"
  c_memcpy :: Ptr a -> Ptr a -> CSize -> IO ()
