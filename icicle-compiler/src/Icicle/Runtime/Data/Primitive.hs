{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Icicle.Runtime.Data.Primitive (
    Unit64(..)
  , Bool64(..)
  , Error64(..)

  , Time64(..)
  , UnpackedTime64(..)

  , Field(..)

  , unit

  , pattern False64
  , pattern True64
  , fromBool
  , fromBool64

  , pattern NotAnError64
  , pattern Tombstone64
  , pattern Fold1NoValue64
  , pattern CannotCompute64
  , pattern NotANumber64
  , pattern IndexOutOfBounds64
  , fromExceptionInfo
  , fromError64
  , isError

  , renderTime
  , packTime
  , packTimeVector
  , unpackTime
  , unpackTimeVector
  , fromIvorySeconds
  ) where

import           Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as Storable
import           Data.Word (Word64, Word32, Word8)

import           Foreign.Ptr (castPtr)
import           Foreign.Storable (Storable(..))

import           GHC.Generics (Generic)

import           Icicle.Data.Time (packedOfTime, timeOfIvorySeconds)
import           Icicle.Common.Base (ExceptionInfo(..))

import           P

import           Text.Printf (printf)

import           X.Text.Show (gshowsPrec)


--
-- NOTE: iint_t and idouble_t are not defined here as they are represented on
-- the Haskell side by Int64 and Double, respectively.
--

-- | The @iunit_t@ runtime type.
--
newtype Unit64 =
  Unit64 {
      unUnit64 :: Word64
    } deriving (Eq, Ord, Generic, Storable)

instance Show Unit64 where
  showsPrec =
    gshowsPrec

-- | The @ibool_t@ runtime type.
--
newtype Bool64 =
  Bool64 {
      unBool64 :: Word64
    } deriving (Eq, Ord, Generic, Storable)

instance Show Bool64 where
  showsPrec =
    gshowsPrec

-- | The @itime_t@ runtime type.
--
newtype Time64 =
  Time64 {
      unTime64 :: Word64
    } deriving (Eq, Ord, Generic, Storable)

instance Show Time64 where
  showsPrec p =
    showsPrec p . unpackTime

-- | The @itime_t@ runtime type in unpacked form.
--
data UnpackedTime64 =
  UnpackedTime64 {
      timeYear :: !Int16
    , timeMonth :: !Word8
    , timeDay :: !Word8
    , timeSeconds :: !Word32
    } deriving (Eq, Ord, Generic)

instance Show UnpackedTime64 where
  showsPrec =
    gshowsPrec

instance Storable UnpackedTime64 where
  sizeOf _ =
    8
  {-# INLINE sizeOf #-}

  alignment _ =
    8
  {-# INLINE alignment #-}

  peek ptr =
    fmap unpackTime (peek (castPtr ptr))
  {-# INLINE peek #-}

  peekElemOff ptr off =
    fmap unpackTime (peekElemOff (castPtr ptr) off)
  {-# INLINE peekElemOff #-}

  peekByteOff ptr off =
    fmap unpackTime (peekByteOff (castPtr ptr) off)
  {-# INLINE peekByteOff #-}

  poke ptr x =
    poke (castPtr ptr) (packTime x)
  {-# INLINE poke #-}

  pokeElemOff ptr off x =
    pokeElemOff (castPtr ptr) off (packTime x)
  {-# INLINE pokeElemOff #-}

  pokeByteOff ptr off x =
    pokeByteOff (castPtr ptr) off (packTime x)
  {-# INLINE pokeByteOff #-}

-- | The @ierror_t@ runtime type.
--
newtype Error64 =
  Error64 {
      unError64 :: Word64
    } deriving (Eq, Ord, Generic, Storable)

instance Show Error64 where
  showsPrec =
    gshowsPrec

-- | A named struct field.
--
data Field a =
  Field {
      fieldName :: !Text
    , fieldData :: !a
    } deriving (Eq, Ord, Generic, Functor, Foldable, Traversable)

instance Show a => Show (Field a) where
  showsPrec =
    gshowsPrec

unit :: Unit64
unit =
  Unit64 0x1C31C13
{-# INLINE unit #-}

#if __GLASGOW_HASKELL__ >= 800
pattern False64 :: Bool64
#endif
pattern False64 =
  Bool64 0

#if __GLASGOW_HASKELL__ >= 800
pattern True64 :: Bool64
#endif
pattern True64 =
  Bool64 1

fromBool :: Bool -> Bool64
fromBool = \case
  False ->
    False64
  True ->
    True64
{-# INLINE fromBool #-}

fromBool64 :: Bool64 -> Bool
fromBool64 = \case
  False64 ->
    False
  _ ->
    True
{-# INLINE fromBool64 #-}

#if __GLASGOW_HASKELL__ >= 800
pattern NotAnError64 :: Error64
#endif
pattern NotAnError64 =
  Error64 0

#if __GLASGOW_HASKELL__ >= 800
pattern Tombstone64 :: Error64
#endif
pattern Tombstone64 =
  Error64 1

#if __GLASGOW_HASKELL__ >= 800
pattern Fold1NoValue64 :: Error64
#endif
pattern Fold1NoValue64 =
  Error64 2

#if __GLASGOW_HASKELL__ >= 800
pattern CannotCompute64 :: Error64
#endif
pattern CannotCompute64 =
  Error64 3

#if __GLASGOW_HASKELL__ >= 800
pattern NotANumber64 :: Error64
#endif
pattern NotANumber64 =
  Error64 4

#if __GLASGOW_HASKELL__ >= 800
pattern IndexOutOfBounds64 :: Error64
#endif
pattern IndexOutOfBounds64 =
  Error64 5

fromExceptionInfo :: ExceptionInfo -> Error64
fromExceptionInfo = \case
  ExceptNotAnError ->
    NotAnError64
  ExceptTombstone ->
    Tombstone64
  ExceptFold1NoValue ->
    Fold1NoValue64
  ExceptCannotCompute ->
    CannotCompute64
  ExceptNotANumber ->
    NotANumber64
  ExceptIndexOutOfBounds ->
    Tombstone64
{-# INLINE fromExceptionInfo #-}

isError :: Error64 -> Bool
isError = \case
  NotAnError64 ->
    False
  _ ->
    True
{-# INLINE isError #-}

fromError64 :: Error64 -> Maybe ExceptionInfo
fromError64 = \case
  NotAnError64 ->
    Just ExceptNotAnError
  Tombstone64 ->
    Just ExceptTombstone
  Fold1NoValue64 ->
    Just ExceptFold1NoValue
  CannotCompute64 ->
    Just ExceptCannotCompute
  NotANumber64 ->
    Just ExceptNotANumber
  IndexOutOfBounds64 ->
    Just ExceptIndexOutOfBounds
  _ ->
    Nothing
{-# INLINE fromError64 #-}

renderTime :: Time64 -> Text
renderTime x =
  let
    UnpackedTime64 year month day daySeconds =
      unpackTime x

    (dayMinutes, sec) =
      daySeconds `quotRem` 60

    (hour, minute) =
      dayMinutes `quotRem` 60
  in
    Text.pack $
      printf "%04d-%02d-%02d %02d:%02d:%02d" year month day hour minute sec

fromIvorySeconds :: Int64 -> Time64
fromIvorySeconds seconds =
  -- FIX don't depend on Icicle.Data.Time
  Time64 . packedOfTime $ timeOfIvorySeconds seconds
{-# INLINE fromIvorySeconds #-}

packTime :: UnpackedTime64 -> Time64
packTime x =
  Time64 $!
        unsafeShiftL (fromIntegral $ timeYear x) 48
    .|. unsafeShiftL (fromIntegral $ timeMonth x) 40
    .|. unsafeShiftL (fromIntegral $ timeDay x) 32
    .|. fromIntegral (timeSeconds x)
{-# INLINE packTime #-}

unpackTime :: Time64 -> UnpackedTime64
unpackTime (Time64 x) =
  UnpackedTime64 {
      timeYear =
        fromIntegral (unsafeShiftR x 48)
    , timeMonth =
        fromIntegral (unsafeShiftR x 40) .&. 0xff
    , timeDay =
        fromIntegral (unsafeShiftR x 32) .&. 0xff
    , timeSeconds =
        fromIntegral x .&. 0xffffffff
    }
{-# INLINE unpackTime #-}

packTimeVector :: Storable.Vector UnpackedTime64 -> Storable.Vector Time64
packTimeVector x =
  Storable.unsafeCast x
{-# INLINE packTimeVector #-}

unpackTimeVector :: Storable.Vector Time64 -> Storable.Vector UnpackedTime64
unpackTimeVector x =
  Storable.unsafeCast x
{-# INLINE unpackTimeVector #-}
