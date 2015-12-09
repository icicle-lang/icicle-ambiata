{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}
module Icicle.Benchmark.Generator (
    FileSpec(..)
  , AttributeType(..)
  , Seed
  , Year
  , generateSparse
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad ()

import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Time.Calendar (isLeapYear)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import           Data.Word (Word8)

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Marshal.Utils (copyBytes)
import           Foreign.Ptr (plusPtr)
import           Foreign.Storable

import           P

import           Prelude (minBound, maxBound)

import           System.IO (BufferMode(..), hSetBuffering)
import           System.IO (IO, FilePath)
import           System.IO (IOMode(..), withBinaryFile)
import           System.IO.Unsafe (unsafePerformIO)

import qualified Test.QuickCheck.Arbitrary as QC
import           Test.QuickCheck.Gen (Gen(..))
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC

import           Text.Printf (printf)

------------------------------------------------------------------------

type Seed = Int
type Year = Int

data AttributeType = Double | Int
  deriving (Eq, Ord, Show)

data FileSpec = FileSpec {
    specSeed         :: Seed
  , specEntityCount  :: Int32
  , specAttributes   :: Map ByteString AttributeType
  , specStart        :: Year
  , specEnd          :: Year
  } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

generateSparse :: FileSpec -> FilePath -> IO ()
generateSparse spec path = do
  let (leap, nonLeap) =
        generate (specSeed spec) $
        (,) <$> createTemplate spec Leap
            <*> createTemplate spec NonLeap

  withBinaryFile path WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering (Just (4*1024*1024)))

    for_ [0..specEntityCount spec - 1] $ \entity -> do
      eLeap    <- replaceEntity entity leap
      eNonLeap <- replaceEntity entity nonLeap
      for_ [specStart spec..specEnd spec] $ \year -> do
        bs  <- replaceYear year (eLeap, eNonLeap)
        -- bs' <- addCorruption (specSeed spec) 10000 bs
        C.hPut h bs

replaceEntity :: Int32 -> (U.Vector Int, U.Vector Int, ByteString) -> IO (U.Vector Int, ByteString)
replaceEntity entity (eixs, yixs, bs0) = do
  let ebs = C.pack (printf "%010d" entity)
  bs1 <- replaceBytes ebs eixs bs0
  return (yixs, bs1)

replaceYear :: Int -> ((U.Vector Int, ByteString), (U.Vector Int, ByteString)) -> IO ByteString
replaceYear year (leap, nonLeap)
  | isLeapYear' year = go leap
  | otherwise        = go nonLeap
  where
    go (yixs, bs) = do
      let ybs = C.pack (printf "%04d" year)
      replaceBytes ybs yixs bs

generate :: Seed -> Gen a -> a
generate seed (MkGen g) =
  let size = 30
  in g (QC.mkQCGen seed) size

------------------------------------------------------------------------

createTemplate :: FileSpec -> IsLeap -> Gen (U.Vector Int, U.Vector Int, ByteString)
createTemplate spec leap = do
  records <- templateOfLeap spec leap

  let template = C.concat (V.toList records)

      startIxs = U.prescanl'  (+) 0 . U.convert . V.map C.length $ records
      endIxs   = U.postscanl' (+) 0 . U.convert . V.map C.length $ records

      timeSize = 21
      yearIxs  = U.map (subtract timeSize) endIxs

  return (startIxs, yearIxs, template)

templateOfLeap :: FileSpec -> IsLeap -> Gen (V.Vector ByteString)
templateOfLeap spec leap = do
  let times = timesOfYear leap
      n     = V.length times

  eavs <- V.fromListN n <$> QC.vectorOf n (genEAV spec)

  return . V.map (L.toStrict . B.toLazyByteString)
         $ V.zipWith (<>) eavs times

timesOfYear :: IsLeap -> V.Vector Builder
timesOfYear leap =
  V.concatMap timeOfYear' (V.zip (V.enumFromTo 1 12) (daysInMonths leap))

timeOfYear' :: (Int8, Int8) -> V.Vector Builder
timeOfYear' (month, ndays) = do
  day     <- V.enumFromTo 1 ndays
  hour    <- V.enumFromTo 0 23
  minute  <- V.enumFromTo 0 59
  return $ "YYYY-"
        <> bInt8Dec2 month  <> "-"
        <> bInt8Dec2 day    <> "T"
        <> bInt8Dec2 hour   <> ":"
        <> bInt8Dec2 minute <> ":00Z\n"

bInt8Dec2 :: Int8 -> Builder
bInt8Dec2 x | x > 9     = ""  <> B.int8Dec x
            | otherwise = "0" <> B.int8Dec x

genEAV :: FileSpec -> Gen Builder
genEAV spec = do
  let genDouble = QC.arbitrarySizedFractional :: Gen Double
      genInt    = QC.arbitrarySizedIntegral   :: Gen Int

  e      <- QC.choose (0, specEntityCount spec - 1)
  (a, t) <- QC.elements (Map.toList (specAttributes spec))
  v      <- case t of
              Double -> B.string7 . show <$> genDouble
              Int    -> B.intDec <$> genInt

  return ("EEEEEEEEEE|" <> B.byteString a <> "|" <> v <> "|")

------------------------------------------------------------------------

data IsLeap = Leap | NonLeap

isLeapYear' :: Year -> Bool
isLeapYear' y =
  isLeapYear (fromIntegral y)

daysInMonths :: IsLeap -> V.Vector Int8
daysInMonths leap =
  let feb = case leap of
        Leap    -> 29
        NonLeap -> 28
  in V.fromList [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

------------------------------------------------------------------------

replaceBytes :: ByteString -> U.Vector Int -> ByteString -> IO ByteString
replaceBytes replacement ixs bs = do
  let (PS fpRep0 offRep lenRep) = replacement
      copy@(PS fp0 off _)       = C.copy bs

  withForeignPtr fp0 $ \ptr0 ->
    withForeignPtr fpRep0 $ \ptrRep0 -> do

      let ptr    = ptr0   `plusPtr` off
          ptrRep = ptrRep0 `plusPtr` offRep

      U.forM_ ixs $ \ix ->
        copyBytes (ptr `plusPtr` ix) ptrRep lenRep

  return copy

replaceDigits :: U.Vector (Int, Word8) -> ByteString -> IO ByteString
replaceDigits reps bs = do
  let copy@(PS fp0 off _) = C.copy bs

  withForeignPtr fp0 $ \ptr0 -> do
    let ptr = ptr0 `plusPtr` off
    U.forM_ reps $ \(i, r) -> do
      let pi = ptr `plusPtr` i
      -- only overwrite digits
      (c :: Word8) <- peek pi
      when (c >= 0x30 && c <= 0x39)
           (poke pi r)

  return copy

addCorruption :: Seed -> Int -> ByteString -> IO ByteString
addCorruption seed n bs = do
  let index  = QC.choose (0, C.length bs)
      digits = U.fromListN n . List.take n $ List.cycle [0x30..0x39]

  let ixs = generate seed (U.fromListN n <$> QC.vectorOf n index)
      reps = U.zip ixs digits

  replaceDigits reps bs
