{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE BangPatterns      #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Test.Foreign.Utils where

import qualified Data.List as List

import           Disorder.Corpus

import           Foreign

import           Jetski (Return, retInt64, Argument, argPtr)

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property

import           X.Control.Monad.Trans.Either

import           Icicle.Internal.Pretty
import           Icicle.Data.Time
import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Arbitrary ()

import qualified Anemone.Foreign.Mempool as Mempool

data Ty
  = IDouble
  | IInt
  | IError
  | IBool
  | ITime
  | IString
  | IUnit
  | IArray Ty
  deriving (Show, Eq)

data Val
  = VDouble Double
  | VInt    Int
  | VErrorTombstone
  | VBool   Bool
  | VTime   Time
  | VString [Char]
  | VUnit
  | VArray  [Val]
  deriving (Show, Eq)

data Input = Input
  { inputType :: Ty
  , inputArr  :: [Val]
  }

instance Show Input where
  show (Input ty vs) =
    "Input: " <> show ty <> "\n" <>
    "       " <> show vs

instance Arbitrary Ty where
  arbitrary = oneof_sized
    (fmap pure [IDouble, IInt, IError, IBool, ITime, IString, IUnit])
    [IArray <$> arbitrary]

instance Arbitrary Input where
  arbitrary = genInput

genValForType :: Ty -> Gen Val
genValForType ty = case ty of
  IDouble
    -> VDouble <$> arbitrary
  IInt
    -> VInt <$> arbitrary
  IBool
    -> VBool <$> arbitrary
  IError
    -> return $ VErrorTombstone
  ITime
    -> VTime <$> arbitrary
  IString
    -> VString <$> elements simpsons
  IUnit
    -> return VUnit
  IArray t
    -> VArray . List.take 5 <$> listOf (genValForType t)

genInput :: Gen Input
genInput = do
  ty   <- arbitrary
  val  <- listOf (genValForType ty)
  return $ Input ty val

seaOfType :: Ty -> Doc
seaOfType ty = case ty of
  IDouble  -> "idouble"
  IInt     -> "iint"
  IError   -> "ierror"
  IBool    -> "ibool"
  ITime    -> "itime"
  IString  -> "istring"
  IUnit    -> "iunit"
  IArray t -> "iarray_" <> seaOfType t

allSubArrays :: Ty -> [Ty]
allSubArrays ty = case ty of
  IArray t' -> allSubArrays t' <> [ty]
  _         -> [ty]

--------------------------------------------------------------------------------

pokeVal :: Mempool.Mempool -> WordPtr -> Val -> IO ()
pokeVal mempool ptr v = case v of
  VInt x
    -> poke ptr' (fromIntegral x :: Word64)
  VDouble x
    -> poke (wordPtrToPtr ptr) x
  VErrorTombstone
    -> poke ptr' (1 :: Word64)
  VBool x
    -> let b :: Word64
           b = if x then 1 else 0
       in  poke ptr' b
  VTime x -> poke ptr' (packedOfTime x)
  VUnit
    -> poke ptr' (0 :: Word64)
  VString xs
    -> pokeStr mempool ptr xs
  VArray xs
    -> pokeArr mempool ptr xs
 where
  ptr' = wordPtrToPtr ptr :: Ptr Word64

pokeStr :: Mempool.Mempool -> WordPtr -> [Char] -> IO ()
pokeStr mempool ptrRef xs = do
  let bytes = fromIntegral $ (length xs + 1)
  ptr <- Mempool.allocBytes mempool bytes
  zipWithM_ (pokeByteOff ptr) [0..] xs
  pokeByteOff ptr (length xs) '\0'
  poke (wordPtrToPtr ptrRef) ptr

pokeArr :: Mempool.Mempool -> WordPtr -> [Val] -> IO ()
pokeArr mempool ptrRef xs = do
  -- needs to be rounded up to the nearest power of two because the iarray_* functions assume that
  let
    roundUp len n
      | len >= n
      = roundUp len (2*n)
      | otherwise
      = n
  let alloc = fromIntegral $ roundUp (8 * length xs) (4 * 8)
  let len = fromIntegral (length xs) :: Word64
  -- Include length header after rounding up
  ptr <- Mempool.allocBytes mempool (alloc + 8)
  pokeByteOff ptr 0 len
  let ptrOff off = ptrToWordPtr $ plusPtr ptr (off * 8)
  zipWithM_ (\off v -> pokeVal mempool (ptrOff off) v) [1..] xs
  poke (wordPtrToPtr ptrRef) ptr

allocValuePtr :: Mempool.Mempool -> Val -> IO Argument
allocValuePtr mempool v = do
  ptr <- Mempool.allocBytes mempool 8
  pokeVal mempool (ptrToWordPtr ptr) v
  return $ argPtr ptr

allocInput :: Mempool.Mempool -> Input -> IO Argument
allocInput mempool (Input _ vs) = allocValuePtr mempool (VArray vs)

--------------------------------------------------------------------------------

runRight :: (Monad m, Show a) => EitherT a m Property -> m Property
runRight a = do
  e <- runEitherT a
  case e of
    Left  x -> return (counterexample (show x) failed)
    Right x -> return x

retBool :: Return Bool
retBool =
  (/= 0) <$> retInt64
