{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE BangPatterns      #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Test.Foreign.Utils where

import qualified Data.List as List

import           Disorder.Corpus

import           Foreign

import           Jetski (Return, retInt64)

import           P

import qualified Prelude as Savage

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property

import           X.Control.Monad.Trans.Either

import           Icicle.Internal.Pretty
import qualified Icicle.Common.Base as Icicle
import           Icicle.Data.Time
import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Arbitrary ()


data Ty
  = IDouble
  | IInt
  | IError
  | IBool
  | ITime
  | IString
  | IUnit
  | IArray Ty
  deriving (Show)

data Val
  = VDouble Double
  | VInt    Int
  | VError  Icicle.ExceptionInfo
  | VBool   Bool
  | VTime   Time
  | VString [Char]
  | VUnit
  | VArray  [Val]
  deriving (Show, Eq)

data Input = Input
  { inputType :: Ty
  , inputArr  :: [Val]
  } deriving (Show)

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
    -> return $ VError Icicle.ExceptTombstone
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

peekArr :: Ty -> WordPtr -> IO Val
peekArr t wptr
  = let ptr = wordPtrToPtr wptr
        arr p i len acc
          | i > len = return acc
          | otherwise = do
              w <- peekElemOff p i
              x <- peekVal t w
              arr p (i+1) len (acc <> [x])
    in do len <- peekByteOff ptr 0
          VArray <$> arr ptr 1 len []

peekVal :: Ty -> WordPtr -> IO Val
peekVal ty wptr = case ty of
  IInt
    -> VInt <$> peekElemOff (wordPtrToPtr wptr) 0
  IDouble
    -> VDouble <$> peekElemOff (wordPtrToPtr wptr) 0
  IError
    -> return (VError Icicle.ExceptTombstone)
  IBool
    -> VBool <$> peekElemOff (wordPtrToPtr wptr) 0
  ITime
    -> VTime . timeOfPacked <$> peekElemOff (wordPtrToPtr wptr) 0
  IUnit
    -> return VUnit
  IString
    -> let str p i acc = do
             c <- peekElemOff p i
             if c == '\0' then return acc else str p (i+1) (acc <> [c])
       in VString <$> str (wordPtrToPtr wptr) 0 []
  IArray t
    -> peekArr t wptr

pokeVal :: Val -> IO (WordPtr, IO ())
pokeVal v = case v of
  VInt x
    -> poke64 (fromIntegral x :: Word64)
  VDouble x
    -> poke64 x
  VError _
    -> poke64 (0 :: Word64)
  VBool x
    -> let b :: Word64
           b = if x then 1 else 0
       in  poke64 b
  VTime x
    -> poke64 (packedOfTime x)
  VUnit
    -> poke64 (0 :: Word64)
  VString xs
    -> pokeStr xs
  VArray xs
    -> pokeArr xs

poke64 :: Storable a => a -> IO (WordPtr, IO ())
poke64 x = do
  ptr <- mallocBytes 8
  pokeElemOff ptr 0 x
  return (ptrToWordPtr ptr, free ptr)

pokeStr :: [Char] -> IO (WordPtr, IO ())
pokeStr xs = do
  ptr <- mallocBytes (sizeOf 'c' * (length xs + 1))
  zipWithM_ (pokeElemOff ptr) [0..] xs
  pokeElemOff ptr (length xs) '\0'
  return (ptrToWordPtr ptr, free ptr)

pokeArr :: [Val] -> IO (WordPtr, IO ())
pokeArr xs = do
  ptr <- mallocBytes (sizeOf (Savage.undefined :: WordPtr) * (length xs + 1))
  pokeByteOff ptr 0 (length xs)
  (ps, fs) <- List.unzip <$> mapM pokeVal xs
  zipWithM_ (pokeElemOff ptr) [1..] ps
  return . (ptrToWordPtr ptr,) $ foldr (>>) (return ()) fs

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
