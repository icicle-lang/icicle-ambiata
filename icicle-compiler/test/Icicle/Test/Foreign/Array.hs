{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE BangPatterns      #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Test.Foreign.Array where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.List as List

import           Foreign
import           Foreign.LibFFI (RetType)
import           Foreign.LibFFI.Base (mkStorableRetType)
import           Foreign.LibFFI.FFITypes (ffi_type_uint64)
import           Foreign.LibFFI.Types as X

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Property

import qualified Prelude as Savage

import           Disorder.Corpus
import           Disorder.Core.IO

import           P

import           X.Control.Monad.Trans.Either

import           Jetski

import qualified Icicle.Common.Base as Icicle
import           Icicle.Data.Time
import           Icicle.Internal.Pretty
import           Icicle.Test.Sea.Utils
import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Arbitrary



-- array_eq (x, x) == true
prop_array_eq_refl input
  = testIO
  $ runProp input "eq"
  $ \ptr -> [ argPtr ptr, argPtr ptr]

-- array_eq (x, y) == array_eq (y, x)
prop_array_eq_comm input1 input2
  = testIO
  $ runWith input1
  $ \ptr1
  -> runProp input2 "eq_comm"
  $ \ptr2
  -> [ argPtr (wordPtrToPtr ptr1), argPtr ptr2 ]

-- array_copy (x) == x
prop_array_copy_eq input
  = testIO
  $ runProp input "copy_eq"
  $ \ptr -> [ argPtr ptr ]

-- y = array_copy (x)
-- z = array_put_mutable (x, i, a)
-- array_eq (x, y) == false
prop_array_put_mutable_diff
  = forAll arbitrary $ \input -> not (univalue (inputType input)) ==>
    forAll arbitrary $ \i -> i >= 0 && i < length (inputArr input) ==>
    forAll (genValForType (inputType input)) $ \v -> putTestCond input i v ==> testIO $ do
      (vptr, vfree) <- pokeVal v
      ret <- runProp input "put_mutable_diff"
           $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i), argPtr (wordPtrToPtr vptr) ]
      vfree
      return ret

-- y = array_copy (x)
-- z = array_put_immutable (x, i, a)
-- array_eq (x, y) == true
prop_array_put_immutable_copy
  = forAll arbitrary $ \input -> not (univalue (inputType input)) ==>
    forAll arbitrary $ \i -> i >= 0 && i <= length (inputArr input) ==>
    forAll (genValForType (inputType input)) $ \v -> putTestCond input i v ==> testIO $ do
      (vptr, vfree) <- pokeVal v
      ret <- runProp input "put_immutable_copy"
           $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i), argPtr (wordPtrToPtr vptr) ]
      vfree
      return ret

-- swap (swap (x, i, j), j, i) == x
prop_array_swap_swap_eq input
  = forAll (arbitrary :: Gen Int) $ \i -> i >= 0 && i < length (inputArr input) ==>
    forAll (arbitrary :: Gen Int) $ \j -> j >= 0 && j < length (inputArr input) ==>
      testIO $ runProp input "swap_swap_eq"
             $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i), argInt64 (fromIntegral j) ]

-- y = array_copy (x)
-- z = array_immutable_delete (x, i)
-- array_eq (x, y) == true
prop_array_delete_copy
  = forAll arbitrary $ \input -> not (univalue (inputType input)) ==>
    forAll arbitrary $ \i -> i >= 0 && i < length (inputArr input) ==>
      testIO $ runProp input "delete_copy"
             $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i) ]

-- y = array_immutable_delete (x, i)
-- y->count == x->count - 1
prop_array_delete_len input
  = forAll arbitrary $ \i -> i >= 0 && i < length (inputArr input) ==>
      testIO $ runProp input "delete_len"
             $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i) ]


putTestCond input i v
  | i == length (inputArr input) = True
  | otherwise = v /= (inputArr input) List.!! i

univalue ty = case ty of
  IError -> True
  IUnit  -> True
  _      -> False

-- savagery

runProp :: Input -> Text -> (Ptr a -> [Arg]) -> IO Property
runProp input fun args = do
  let ty = inputType input
  let fn = seaTestFunName fun
  let fs = seprops ty

  runWith input $ \wptr -> runRight $ do
    let ptr = wordPtrToPtr wptr
    src <- readLibrary fs
    f   <- function src fn retBool
    r   <- liftIO $ f $ args ptr
    arr <- liftIO $ peekArr ty wptr
    return $ counterexample ("array after test: " <> show arr)
           $ property r

seprops :: Ty -> SourceCode
seprops t = codeOfDoc $ vsep
  [ "MAKE_ARRAY(" <> ty <> ")"

  , "ibool_t " <> fn "eq"
               <> " (" <> tt <> " x" <> ", " <> tt <> " y) { "
               <> "return " <> ta <> "_eq(x,y);"
               <> "}"

  , "ibool_t " <> fn "eq_comm"
               <> " (" <> tt <> " x" <> ", " <> tt <> " y) { "
               <> "ibool_t r1 = " <> ta <> "_eq(x,y);"
               <> "ibool_t r2 = " <> ta <> "_eq(y,x);"
               <> "return (r1 && r2) || (!r1 && !r2);"
               <> "}"

  , "ibool_t " <> fn "copy_eq"
               <> " (" <> tt <> " x) { "
               <> "imempool_t *test_mempool = imempool_create();"
               <> tt <> " y = " <> ta <> "_copy(test_mempool, x);"
               <> "ibool_t r = " <> ta <> "_eq(x,y);"
               <> "imempool_free(test_mempool);"
               <> "return r;"
               <> "}"

  , "ibool_t " <> fn "put_mutable_diff"
               <> " (" <> tt <> " x, iint_t i, " <> ty <> "_t v) { "
               <> "imempool_t *test_mempool = imempool_create();"
               <> tt  <> " y = " <> ta <> "_copy(test_mempool, x);"
               <> tt  <> " z = " <> ta <> "_put_mutable(test_mempool, x, i, v);"
               <> "ibool_t r = " <> ta <> "_eq(x,y);"
               <> "imempool_free(test_mempool);"
               <> "return !r;"
               <> "}"

  , "ibool_t " <> fn "put_immutable_copy"
               <> " (" <> tt <> " x, iint_t i, " <> ty <> "_t v) { "
               <> "imempool_t *test_mempool = imempool_create();"
               <> tt  <> " y = " <> ta <> "_copy(test_mempool, x);"
               <> tt  <> " z = " <> ta <> "_put_immutable(test_mempool, x, i, v);"
               <> "ibool_t r = " <> ta <> "_eq(x,y);"
               <> "imempool_free(test_mempool);"
               <> "return r;"
               <> "}"

  , "ibool_t " <> fn "swap_swap_eq"
               <> " (" <> tt <> " x, iint_t i, iint_t j) {"
               <> "imempool_t *test_mempool = imempool_create();"
               <> tt  <> " y = " <> ta <> "_copy(test_mempool, x);"
               <> ta <> "_swap(x, i, j);"
               <> ta <> "_swap(x, j, i);"
               <> "ibool_t r = " <> ta <> "_eq(y,x);"
               <> "imempool_free(test_mempool);"
               <> "return r;"
               <> "}"

  , "ibool_t " <> fn "delete_copy"
               <> " (" <> tt <> " x, iint_t i) { "
               <> "imempool_t *test_mempool = imempool_create();"
               <> tt  <> " y = " <> ta <> "_copy(test_mempool, x);"
               <> tt  <> " z = " <> ta <> "_delete (test_mempool, x, i);"
               <> "ibool_t r = " <> ta <> "_eq(x,y);"
               <> "imempool_free(test_mempool);"
               <> "return r;"
               <> "}"

  , "ibool_t " <> fn "delete_len"
               <> " (" <> tt <> " x, iint_t i) { "
               <> "imempool_t *test_mempool = imempool_create();"
               <> tt  <> " y = " <> ta <> "_delete (test_mempool, x, i);"
               <> "ibool_t r = y->count == x->count - 1;"
               <> "imempool_free(test_mempool);"
               <> "return r;"
               <> "}"

  ]
  where
    fn = pretty . seaTestFunName
    ty = pretty (seaOfType t)
    ta = "iarray_" <> ty
    tt = ta <> "_t"

--------------------------------------------------------------------------------

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

seaTestFunName :: Text -> Text
seaTestFunName fn
   = "test_" <> fn

seaOfType :: Ty -> Text
seaOfType ty = case ty of
  IDouble  -> "idouble"
  IInt     -> "iint"
  IError   -> "ierror"
  IBool    -> "ibool"
  ITime    -> "itime"
  IString  -> "istring"
  IUnit    -> "iunit"
  IArray t -> "iarray_" <> seaOfType t

runWith :: Input -> (WordPtr -> IO Property) -> IO Property
runWith (Input _ val) prop = go val
  where
    go xs = do
      (array, afree) <- pokeArr xs
      !r <- prop array
      afree
      return r

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

retBool :: RetType Bool
retBool = mkStorableRetType ffi_type_uint64


return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunMore checkArgs
