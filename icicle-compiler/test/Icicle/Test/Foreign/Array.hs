{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE BangPatterns      #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Icicle.Test.Foreign.Array where

import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (bracket)
import           X.Control.Monad.Trans.Either (bracketEitherT')

import qualified Data.List as List

import           Disorder.Core.IO

import           Jetski

import           Foreign

import           P

import           System.IO

import           Test.QuickCheck

import           Icicle.Sea.Eval.Base (getCompilerOptions)
import           Icicle.Internal.Pretty
import           Icicle.Test.Sea.Utils
import           Icicle.Test.Arbitrary

import           Icicle.Test.Foreign.Utils

import qualified Anemone.Foreign.Mempool as Mempool

-- array_eq (x, x) == true
prop_array_eq_refl input
  = testIO $ runProp input name_eq $ \_ ptr -> do
    return [ ptr, ptr]

name_eq
  = fn "eq"

test_eq input
  = "ibool_t "
  <> pretty name_eq
  <> " (" <> tt <> " *xPtr" <> ", " <> tt <> " *yPtr) { "
  <> tt <> " x = *xPtr;"
  <> tt <> " y = *yPtr;"
  <> "return " <> ta <> "_eq(x,y);"
  <> "}"
  where
    (ta, tt) = arrayType input

-- array_eq (x, y) == array_eq (y, x)
prop_array_eq_comm input1
  = forAll (listOf $ genValForType $ inputType input1) $ \vals2 ->
    testIO $ runProp input1 name_eq_comm $ \pool ptr1 -> do
    ptr2 <- allocInput pool (Input (inputType input1) vals2)
    return [ ptr1, ptr2 ]

name_eq_comm
  = fn "eq_comm"

test_eq_comm input
  = "ibool_t "
  <> pretty name_eq_comm
  <> " (" <> tt <> " *xPtr" <> ", " <> tt <> " *yPtr) { "
  <> tt <> " x = *xPtr;"
  <> tt <> " y = *yPtr;"
  <> "ibool_t r1 = " <> ta <> "_eq(x,y);"
  <> "ibool_t r2 = " <> ta <> "_eq(y,x);"
  <> "return (r1 && r2) || (!r1 && !r2);"
  <> "}"
  where
    (ta, tt) = arrayType input

-- array_copy (x) == x
prop_array_copy_eq input
  = testIO $ runProp input name_copy_eq $ \pool ptr ->
    return [ argMempool pool, ptr ]

name_copy_eq
  = fn "copy_eq"

test_copy_eq input
  = "ibool_t "
  <> pretty name_copy_eq
  <> " (anemone_mempool_t *mempool, " <> tt <> " *xPtr) { "
  <> tt <> " x = *xPtr;"
  <> tt <> " y = " <> ta <> "_copy(mempool, x);"
  <> "ibool_t r = " <> ta <> "_eq(x,y);"
  <> "return r;"
  <> "}"
  where
    (ta, tt) = arrayType input

-- y = array_copy (x)
-- z = array_put_mutable (y, i, a)
-- array_eq (x, z) == false
prop_array_put_mutable_diff
  = forAll arbitrary $ \input -> not (univalue (inputType input)) ==>
    forAll arbitrary $ \i -> i >= 0 && i <= length (inputArr input) ==>
    forAll (genValForType (inputType input)) $ \v -> putTestCond input i v ==>
    testIO $ runProp input name_mutable_diff $ \pool ptr -> do
      vptr <- allocValuePtr pool v
      return [ argMempool pool, ptr, argInt64 (fromIntegral i), vptr ]

name_mutable_diff
  = fn "put_mutable_diff"

test_mutable_diff input
  = "ibool_t "
  <> pretty name_mutable_diff
  <> " (anemone_mempool_t *mempool, " <> tt <> " *xPtr, iint_t i, " <> ty <> "_t *vPtr) { "
  <> tt <> " x = *xPtr;"
  <> ty <> "_t v = *vPtr;"
  <> tt  <> " y = " <> ta <> "_copy(mempool, x);"
  <> tt  <> " z = " <> ta <> "_put_mutable(mempool, y, i, v);"
  <> "ibool_t r = " <> ta <> "_eq(x,z);"
  <> "return !r;"
  <> "}"
  where
    ty = seaOfType (inputType input)
    (ta, tt) = arrayType input

-- y = array_copy (x)
-- z = array_put_immutable (x, i, a)
-- array_eq (x, y) == true
prop_array_put_immutable_copy
  = forAll arbitrary $ \input -> not (univalue (inputType input)) ==>
    forAll arbitrary $ \i -> i >= 0 && i <= length (inputArr input) ==>
    forAll (genValForType (inputType input)) $ \v -> putTestCond input i v ==>
    testIO $ runProp input name_immutable_copy $ \pool ptr -> do
      vptr <- allocValuePtr pool v
      return [ argMempool pool, ptr, argInt64 (fromIntegral i), vptr ]

name_immutable_copy
  = fn "put_immutable_copy"

test_immutable_copy input
  = "ibool_t " <> pretty name_immutable_copy
  <> " (anemone_mempool_t *mempool, " <> tt <> " *xPtr, iint_t i, " <> ty <> "_t *vPtr) { "
  <> tt <> " x = *xPtr;"
  <> ty <> "_t v = *vPtr;"
  <> tt  <> " y = " <> ta <> "_copy(mempool, x);"
  <> tt  <> " z = " <> ta <> "_put_immutable(mempool, x, i, v);"
  <> "ibool_t r = " <> ta <> "_eq(x,y);"
  <> "return r;"
  <> "}"
  where
    ty = seaOfType (inputType input)
    (ta, tt) = arrayType input


-- swap (swap (x, i, j), j, i) == x
prop_array_swap_swap_eq input
  = forAll (arbitrary :: Gen Int) $ \i -> i >= 0 && i < length (inputArr input) ==>
    forAll (arbitrary :: Gen Int) $ \j -> j >= 0 && j < length (inputArr input) ==>
    testIO $ runProp input name_swap_swap_eq $ \pool ptr -> 
      return [ argMempool pool, ptr, argInt64 (fromIntegral i), argInt64 (fromIntegral j) ]

name_swap_swap_eq
  = fn "swap_swap_eq"

test_swap_swap_eq input
  = "ibool_t "
  <> pretty name_swap_swap_eq
  <> " (anemone_mempool_t *mempool, " <> tt <> " *xPtr, iint_t i, iint_t j) {"
  <> tt <> " x = *xPtr;"
  <> tt  <> " y = " <> ta <> "_copy(mempool, x);"
  <> ta <> "_swap(x, i, j);"
  <> ta <> "_swap(x, j, i);"
  <> "ibool_t r = " <> ta <> "_eq(y,x);"
  <> "return r;"
  <> "}"
  where
    (ta, tt) = arrayType input

-- y = array_copy (x)
-- z = array_immutable_delete (x, i)
-- array_eq (x, y) == true
prop_array_delete_copy
  = forAll arbitrary $ \input -> not (univalue (inputType input)) ==>
    forAll arbitrary $ \i -> i >= 0 && i < length (inputArr input) ==>
    testIO $ runProp input name_delete_copy $ \pool ptr -> 
      return [ argMempool pool, ptr, argInt64 (fromIntegral i) ]

name_delete_copy
  = fn "delete_copy"

test_delete_copy t
  = "ibool_t "
  <> pretty name_delete_copy
  <> " (anemone_mempool_t *mempool, " <> tt <> " *xPtr, iint_t i) { "
  <> tt <> " x = *xPtr;"
  <> tt  <> " y = " <> ta <> "_copy(mempool, x);"
  <> tt  <> " z = " <> ta <> "_delete (mempool, x, i);"
  <> "ibool_t r = " <> ta <> "_eq(x,y);"
  <> "return r;"
  <> "}"
  where (ta, tt) = arrayType t

-- y = array_immutable_delete (x, i)
-- y->count == x->count - 1
prop_array_delete_len input
  = forAll arbitrary $ \i -> i >= 0 && i < length (inputArr input) ==>
    testIO $ runProp input name_delete_len $ \pool ptr -> 
      return [ argMempool pool, ptr, argInt64 (fromIntegral i) ]

name_delete_len
  = fn "delete_len"

test_delete_len input
  = "ibool_t " <> pretty name_delete_len
  <> " (anemone_mempool_t *mempool, " <> tt <> " *xPtr, iint_t i) { "
  <> tt <> " x = *xPtr;"
  <> tt  <> " y = " <> ta <> "_delete (mempool, x, i);"
  <> "ibool_t r = y->count == x->count - 1;"
  <> "return r;"
  <> "}"
  where (ta, tt) = arrayType input

--------------------------------------------------------------------------------

fn :: Text -> Text
fn f = "test_" <> f

arrayType :: Input -> (Doc, Doc)
arrayType input
  = let ta = "iarray_" <> pretty (seaOfType (inputType input))
        tt = ta <> "_t"
    in (ta, tt)

putTestCond :: Input -> Int -> Val -> Bool
putTestCond input i v
  | i == length (inputArr input) = True
  | otherwise = v /= (inputArr input) List.!! i

univalue :: Ty -> Bool
univalue ty = case ty of
  IError -> True
  IUnit  -> True
  _      -> False

runProp :: Input -> Text -> (Mempool.Mempool -> Argument -> IO [Argument]) -> IO Property
runProp input fun args = do
  let fs = seprops input
  bracket (liftIO Mempool.create) (liftIO . Mempool.free) $ \pool -> runRight $ do
  defaultOpts <- getCompilerOptions
  let opts = ["-DICICLE_ASSERT=1"] <> defaultOpts
  -- Cache the output, because we're likely to reuse the same type in another test 
  bracketEitherT' (compileLibrary CacheLibrary opts fs) releaseLibrary $ \lib -> runRight $ do
    ptr   <- liftIO $ allocInput pool input
    f     <- function lib fun retBool
    args' <- liftIO $ args pool ptr
    r     <- liftIO $ f args'
    return $ property r

seprops :: Input -> SourceCode
seprops input = codeOfDoc $ vsep
  ( fmap (\t -> "MAKE_ARRAY(" <> seaOfType t <> ")") (allSubArrays $ inputType input) <> 
  [ test_eq input
  , test_eq_comm input
  , test_copy_eq input
  , test_mutable_diff input
  , test_immutable_copy input
  , test_swap_swap_eq input
  , test_delete_copy input
  , test_delete_len input
  ])

argMempool :: Mempool.Mempool -> Argument
argMempool (Mempool.Mempool ptr) = argPtr ptr

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunMore checkArgs
