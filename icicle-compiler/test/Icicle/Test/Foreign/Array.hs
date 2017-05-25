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

import qualified Data.List as List

import           Disorder.Core.IO

import           Jetski

import           Foreign

import           P

import           System.IO

import           Test.QuickCheck

import           Icicle.Internal.Pretty
import           Icicle.Test.Sea.Utils
import           Icicle.Test.Arbitrary

import           Icicle.Test.Foreign.Utils


-- array_eq (x, x) == true
prop_array_eq_refl input
  = testIO
  $ runProp input name_eq
  $ \ptr -> [ argPtr ptr, argPtr ptr]

name_eq
  = fn "eq"

test_eq input
  = "ibool_t "
  <> pretty name_eq
  <> " (" <> tt <> " x" <> ", " <> tt <> " y) { "
  <> "return " <> ta <> "_eq(x,y);"
  <> "}"
  where
    (ta, tt) = arrayType input

-- array_eq (x, y) == array_eq (y, x)
prop_array_eq_comm input1 input2
  = testIO
  $ runWith input1
  $ \ptr1
  -> runProp input2 name_eq_comm
  $ \ptr2
  -> [ argPtr (wordPtrToPtr ptr1), argPtr ptr2 ]

name_eq_comm
  = fn "eq_comm"

test_eq_comm input
  = "ibool_t "
  <> pretty name_eq_comm
  <> " (" <> tt <> " x" <> ", " <> tt <> " y) { "
  <> "ibool_t r1 = " <> ta <> "_eq(x,y);"
  <> "ibool_t r2 = " <> ta <> "_eq(y,x);"
  <> "return (r1 && r2) || (!r1 && !r2);"
  <> "}"
  where
    (ta, tt) = arrayType input

-- array_copy (x) == x
prop_array_copy_eq input
  = testIO
  $ runProp input name_copy_eq
  $ \ptr -> [ argPtr ptr ]

name_copy_eq
  = fn "copy_eq"

test_copy_eq input
  = "ibool_t "
  <> pretty name_copy_eq
  <> " (" <> tt <> " x) { "
  <> "anemone_mempool_t *test_mempool = anemone_mempool_create();"
  <> tt <> " y = " <> ta <> "_copy(test_mempool, x);"
  <> "ibool_t r = " <> ta <> "_eq(x,y);"
  <> "anemone_mempool_free(test_mempool);"
  <> "return r;"
  <> "}"
  where
    (ta, tt) = arrayType input

-- y = array_copy (x)
-- z = array_put_mutable (y, i, a)
-- array_eq (x, z) == false
-- TODO flakey segfaulty test -- the values are always as expected but it might segfault
prop_array_put_mutable_diff
  = forAll arbitrary $ \input -> not (univalue (inputType input)) ==>
    forAll arbitrary $ \i -> i >= 0 && i <= length (inputArr input) ==>
    forAll (genValForType (inputType input)) $ \v -> putTestCond input i v ==> testIO $ do
      bracket (pokeVal v) snd $ \(vptr, _) ->
        runProp input name_mutable_diff
          $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i), argPtr (wordPtrToPtr vptr) ]

name_mutable_diff
  = fn "put_mutable_diff"

test_mutable_diff input
  = "ibool_t "
  <> pretty name_mutable_diff
  <> " (" <> tt <> " x, iint_t i, " <> ty <> "_t v) { "
  <> "anemone_mempool_t *test_mempool = anemone_mempool_create();"
  <> tt  <> " y = " <> ta <> "_copy(test_mempool, x);"
  <> tt  <> " z = " <> ta <> "_put_mutable(test_mempool, y, i, v);"
  <> "ibool_t r = " <> ta <> "_eq(x,z);"
  <> "anemone_mempool_free(test_mempool);"
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
    forAll (genValForType (inputType input)) $ \v -> putTestCond input i v ==> testIO $ do
      bracket (pokeVal v) snd $ \(vptr, _) ->
        runProp input name_immutable_copy
          $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i), argPtr (wordPtrToPtr vptr) ]

name_immutable_copy
  = fn "put_immutable_copy"

test_immutable_copy input
  = "ibool_t " <> pretty name_immutable_copy
  <> " (" <> tt <> " x, iint_t i, " <> ty <> "_t v) { "
  <> "anemone_mempool_t *test_mempool = anemone_mempool_create();"
  <> tt  <> " y = " <> ta <> "_copy(test_mempool, x);"
  <> tt  <> " z = " <> ta <> "_put_immutable(test_mempool, x, i, v);"
  <> "ibool_t r = " <> ta <> "_eq(x,y);"
  <> "anemone_mempool_free(test_mempool);"
  <> "return r;"
  <> "}"
  where
    ty = seaOfType (inputType input)
    (ta, tt) = arrayType input


-- swap (swap (x, i, j), j, i) == x
prop_array_swap_swap_eq input
  = forAll (arbitrary :: Gen Int) $ \i -> i >= 0 && i < length (inputArr input) ==>
    forAll (arbitrary :: Gen Int) $ \j -> j >= 0 && j < length (inputArr input) ==> do
      testIO
        $ runProp input name_swap_swap_eq
        $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i), argInt64 (fromIntegral j) ]

name_swap_swap_eq
  = fn "swap_swap_eq"

test_swap_swap_eq input
  = "ibool_t "
  <> pretty name_swap_swap_eq
  <> " (" <> tt <> " x, iint_t i, iint_t j) {"
  <> "anemone_mempool_t *test_mempool = anemone_mempool_create();"
  <> tt  <> " y = " <> ta <> "_copy(test_mempool, x);"
  <> ta <> "_swap(x, i, j);"
  <> ta <> "_swap(x, j, i);"
  <> "ibool_t r = " <> ta <> "_eq(y,x);"
  <> "anemone_mempool_free(test_mempool);"
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
      testIO
        $ runProp input name_delete_copy
        $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i) ]

name_delete_copy
  = fn "delete_copy"

test_delete_copy t
  = "ibool_t "
  <> pretty name_delete_copy
  <> " (" <> tt <> " x, iint_t i) { "
  <> "anemone_mempool_t *test_mempool = anemone_mempool_create();"
  <> tt  <> " y = " <> ta <> "_copy(test_mempool, x);"
  <> tt  <> " z = " <> ta <> "_delete (test_mempool, x, i);"
  <> "ibool_t r = " <> ta <> "_eq(x,y);"
  <> "anemone_mempool_free(test_mempool);"
  <> "return r;"
  <> "}"
  where (ta, tt) = arrayType t

-- y = array_immutable_delete (x, i)
-- y->count == x->count - 1
prop_array_delete_len input
  = forAll arbitrary $ \i -> i >= 0 && i < length (inputArr input) ==> do
      testIO
        $ runProp input name_delete_len
        $ \ptr -> [ argPtr ptr, argInt64 (fromIntegral i) ]

name_delete_len
  = fn "delete_len"

test_delete_len input
  = "ibool_t " <> pretty name_delete_len
  <> " (" <> tt <> " x, iint_t i) { "
  <> "anemone_mempool_t *test_mempool = anemone_mempool_create();"
  <> tt  <> " y = " <> ta <> "_delete (test_mempool, x, i);"
  <> "ibool_t r = y->count == x->count - 1;"
  <> "anemone_mempool_free(test_mempool);"
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

runWith :: Input -> (WordPtr -> IO Property) -> IO Property
runWith (Input _ val) prop = go val
  where
    go xs = bracket (pokeArr xs) snd (prop . fst)

runProp :: Input -> Text -> (Ptr a -> [Argument]) -> IO Property
runProp input fun args = do
  -- let ty = inputType input
  let fs = seprops input
  runWith input $ \wptr -> runRight $ do
    let ptr = wordPtrToPtr wptr
    src <- readLibraryWith ["-DICICLE_ASSERT=1"] fs
    f   <- function src fun retBool
    r   <- liftIO $ f $ args ptr
    -- arr <- liftIO $ peekArr ty wptr
    -- return $ counterexample ("array after test:\n" <> show arr)
    return $ property r

seprops :: Input -> SourceCode
seprops input = codeOfDoc $ vsep
  [ "MAKE_ARRAY(" <> seaOfType t <> ")"
  , test_eq input
  , test_eq_comm input
  , test_copy_eq input
  , test_mutable_diff input
  , test_immutable_copy input
  , test_swap_swap_eq input
  , test_delete_copy input
  , test_delete_len input
  ]
  where t = inputType input

return []
tests :: IO Bool
tests = releaseLibraryAfterTests $ do
  $checkAllWith TestRunMore checkArgs
