{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.Psv.Base (
    PsvMode(..)
  , PsvFormat(..)
  , StringWord(..)
  , wordsOfString
  , wordsOfBytes
  , wordsOfBytes'
  ) where


import qualified Data.ByteString as B
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Word (Word8)

import           Icicle.Data (Time)
import           Icicle.Internal.Pretty

import           P

import           Text.Printf (printf)


data PsvMode
  = PsvSnapshot Time
  | PsvChords
  deriving (Eq, Ord, Show)

data PsvFormat
  = PsvSparse
  | PsvDense
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

data StringWord = StringWord {
    swOffset :: Int
  , swSize   :: Int
  , swMask   :: Doc
  , swBits   :: Doc
  }

wordsOfString :: Text -> [StringWord]
wordsOfString
 = wordsOfBytes . B.unpack . T.encodeUtf8

wordsOfBytes :: [Word8] -> [StringWord]
wordsOfBytes bs
 = reverse (wordsOfBytes' bs 0 [])

wordsOfBytes' :: [Word8] -> Int -> [StringWord] -> [StringWord]
wordsOfBytes' [] _   acc = acc
wordsOfBytes' bs off acc
 = wordsOfBytes' remains (off + 8) (sw : acc)
 where
  sw = StringWord { swOffset = off, swSize = nbytes, swMask = mask, swBits = bits }

  (bytes, remains) = splitAt 8 bs

  nbytes = length bytes

  nzeros = 8 - nbytes
  zeros  = List.replicate nzeros 0x00

  mask = text $ "0x" <> concatMap (printf "%02X") (zeros <> List.replicate nbytes 0xff)
  bits = text $ "0x" <> concatMap (printf "%02X") (zeros <> reverse bytes)
