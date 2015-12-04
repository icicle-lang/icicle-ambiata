{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Chords.File (
    maximumOfTimes
  , chordFile
  , hPutChordFile
  , writeChordFile
  , header
  , record
  ) where

import qualified Icicle.Data as D
import qualified Icicle.Data.Time as D

import qualified Data.Map as Map

import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Encoding as T

import System.IO (IO, IOMode(..), Handle, FilePath, withBinaryFile)

import           P

------------------------------------------------------------------------

type ChordMap = Map.Map D.Entity [D.Time]

maximumOfTimes :: ChordMap -> Int
maximumOfTimes
 = Map.fold max 0
 . Map.map length

header :: ChordMap -> BS.Builder
header chordmap
 = let magic   = "CHORDATA"
       version = 1
       times   = maximumOfTimes chordmap
   in  mconcat
     [ BS.string8 magic
     , BS.int64LE version
     , BS.int64LE $ fromIntegral times ]

record :: (D.Entity, [D.Time]) -> BS.Builder
record (ent, times)
 = let entity       = T.encodeUtf8 $ D.getEntity ent
       entity_size  = BSC.length entity
       times_count  = length times
       times_build  = mconcat $ fmap (BS.word64LE . D.packedOfTime) times
   in  mconcat
     [ BS.int64LE $ fromIntegral entity_size
     , BS.int64LE $ fromIntegral times_count
     , BS.byteString entity
     , BS.word8 0
     , times_build ]


chordFile :: ChordMap -> BS.Builder
chordFile chordmap
 = let header' = header chordmap
       records = mconcat $ fmap record $ Map.toList chordmap
   in  mconcat
     [ header'
     , records
     , BS.int64LE 0, BS.int64LE 0, BS.word8 0 ]


hPutChordFile :: ChordMap -> Handle -> IO ()
hPutChordFile chordmap hnd
 = BS.hPutBuilder hnd (chordFile chordmap)

writeChordFile :: ChordMap -> FilePath -> IO ()
writeChordFile chordmap path
 = withBinaryFile path WriteMode (hPutChordFile chordmap)
