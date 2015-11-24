{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Chords.File (
    maximumOfDates
  , chordFile
  , hPutChordFile
  , writeChordFile
  , header
  , record
  ) where

import qualified Icicle.Data as D
import qualified Icicle.Data.DateTime as D

import qualified Data.Map as Map

import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Encoding as T

import System.IO (IO, IOMode(..), Handle, FilePath, withBinaryFile)

import           P

------------------------------------------------------------------------

type ChordMap = Map.Map D.Entity [D.DateTime]

maximumOfDates :: ChordMap -> Int
maximumOfDates
 = Map.fold max 0
 . Map.map length

header :: ChordMap -> BS.Builder
header chordmap
 = let magic   = "CHORDATA"
       version = 1
       dates   = maximumOfDates chordmap
   in  mconcat
     [ BS.string8 magic
     , BS.int64LE version
     , BS.int64LE $ fromIntegral dates ]

record :: (D.Entity, [D.DateTime]) -> BS.Builder
record (ent, dates)
 = let entity       = T.encodeUtf8 $ D.getEntity ent
       entity_size  = BSC.length entity
       dates_count  = length dates
       dates_build  = mconcat $ fmap (BS.word64LE . D.packedOfDate) dates
   in  mconcat
     [ BS.int64LE $ fromIntegral entity_size
     , BS.int64LE $ fromIntegral dates_count
     , BS.byteString entity
     , BS.word8 0
     , dates_build ]


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
