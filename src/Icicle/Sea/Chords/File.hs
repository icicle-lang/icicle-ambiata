{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Chords.File (
    maximumOfDates
  , chordFile
  , writeFile
  , header
  , record
  ) where

import qualified Icicle.Data as D
import qualified Icicle.Data.DateTime as D

import qualified Data.Map as Map

import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Builder as BS
import qualified Data.Text as T

import System.IO (IO, Handle)

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
 = let entity       = BSC.pack $ T.unpack $ D.getEntity ent
       entity_size  = BSC.length entity + 1
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


writeFile :: ChordMap -> Handle -> IO ()
writeFile chordmap hnd
 = BS.hPutBuilder hnd (chordFile chordmap)
