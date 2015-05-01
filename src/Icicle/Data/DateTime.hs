-- | Quick and dirty date stuff
-- TODO support times as well
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Data.DateTime (
    DateTime (..)
  , renderDate
  , dateOfYMD
  ) where

import qualified Data.Dates as D

import           Data.Text  as T

import           P


data DateTime =
  DateTime {
      getDateTime :: D.DateTime
    } deriving (Eq, Ord, Show)


renderDate  :: DateTime -> Text
renderDate d
 = -- if   D.hour d + D.minute d + D.second d == 0
   -- then T.pack (show (D.year d) <> "-" <>
   T.pack dateStr
 where
  d' = getDateTime d
  dateStr
   = show (D.year d') <> "-" <> show (D.month d') <> "-" <> show (D.day d')


dateOfYMD :: Int -> Int -> Int -> DateTime
dateOfYMD y m d
 = DateTime
 $ D.DateTime y m d 0 0 0

