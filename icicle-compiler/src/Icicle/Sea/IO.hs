{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Icicle.Sea.IO
  ( seaOfDriver
  , defaultOutputMissing
  , defaultPsvConstants
  , defaultPsvFactsLimit
  , defaultPsvMaxRowCount
  , defaultPsvInputBufferSize
  , defaultPsvOutputBufferSize
  , defaultPsvMaxMapSize

  , Mode(..)
  , IOFormat (..)
  , InputOpts (..)
  , InputAllowDupTime (..)
  , PsvConstants (..)
  , PsvConfig(..)
  , PsvInputConfig(..)
  , PsvInputFormat(..)
  , PsvInputDenseDict(..)
  , PsvOutputConfig(..)
  , PsvOutputFormat(..)

  , module Icicle.Sea.IO.Offset
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Sea.Data
import           Icicle.Sea.Error (SeaError(..))

import           Icicle.Sea.IO.Offset
import           Icicle.Sea.IO.Base
import           Icicle.Sea.IO.Psv

import           P


data IOFormat
  = FormatPsv   PsvConfig
    deriving (Eq, Show)

seaOfDriver :: IOFormat -> InputOpts -> [Cluster c k] -> Either SeaError Doc
seaOfDriver format opts clusters
  = case format of
      FormatPsv conf -> do
        seaOfPsvDriver opts conf clusters
