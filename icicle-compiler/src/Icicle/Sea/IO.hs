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
  , defaultZebraConfig
  , defaultZebraChunkFactCount
  , defaultZebraAllocLimitGB

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
  , ZebraConfig (..)
  , ZebraChunkFactCount (..)
  , ZebraAllocLimitGB (..)

  , module Icicle.Sea.IO.Offset
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Data

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.State

import           Icicle.Sea.IO.Offset
import           Icicle.Sea.IO.Base
import           Icicle.Sea.IO.Psv
import           Icicle.Sea.IO.Zebra

import           P


data IOFormat
  = FormatPsv   PsvConfig
  | FormatZebra ZebraConfig Mode PsvOutputConfig -- temporary
    deriving (Eq, Show)

seaOfDriver :: IOFormat -> InputOpts -> [Attribute] -> [SeaProgramAttribute] -> Either SeaError Doc
seaOfDriver format opts attributes states
  = case format of
      FormatPsv conf -> do
        seaOfPsvDriver opts conf states
      FormatZebra _ mode outputConfig -> do
        -- FIXME generate code for psv as well when using zebra, because we
        -- are relying on some psv functions, they should be factored out or something
        let psvConfig =
              PsvConfig (PsvInputConfig mode PsvInputSparse) outputConfig
        x <- seaOfPsvDriver opts psvConfig states
        y <- seaOfZebraDriver attributes states
        return $ vsep [x, "", y]
