{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Icicle.Sea.IO
  ( seaOfDriver
  , seaOfConstants
  , seaOfDefaultConstants
  , defaultOutputMissing
  , defaultPsvMaxRowCount
  , defaultPsvInputBufferSize
  , defaultPsvOutputBufferSize
  , psvDefaultConstants
  , Mode(..)
  , IOFormat (..)
  , InputOpts (..)
  , InputAllowDupTime (..)
  , PsvConfig(..)
  , PsvInputConfig(..)
  , PsvInputFormat(..)
  , PsvInputDenseDict(..)
  , PsvOutputConfig(..)
  , PsvOutputFormat(..)
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Data

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.State

import           Icicle.Sea.IO.Base
import           Icicle.Sea.IO.Psv
import           Icicle.Sea.IO.Zebra

import           P


data IOFormat
  = FormatPsv   PsvConfig
  | FormatZebra Mode PsvOutputConfig -- temporary
    deriving (Eq, Show)

seaOfDriver :: IOFormat -> InputOpts -> [Attribute] -> [SeaProgramState] -> Either SeaError Doc
seaOfDriver format opts attributes states
  = case format of
      FormatPsv conf
        -> seaOfPsvDriver opts conf states
      FormatZebra mode outputConfig
        -> -- FIXME generate code for psv as well when using zebra, because we
           -- are relying on some psv functions, they should be factored out or something
           do x <- seaOfPsvDriver opts (psvDefaultConstants (PsvInputConfig mode PsvInputSparse) outputConfig) states
              y <- seaOfZebraDriver attributes states
              return $ vsep [x, "", y]
