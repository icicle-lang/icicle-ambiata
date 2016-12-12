{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Icicle.Sea.IO
  ( seaOfDriver
  , IOFormat (..)
  , InputOpts (..)
  , InputAllowDupTime (..)
  , Psv.PsvInputConfig(..)
  , Psv.PsvOutputConfig(..)
  , Psv.defaultOutputMissing
  , Mode(..)
  , Psv.PsvOutputFormat(..)
  , Psv.PsvInputFormat(..)
  , Psv.PsvInputDenseDict(..)
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.State

import           Icicle.Sea.IO.Base
import           Icicle.Sea.IO.Psv
import           Icicle.Sea.IO.Zebra

import           P


data IOFormat
  = FormatPsv   Psv.PsvInputConfig Psv.PsvOutputConfig
  | FormatZebra Psv.PsvOutputConfig -- temporary
    deriving (Eq, Show)

seaOfDriver :: IOFormat -> InputOpts -> [SeaProgramState] -> Either SeaError Doc
seaOfDriver format opts states
  = case format of
      FormatPsv conf
        -> seaOfPsvDriver opts conf states
      FormatZebra outputConfig
        -> Zebra.seaOfZebraDriver outputConfig states
