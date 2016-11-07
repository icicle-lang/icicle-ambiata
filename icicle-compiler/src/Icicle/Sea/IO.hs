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
  , InputFactsLimit (..)
  , InputOverLimit (..)
  , defaultFactsLimit
  , Psv.PsvInputConfig(..)
  , Psv.PsvOutputConfig(..)
  , Psv.defaultOutputMissing
  , Psv.PsvMode(..)
  , Psv.PsvOutputFormat(..)
  , Psv.PsvInputFormat(..)
  , Psv.PsvInputDenseDict(..)
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.State

import           Icicle.Sea.IO.Base
import qualified Icicle.Sea.IO.Psv   as Psv
import qualified Icicle.Sea.IO.Zebra as Zebra

import           P


data IOFormat
  = FormatPsv Psv.PsvInputConfig Psv.PsvOutputConfig
  | FormatZebra

seaOfDriver :: IOFormat -> InputOpts -> [SeaProgramState] -> Either SeaError Doc
seaOfDriver format opts states
  = case format of
      FormatPsv inputConfig outputConfig
        -> Psv.seaOfPsvDriver opts inputConfig outputConfig states
      FormatZebra
        -> Zebra.seaOfZebraDriver -- todo
