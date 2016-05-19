{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Icicle.Sea.IO
  ( seaOfDriver
  , Psv.defaultMissingValue
  , IOFormat (..)
  , InputOpts (..)
  , InputAllowDupTime (..)
  , Psv.PsvInputConfig(..)
  , Psv.PsvOutputConfig(..)
  , Psv.PsvMode(..)
  , Psv.PsvOutputFormat(..)
  , Psv.PsvInputFormat(..)
  , Psv.PsvInputDenseDict(..)
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.IO.Base
import qualified Icicle.Sea.IO.Psv as Psv

import           P


data IOFormat
  = FormatPsv Psv.PsvInputConfig Psv.PsvOutputConfig
  | FormatEnterprise

seaOfDriver :: IOFormat -> InputOpts -> [SeaProgramState] -> Either SeaError Doc
seaOfDriver format opts states
  = case format of
      FormatPsv inputConfig outputConfig
        -> Psv.seaOfPsvDriver opts inputConfig outputConfig states
      FormatEnterprise -> Right "" -- todo
