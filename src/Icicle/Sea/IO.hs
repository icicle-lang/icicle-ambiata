{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Icicle.Sea.IO
  ( seaOfDriver
  , Psv.defaultMissingValue
  , InputFormat (..)
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


data InputFormat
  = InputPsv Psv.PsvInputConfig Psv.PsvOutputConfig
  | InputEnterprise

seaOfDriver :: InputFormat -> InputOpts -> [SeaProgramState] -> Either SeaError Doc
seaOfDriver format opts states
  = case format of
      InputPsv inputConfig outputConfig
        -> Psv.seaOfPsvDriver opts inputConfig outputConfig states
      InputEnterprise -> Right "" -- todo
