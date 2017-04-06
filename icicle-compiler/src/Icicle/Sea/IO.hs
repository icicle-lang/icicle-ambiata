{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Icicle.Sea.IO
  ( InputFormat (..)
  , OutputFormat (..)
  , seaOfDriver
  , module Icicle.Sea.IO.Psv
  , module Icicle.Sea.IO.Zebra
  , module Icicle.Sea.IO.Offset
  , module Icicle.Sea.IO.Base
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


data InputFormat
  = InputFormatPsv   PsvInputConfig
  | InputFormatZebra ZebraInputConfig Mode
    deriving (Eq, Ord, Show)

data OutputFormat
  = OutputFormatPsv PsvOutputConfig
  | OutputFormatZebra
    deriving (Eq, Ord, Show)

-- TODO Psv code always needs to be generated because it defines the fleet.
-- TODO When we remove Psv, compile times should be better.
--
seaOfDriver ::
     InputFormat
  -> OutputFormat
  -> InputOpts
  -> [Attribute]
  -> [SeaProgramAttribute]
  -> Either SeaError Doc
seaOfDriver inputFormat outputFormat opts attributes states =
  case (inputFormat, outputFormat) of
    (InputFormatPsv inputConfig, OutputFormatPsv outputConfig) ->
      seaOfPsvDriver opts inputConfig outputConfig states

    (InputFormatPsv inputConfig, OutputFormatZebra) -> do
      let
        dummyPsvOutputConfig =
          PsvOutputConfig Chords PsvOutputDense defaultOutputMissing
      psv <- seaOfPsvDriver opts inputConfig dummyPsvOutputConfig states
      zebra <- seaOfZebraDriver attributes states
      return . vsep $ [psv, zebra]

    (InputFormatZebra{}, OutputFormatPsv outputConfig) -> do
      let
        dummyPsvInputConfig =
          PsvInputConfig Chords PsvInputSparse
      psv <- seaOfPsvDriver opts dummyPsvInputConfig outputConfig states
      zebra <- seaOfZebraDriver attributes states
      return . vsep $ [psv, zebra]

    (InputFormatZebra{}, OutputFormatZebra) -> do
      let
        dummyPsvInputConfig =
          PsvInputConfig Chords PsvInputSparse
      let
        dummyPsvOutputConfig =
          PsvOutputConfig Chords PsvOutputDense defaultOutputMissing
      psv <- seaOfPsvDriver opts dummyPsvInputConfig dummyPsvOutputConfig states
      zebra <- seaOfZebraDriver attributes states
      return . vsep $ [psv, zebra]
