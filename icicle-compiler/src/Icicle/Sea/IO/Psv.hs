{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.IO.Psv (
    PsvConfig (..)
  , PsvConstants (..)
  , PsvInputConfig(..)
  , PsvOutputConfig(..)
  , PsvOutputFormat(..)
  , PsvInputFormat(..)
  , PsvInputDenseDict(..)
  , seaOfPsvDriver
  , defaultPsvConstants
  , defaultOutputMissing
  , defaultPsvFactsLimit
  , defaultPsvMaxRowCount
  , defaultPsvInputBufferSize
  , defaultPsvOutputBufferSize
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.IO.Base
import           Icicle.Sea.IO.Psv.Input
import           Icicle.Sea.IO.Psv.Output

import           P


data PsvConfig = PsvConfig {
    psvInputConfig      :: PsvInputConfig
  , psvOutputConfig     :: PsvOutputConfig
  } deriving (Eq, Show)

data PsvConstants = PsvConstants {
    psvMaxRowCount      :: Int
  , psvInputBufferSize  :: Int
  , psvOutputBufferSize :: Int
  , psvFactsLimit       :: Int
  } deriving (Eq, Ord, Show)

defaultPsvConstants :: PsvConstants
defaultPsvConstants =
  PsvConstants defaultPsvMaxRowCount defaultPsvInputBufferSize defaultPsvOutputBufferSize defaultPsvFactsLimit

defaultPsvMaxRowCount :: Int
defaultPsvMaxRowCount = 128

defaultPsvInputBufferSize :: Int
defaultPsvInputBufferSize = 256 * 1024

defaultPsvOutputBufferSize :: Int
defaultPsvOutputBufferSize = 256 * 1024

defaultPsvFactsLimit :: Int
defaultPsvFactsLimit = 1024 * 1024

------------------------------------------------------------------------

seaOfPsvDriver :: InputOpts -> PsvConfig -> [SeaProgramAttribute] -> Either SeaError Doc
seaOfPsvDriver opts config states = do
  let inputConfig  = psvInputConfig config
  let outputConfig = psvOutputConfig config
  let outputList   = case inputPsvFormat inputConfig of
                       PsvInputSparse
                         -> Nothing
                       PsvInputDense _ feed
                         -> Just [feed]
  let mode         = inputPsvMode inputConfig

  let struct_sea  = seaOfFleetState      states
      alloc_sea   = seaOfAllocFleet      states
      collect_sea = seaOfCollectFleet    states
      config_sea  = seaOfConfigureFleet  mode states

  read_sea  <- seaOfReadAnyFactPsv   opts inputConfig  states
  write_sea <- seaOfWriteFleetOutput      outputConfig outputList states

  pure $ vsep
    [ struct_sea
    , ""
    , alloc_sea
    , ""
    , collect_sea
    , ""
    , config_sea
    , ""
    , read_sea
    , ""
    , write_sea
    ]

