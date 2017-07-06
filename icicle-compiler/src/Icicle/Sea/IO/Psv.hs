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
  , defaultPsvMaxMapSize

  , PsvSchema
  , renderPrettyPsvSchema
  , renderCompactPsvSchema
  , parsePsvSchema

  , PsvSchemaDecodeError
  , renderPsvSchemaDecodeError
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Sea.Data
import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.IO.Base
import           Icicle.Sea.IO.Psv.Input
import           Icicle.Sea.IO.Psv.Output
import           Icicle.Sea.IO.Psv.Schema

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
  , psvMaxMapSize       :: Int
  } deriving (Eq, Ord, Show)

defaultPsvConstants :: PsvConstants
defaultPsvConstants =
  PsvConstants defaultPsvMaxRowCount defaultPsvInputBufferSize defaultPsvOutputBufferSize defaultPsvFactsLimit defaultPsvMaxMapSize

defaultPsvMaxRowCount :: Int
defaultPsvMaxRowCount = 128

defaultPsvInputBufferSize :: Int
defaultPsvInputBufferSize = 256 * 1024

defaultPsvOutputBufferSize :: Int
defaultPsvOutputBufferSize = 256 * 1024

defaultPsvFactsLimit :: Int
defaultPsvFactsLimit = 1024 * 1024

defaultPsvMaxMapSize :: Int
defaultPsvMaxMapSize = 1024 * 1024

------------------------------------------------------------------------

seaOfPsvDriver :: InputOpts -> PsvConfig -> [Cluster c k] -> Either SeaError Doc
seaOfPsvDriver opts config clusters = do
  let inputConfig  = psvInputConfig config
  let outputConfig = psvOutputConfig config
  let outputList   = case inputPsvFormat inputConfig of
                       PsvInputSparse
                         -> Nothing
                       PsvInputDense _ feed
                         -> Just [feed]
  let mode         = inputPsvMode inputConfig

  let struct_sea  = seaOfFleetState      clusters
      alloc_sea   = seaOfAllocFleet      clusters
      collect_sea = seaOfCollectFleet    clusters
      config_sea  = seaOfConfigureFleet  mode clusters

  read_sea  <- seaOfReadAnyFactPsv   opts inputConfig  clusters
  write_sea <- seaOfWriteFleetOutput      outputConfig outputList clusters

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

