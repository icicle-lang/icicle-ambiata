{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.IO.Psv (
    PsvConfig (..)
  , PsvInputConfig(..)
  , PsvOutputConfig(..)
  , PsvOutputFormat(..)
  , PsvInputFormat(..)
  , PsvInputDenseDict(..)
  , seaOfConstants
  , seaOfDefaultConstants
  , seaOfPsvDriver
  , psvDefaultConstants
  , defaultOutputMissing
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
  -- static constants to be declared
  , psvMaxRowCount      :: Int
  , psvInputBufferSize  :: Int
  , psvOutputBufferSize :: Int
  } deriving (Eq, Show)

psvDefaultConstants :: PsvInputConfig -> PsvOutputConfig -> PsvConfig
psvDefaultConstants x y
  = PsvConfig x y defaultPsvMaxRowCount defaultPsvInputBufferSize defaultPsvOutputBufferSize

defaultPsvMaxRowCount :: Int
defaultPsvMaxRowCount = 128

defaultPsvInputBufferSize :: Int
defaultPsvInputBufferSize = 256 * 1024

defaultPsvOutputBufferSize :: Int
defaultPsvOutputBufferSize = 256 * 1024

seaOfConstants :: PsvConfig -> Doc
seaOfConstants config
  = vsep
  [ seaOfConstant "int" "psv_max_row_count"      (pretty (psvMaxRowCount      config))
  , seaOfConstant "int" "psv_input_buffer_size"  (pretty (psvInputBufferSize  config))
  , seaOfConstant "int" "psv_output_buffer_size" (pretty (psvOutputBufferSize config))
  ]

seaOfDefaultConstants :: Doc
seaOfDefaultConstants
  = vsep
  [ seaOfConstant "int" "psv_max_row_count"      (pretty defaultPsvMaxRowCount)
  , seaOfConstant "int" "psv_input_buffer_size"  (pretty defaultPsvInputBufferSize)
  , seaOfConstant "int" "psv_output_buffer_size" (pretty defaultPsvOutputBufferSize)
  ]

seaOfConstant :: Doc -> Doc -> Doc -> Doc
seaOfConstant ty name val
  = "static const" <+> ty <+> name <+> "=" <+> val <> ";"

------------------------------------------------------------------------

seaOfPsvDriver :: InputOpts -> PsvConfig -> [SeaProgramState] -> Either SeaError Doc
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

