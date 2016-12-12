{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.IO.Zebra (
    seaOfZebraDriver
  ) where

import           Icicle.Internal.Pretty

import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Sea.IO.Base
import           Icicle.Sea.FromAvalanche.State

import           Icicle.Sea.IO.Psv.Output

import           P


seaOfZebraDriver :: PsvOutputConfig -> [SeaProgramState] -> Either SeaError Doc
seaOfZebraDriver outputConfig states = do
  let struct_sea  = seaOfFleetState      states
      alloc_sea   = seaOfAllocFleet      states
      collect_sea = seaOfCollectFleet    states
      config_sea  = seaOfConfigureFleet  Chords states

  write_sea <- seaOfWriteFleetOutput outputConfig Nothing states

  pure $ vsep
    [ struct_sea
    , ""
    , alloc_sea
    , ""
    , collect_sea
    , ""
    , config_sea
    , ""
    , write_sea
    ]
