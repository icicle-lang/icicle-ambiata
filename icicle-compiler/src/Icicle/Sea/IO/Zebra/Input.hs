{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.IO.Zebra.Input
  ( seaInputZebra
  ) where

import qualified Icicle.Sea.IO.Base.Input as Base

--------------------------------------------------------------------------------

-- * Psv input "interface"

-- TODO implement me
seaInputZebra :: Base.SeaInput
seaInputZebra = Base.SeaInput
  { Base.cstmtReadFact     = seaOfReadFact
  , Base.cstmtReadTime     = seaOfReadTime
  , Base.cfunReadTombstone = seaOfReadTombstone
  , Base.cnameFunReadFact  = nameOfReadFact
  , Base.cnameFunAddLast   = nameOfAddLast
  }
  where
    seaOfReadFact _ _ _ _ _ _ = ""
    seaOfReadTime = ""
    seaOfReadTombstone _ _ = ""
    nameOfReadFact _ = ""
    nameOfAddLast  _ = ""
