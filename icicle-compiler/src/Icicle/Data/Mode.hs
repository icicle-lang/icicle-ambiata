{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Data.Mode (
  Mode (..)
, modeFromString
, renderMode
) where

import           P
import           Data.Text

data Mode =
  State
  | Set
  | KeyedSet [Text]
  deriving (Eq, Show)

modeFromString :: Text -> Maybe Mode
modeFromString t = case split ((==) ',') t of
  "state"     : [] = Just State
  "set"       : [] = Just Set
  "keyed_set" : k  = Just $ KeyedSet k
  _                = Nothing

renderMode :: Mode -> Text
renderMode State = "state"
renderMode Set   = "set"
renderMode (KeyedSet k) = intercalate ',' $ keyed_set : k
