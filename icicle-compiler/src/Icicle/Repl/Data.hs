{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Repl.Data (
    UseDotfiles(..)
  , ReplOptions(..)

  , State(..)
  , Input(..)
  , Flag(..)

  , Help(..)
  , FlagInfo(..)
  , OptionInfo(..)

  , Command(..)
  , SetOption(..)
  ) where

import           Data.Set (Set)
import           Data.String (String)

import           Icicle.Data.Time
import           Icicle.Dictionary

import           P

import           System.IO (FilePath)


data UseDotfiles =
    UseDotfiles
  | SkipDotfiles
    deriving (Eq, Ord, Show)

data ReplOptions =
  ReplOptions {
      replDotfiles :: !UseDotfiles
    , replInit :: ![String]
    , replLoad :: ![FilePath]
    } deriving (Eq, Ord, Show)

data Input =
    InputNone
  | InputPsv !FilePath
  | InputZebra !FilePath
    deriving (Eq, Show)

data Flag =
    FlagColor
  | FlagType
  | FlagTypeCheckLog
  | FlagBigData
  | FlagAnnotated
  | FlagInlined
  | FlagDesugar
  | FlagReified
  | FlagCore
  | FlagCoreSimp
  | FlagCoreType
  | FlagCoreEval
  | FlagAvalanche
  | FlagAvalancheEval
  | FlagFlattenSimp
  | FlagFlattenNoSimp
  | FlagFlattenSimpCheck
  | FlagSeaPreamble
  | FlagSea
  | FlagSeaAssembly
  | FlagSeaLLVM
  | FlagSeaRuntime
  | FlagSeaEval
  | FlagStatTime
    deriving (Eq, Ord, Show)

data FlagInfo =
  FlagInfo {
      flagFlag :: !Flag
    , flagName :: !String
    , flagOn :: !String
    , flagOff :: !String
    , flagDescription :: !String
    } deriving (Eq, Ord, Show)

data Help =
  Help {
      helpCommand :: !String
    , helpDescription :: !String
    } deriving (Eq, Ord, Show)

data OptionInfo =
  OptionInfo {
      optionName :: !String
    , optionValue :: !String
    } deriving (Eq, Ord, Show)

data State =
  State {
      stateInput :: !Input
    , stateDictionary :: !Dictionary
    , stateSnapshotDate :: !Date
    , stateMaxMapSize :: !Int
    , stateLimit :: !Int
    , stateCFlags :: !String
    , stateFlags :: !(Set Flag)
    } deriving (Eq, Show)

data SetOption =
    SetShowOptions
  | SetFlagOn !Flag
  | SetFlagOff !Flag
  | SetSnapshot !Date
  | SetMaxMapSize !Int
  | SetLimit !Int
  | SetCFlags !String
    deriving (Eq, Ord, Show)

data Command =
    CommandBlank
  | CommandHelp
  | CommandSet ![SetOption]
  | CommandDictionary
  | CommandLoad !FilePath
  | CommandComment !String
  | CommandLet !String
  | CommandQuery !String
    deriving (Eq, Ord, Show)
