{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Source.Query.Builtin where

import Icicle.Internal.Pretty

import P


data BuiltinFun
 = BuiltinMath BuiltinMath
 | BuiltinTime BuiltinTime
 | BuiltinData BuiltinData
 | BuiltinMap  BuiltinMap
 deriving (Show, Eq, Ord)

listOfBuiltinFuns :: [BuiltinFun]
listOfBuiltinFuns = concat
  [ fmap BuiltinMath [minBound..maxBound]
  , fmap BuiltinTime [minBound..maxBound]
  , fmap BuiltinData [minBound..maxBound]
  , fmap BuiltinMap  [minBound..maxBound]
  ]

data BuiltinMath
 = Log
 | Exp
 | Sqrt
 | Abs
 | ToDouble
 | Floor
 | Ceiling
 | Round
 | Truncate
 deriving (Show, Eq, Ord, Enum, Bounded)

data BuiltinTime
 = DaysBetween
 | DaysEpoch
 deriving (Show, Eq, Ord, Enum, Bounded)

data BuiltinData
 = Seq
 | Box
 deriving (Show, Eq, Ord, Enum, Bounded)

data BuiltinMap
 = MapKeys
 | MapValues
 deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------

instance Pretty BuiltinFun where
 pretty (BuiltinMath b) = pretty b
 pretty (BuiltinTime b) = pretty b
 pretty (BuiltinData b) = pretty b
 pretty (BuiltinMap  b) = pretty b

instance Pretty BuiltinMath where
 pretty Log         = "log"
 pretty Exp         = "exp"
 pretty Sqrt        = "sqrt"
 pretty ToDouble    = "double"
 pretty Abs         = "abs"
 pretty Floor       = "floor"
 pretty Ceiling     = "ceil"
 pretty Round       = "round"
 pretty Truncate    = "trunc"

instance Pretty BuiltinTime where
 pretty DaysBetween = "days between"
 pretty DaysEpoch   = "days"

instance Pretty BuiltinData where
 pretty Seq         = "seq"
 pretty Box         = "box"

instance Pretty BuiltinMap where
 pretty MapKeys   = "keys"
 pretty MapValues = "vals"
