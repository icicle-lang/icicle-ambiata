{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Builtin where

import           GHC.Generics (Generic)

import           Icicle.Internal.Pretty

import           P


data BuiltinFun
 = BuiltinMath  !BuiltinMath
 | BuiltinTime  !BuiltinTime
 | BuiltinData  !BuiltinData
 | BuiltinArray !BuiltinArray
 | BuiltinMap   !BuiltinMap
 deriving (Show, Eq, Ord, Generic)

listOfBuiltinFuns :: [BuiltinFun]
listOfBuiltinFuns = concat
  [ fmap BuiltinMath    [minBound..maxBound]
  , fmap BuiltinTime    [minBound..maxBound]
  , fmap BuiltinData    [minBound..maxBound]
  , fmap BuiltinArray   [minBound..maxBound]
  , fmap BuiltinMap     [minBound..maxBound]
  ]

data BuiltinMath
 = Log
 | Exp
 | Sqrt
 | Abs
 | Acos
 | Asin
 | Atan
 | Atan2
 | Cos
 | Cosh
 | Sin
 | Sinh
 | Tan
 | Tanh
 | ToDouble
 | Floor
 | Ceiling
 | Round
 | Truncate
 deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data BuiltinTime
 = DaysBetween
 | DaysJulianEpoch
 | SecondsBetween
 | SecondsJulianEpoch
 | ProjectDay
 | ProjectMonth
 | ProjectYear
 deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data BuiltinData
 = Seq
 | Box
 deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data BuiltinMap
 = MapKeys
 | MapValues
 | MapCreate
 | MapInsert
 | MapDelete
 | MapLookup
 deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data BuiltinArray
 = ArraySort
 | ArrayLength
 | ArrayIndex
 deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance NFData BuiltinFun
instance NFData BuiltinMath
instance NFData BuiltinTime
instance NFData BuiltinData
instance NFData BuiltinMap
instance NFData BuiltinArray

--------------------------------------------------------------------------------

instance Pretty BuiltinFun where
 pretty (BuiltinMath  b) = pretty b
 pretty (BuiltinTime  b) = pretty b
 pretty (BuiltinData  b) = pretty b
 pretty (BuiltinArray b) = pretty b
 pretty (BuiltinMap   b) = pretty b

instance Pretty BuiltinMath where
 pretty Log         = "log"
 pretty Exp         = "exp"
 pretty Sqrt        = "sqrt"
 pretty Acos        = "acos"
 pretty Asin        = "asin"
 pretty Atan        = "atan"
 pretty Atan2       = "atan2"
 pretty Cos         = "cos"
 pretty Cosh        = "cosh"
 pretty Sin         = "sin"
 pretty Sinh        = "sinh"
 pretty Tan         = "tan"
 pretty Tanh        = "tanh"
 pretty ToDouble    = "double"
 pretty Abs         = "abs"
 pretty Floor       = "floor"
 pretty Ceiling     = "ceil"
 pretty Round       = "round"
 pretty Truncate    = "trunc"

instance Pretty BuiltinTime where
 pretty DaysBetween        = "days between"
 pretty DaysJulianEpoch    = "days"
 pretty SecondsBetween     = "seconds between"
 pretty SecondsJulianEpoch = "seconds"
 pretty ProjectDay         = "day"
 pretty ProjectMonth       = "month"
 pretty ProjectYear        = "year"

instance Pretty BuiltinData where
 pretty Seq         = "seq"
 pretty Box         = "box"

instance Pretty BuiltinArray where
 pretty ArraySort   = "sort"
 pretty ArrayLength = "length"
 pretty ArrayIndex  = "index"

instance Pretty BuiltinMap where
 pretty MapKeys   = "keys"
 pretty MapValues = "vals"
 pretty MapCreate = "map_create"
 pretty MapInsert = "map_insert"
 pretty MapDelete = "map_delete"
 pretty MapLookup = "map_lookup"
