{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Source.Query.Builtin where

import Icicle.Internal.Pretty

import P


data BuiltinFun
 = BuiltinMath  !BuiltinMath
 | BuiltinTime  !BuiltinTime
 | BuiltinData  !BuiltinData
 | BuiltinArray !BuiltinArray
 | BuiltinMap   !BuiltinMap
 deriving (Show, Eq, Ord)

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
 | ToDouble
 | Floor
 | Ceiling
 | Round
 | Truncate
 deriving (Show, Eq, Ord, Enum, Bounded)

data BuiltinTime
 = DaysBetween
 | DaysJulianEpoch
 | SecondsBetween
 | SecondsJulianEpoch
 deriving (Show, Eq, Ord, Enum, Bounded)

data BuiltinData
 = Seq
 | Box
 deriving (Show, Eq, Ord, Enum, Bounded)

data BuiltinMap
 = MapKeys
 | MapValues
 | MapCreate
 | MapInsert
 | MapDelete
 | MapLookup
 deriving (Show, Eq, Ord, Enum, Bounded)

data BuiltinArray
 = ArraySort
 | ArrayLength
 | ArrayIndex
 deriving (Show, Eq, Ord, Enum, Bounded)

instance NFData BuiltinFun   where rnf x = seq x ()
instance NFData BuiltinMath  where rnf x = seq x ()
instance NFData BuiltinTime  where rnf x = seq x ()
instance NFData BuiltinData  where rnf x = seq x ()
instance NFData BuiltinMap   where rnf x = seq x ()
instance NFData BuiltinArray where rnf x = seq x ()

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
