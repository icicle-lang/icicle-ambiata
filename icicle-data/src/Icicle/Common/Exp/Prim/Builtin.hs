{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Common.Exp.Prim.Builtin where

import           GHC.Generics (Generic)

import           Icicle.Common.NanEq
import           Icicle.Common.Type
import           Icicle.Internal.Pretty

import           P

-- | Built-in functions. These are available in all language fragments and
--   supported in C directly.
--
data PrimBuiltinFun
  = PrimBuiltinMath   !PrimBuiltinMath
  | PrimBuiltinMap    !PrimBuiltinMap
  | PrimBuiltinArray  !PrimBuiltinArray
 deriving (Eq, Ord, Show, Generic, NanEq)

-- | Built-in math functions
data PrimBuiltinMath
 = PrimBuiltinCeiling
 | PrimBuiltinFloor
 | PrimBuiltinTruncate
 | PrimBuiltinRound
 | PrimBuiltinToDoubleFromInt
 | PrimBuiltinDiv
 | PrimBuiltinPow
 | PrimBuiltinLog
 | PrimBuiltinExp
 | PrimBuiltinSqrt
 | PrimBuiltinDoubleIsValid
 deriving (Eq, Ord, Show, Enum, Bounded, Generic, NanEq)

-- | Built-in map functions
data PrimBuiltinMap
 = PrimBuiltinKeys   !ValType !ValType -- ^ Get the keys in a map
 | PrimBuiltinVals   !ValType !ValType -- ^ Get the values in a map
 deriving (Eq, Ord, Show, Generic, NanEq)

data PrimBuiltinArray
 = PrimBuiltinSort   !ValType
 | PrimBuiltinLength !ValType
 | PrimBuiltinIndex  !ValType
 deriving (Eq, Ord, Show, Generic, NanEq)

instance NFData PrimBuiltinFun
instance NFData PrimBuiltinMath
instance NFData PrimBuiltinMap
instance NFData PrimBuiltinArray

--------------------------------------------------------------------------------

instance Pretty PrimBuiltinFun where
 pretty (PrimBuiltinMath   p) = pretty p
 pretty (PrimBuiltinMap    p) = pretty p
 pretty (PrimBuiltinArray  p) = pretty p

instance Pretty PrimBuiltinMath where
 pretty p = case p of
   PrimBuiltinDiv             -> "div#"
   PrimBuiltinPow             -> "pow#"
   PrimBuiltinLog             -> "log#"
   PrimBuiltinExp             -> "exp#"
   PrimBuiltinSqrt            -> "sqrt#"
   PrimBuiltinFloor           -> "floor#"
   PrimBuiltinCeiling         -> "ceil#"
   PrimBuiltinRound           -> "round#"
   PrimBuiltinTruncate        -> "trunc#"
   PrimBuiltinToDoubleFromInt -> "doubleOfInt#"
   PrimBuiltinDoubleIsValid   -> "doubleIsValid#"

instance Pretty PrimBuiltinMap where
 pretty p = case p of
   PrimBuiltinKeys   _k _v -> "keys#"
   PrimBuiltinVals   _k _v -> "vals#"

instance Pretty PrimBuiltinArray where
 pretty p = case p of
   PrimBuiltinSort   _t -> "sort#"
   PrimBuiltinLength _t -> "length#"
   PrimBuiltinIndex  _t -> "index#"
