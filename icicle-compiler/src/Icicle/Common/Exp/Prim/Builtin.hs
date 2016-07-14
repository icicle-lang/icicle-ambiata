{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Common.Exp.Prim.Builtin where

import            Icicle.Internal.Pretty
import            Icicle.Common.Type

import            P

-- | Built-in functions. These are available in all language fragments and
--   supported in C directly.
--
data PrimBuiltinFun
  = PrimBuiltinMath   !PrimBuiltinMath
  | PrimBuiltinMap    !PrimBuiltinMap
  | PrimBuiltinArray  !PrimBuiltinArray
 deriving (Eq, Ord, Show)

-- | Built-in math functions
data PrimBuiltinMath
 = PrimBuiltinCeiling
 | PrimBuiltinFloor
 | PrimBuiltinTruncate
 | PrimBuiltinRound
 | PrimBuiltinToDoubleFromInt
 | PrimBuiltinDiv
 | PrimBuiltinLog
 | PrimBuiltinExp
 | PrimBuiltinSqrt
 deriving (Eq, Ord, Show, Enum, Bounded)

-- | Built-in map functions
data PrimBuiltinMap
 = PrimBuiltinKeys   !ValType !ValType -- ^ Get the keys in a map
 | PrimBuiltinVals   !ValType !ValType -- ^ Get the values in a map
 deriving (Eq, Ord, Show)

data PrimBuiltinArray
 = PrimBuiltinSort   !ValType
 | PrimBuiltinLength !ValType
 | PrimBuiltinIndex  !ValType
 deriving (Eq, Ord, Show)

instance NFData PrimBuiltinFun   where rnf x = seq x ()
instance NFData PrimBuiltinMath  where rnf x = seq x ()
instance NFData PrimBuiltinMap   where rnf x = seq x ()
instance NFData PrimBuiltinArray where rnf x = seq x ()

--------------------------------------------------------------------------------

instance Pretty PrimBuiltinFun where
 pretty (PrimBuiltinMath   p) = pretty p
 pretty (PrimBuiltinMap    p) = pretty p
 pretty (PrimBuiltinArray  p) = pretty p

instance Pretty PrimBuiltinMath where
 pretty p = case p of
   PrimBuiltinDiv             -> "div#"
   PrimBuiltinLog             -> "log#"
   PrimBuiltinExp             -> "exp#"
   PrimBuiltinSqrt            -> "sqrt#"
   PrimBuiltinFloor           -> "floor#"
   PrimBuiltinCeiling         -> "ceil#"
   PrimBuiltinRound           -> "round#"
   PrimBuiltinTruncate        -> "trunc#"
   PrimBuiltinToDoubleFromInt -> "doubleOfInt#"

instance Pretty PrimBuiltinMap where
 pretty p = case p of
   PrimBuiltinKeys   k v -> annotateTypeArgs [MapT k v] "keys#"
   PrimBuiltinVals   k v -> annotateTypeArgs [MapT k v] "vals#"

instance Pretty PrimBuiltinArray where
 pretty p = case p of
   PrimBuiltinSort   t -> annotateTypeArgs [ArrayT t] "sort#"
   PrimBuiltinLength t -> annotateTypeArgs [ArrayT t] "length#"
   PrimBuiltinIndex  t -> annotateTypeArgs [ArrayT t] "index#"
