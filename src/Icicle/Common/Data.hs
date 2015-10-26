{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Common.Data (
      valueToCore
    , valueFromCore
    , asAtValueToCore
    ) where

import qualified Data.Map as Map

import qualified Icicle.Common.Base as C
import qualified Icicle.Data as D

import           P


valueToCore :: D.Value -> C.BaseValue
valueToCore = \case
  D.IntValue     x            -> C.VInt x
  D.DoubleValue  x            -> C.VDouble x
  D.BooleanValue x            -> C.VBool x
  D.StringValue  x            -> C.VString x
  D.DateValue    x            -> C.VDateTime x
  D.Tombstone                 -> C.VError C.ExceptTombstone
  D.PairValue    a b          -> C.VPair (valueToCore a) (valueToCore b)
  D.ListValue (D.List xs)     -> C.VArray (fmap valueToCore xs)
  D.MapValue     kvs          -> C.VMap . Map.fromList $ fmap (bimap valueToCore valueToCore) kvs
  D.StructValue (D.Struct vs) -> C.VStruct (Map.fromList (fmap (bimap (C.StructField . D.getAttribute) valueToCore) vs))

valueFromCore :: C.BaseValue -> D.Value
valueFromCore = \case
  C.VBool     x -> D.BooleanValue x
  C.VInt      x -> D.IntValue x
  C.VDouble   x -> D.DoubleValue x
  C.VDateTime x -> D.DateValue x
  C.VString   x -> D.StringValue x
  C.VUnit       -> D.IntValue 13013
  C.VError    _ -> D.Tombstone
  C.VLeft     x -> valueFromCore x
  C.VRight    x -> valueFromCore x
  C.VNone       -> D.Tombstone
  C.VSome     x -> valueFromCore x
  C.VPair   x y -> D.PairValue (valueFromCore x) (valueFromCore y)
  C.VArray   xs -> D.ListValue (D.List (fmap valueFromCore xs))
  C.VBuf   _ xs -> D.ListValue (D.List (fmap valueFromCore xs))
  C.VMap    kvs -> D.MapValue (fmap (bimap valueFromCore valueFromCore) (Map.toList kvs))
  C.VStruct  xs -> D.StructValue (D.Struct (fmap (bimap (D.Attribute . C.nameOfStructField) valueFromCore)
                                                 (Map.toList xs)))


asAtValueToCore :: D.AsAt D.Value -> C.BaseValue
asAtValueToCore x = C.VPair (vLeftRight x (valueToCore (D.fact x))) (C.VDateTime (D.time x))

vLeftRight :: D.AsAt D.Value -> C.BaseValue -> C.BaseValue
vLeftRight x | D.fact x == D.Tombstone = C.VLeft
             | otherwise               = C.VRight

