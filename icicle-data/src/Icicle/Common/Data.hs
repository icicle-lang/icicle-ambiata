{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Common.Data (
    asAtValueToCore
  , valueToCore
  , valueFromCore
  ) where

import qualified Data.Map as Map

import qualified Icicle.Common.Base as C
import qualified Icicle.Common.Type as C
import qualified Icicle.Data as D

import           P


asAtValueToCore :: D.AsAt D.Value -> C.ValType -> Maybe C.BaseValue
asAtValueToCore x t
 = valueToCore (D.PairValue (D.atFact x) (D.TimeValue (D.atTime x))) t

valueToCore :: D.Value -> C.ValType -> Maybe C.BaseValue
valueToCore dv vt
 = case (dv, vt) of
     -- nested options are not allowed
     (_, C.OptionT C.OptionT{}) -> Nothing
     (_, C.OptionT C.SumT{})    -> Nothing

     (D.IntValue     x, C.IntT{})    -> Just (C.VInt x)
     (D.DoubleValue  x, C.DoubleT{}) -> Just (C.VDouble x)
     (D.BooleanValue x, C.BoolT{})   -> Just (C.VBool x)
     (D.StringValue  x, C.StringT{}) -> Just (C.VString x)
     (D.TimeValue    x, C.TimeT{})   -> Just (C.VTime x)

     (D.Tombstone,      C.ErrorT{})  -> Just (C.VError C.ExceptTombstone)

     (D.PairValue a b, C.PairT ta tb)
      -> C.VPair <$> valueToCore a ta <*> valueToCore b tb

     (D.ListValue (D.List xs), C.ArrayT t)
      -> C.VArray <$> traverse (\x -> valueToCore x t) xs

     (D.MapValue kvs, C.MapT tk tv)
      -> let go (k,v) = (,) <$> valueToCore k tk <*> valueToCore v tv
         in C.VMap . Map.fromList <$> traverse go kvs

     (D.StructValue (D.Struct vs), C.StructT (C.StructType ts))
      -> let vs' = Map.fromList vs

             go (k@(C.StructField f), tf) = do
               let v = fromMaybe D.Tombstone (Map.lookup f vs')
               (,) <$> pure k <*> valueToCore v tf

         in C.VStruct . Map.fromList <$> traverse go (Map.toList ts)

     (D.Tombstone, C.OptionT _)
      -> Just C.VNone

     (x, C.OptionT tx)
      -> C.VSome <$> valueToCore x tx

     (x, C.SumT C.ErrorT tb)
      ->  C.VLeft  <$> valueToCore x C.ErrorT
      <|> C.VRight <$> valueToCore x tb

     (_, _)
      -> Nothing


valueFromCore :: C.BaseValue -> Maybe D.Value
valueFromCore = \case
  C.VBool   x -> Just (D.BooleanValue x)
  C.VInt    x -> Just (D.IntValue x)
  C.VDouble x -> Just (D.DoubleValue x)
  C.VTime   x -> Just (D.TimeValue x)
  C.VString x -> Just (D.StringValue x)
  C.VUnit     -> Just (D.IntValue 13013)
  C.VError  _ -> Just D.Tombstone
  C.VLeft   x -> valueFromCore x
  C.VRight  x -> valueFromCore x
  C.VNone     -> Just D.Tombstone
  C.VSome   x -> valueFromCore x
  C.VPair x y -> D.PairValue <$> valueFromCore x <*> valueFromCore y
  C.VArray xs -> D.ListValue . D.List <$> traverse valueFromCore xs
  C.VBuf   xs -> D.ListValue . D.List <$> traverse valueFromCore xs

  C.VMap kvs
   -> let go (k,v) = (,) <$> valueFromCore k <*> valueFromCore v
      in D.MapValue <$> traverse go (Map.toList kvs)

  C.VStruct xs
   -> let go (C.StructField k, v) = (,) <$> pure k <*> valueFromCore v
      in D.StructValue . D.Struct <$> traverse go (Map.toList xs)

