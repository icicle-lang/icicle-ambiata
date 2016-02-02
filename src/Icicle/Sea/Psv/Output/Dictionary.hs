{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.Psv.Output.Dictionary
  ( seaOutputDict
  ) where


import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Vector         as Vector

import           Icicle.Avalanche.Program (Program)
import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Data (Attribute(..))
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Internal.Pretty (Pretty)

import           Formation

import           P


data GlobalProps
  = GlobalProps
  { missingValue :: Text }

instance ToJSON GlobalProps where
  toJSON props
    = object [ "missing_value: " .= missingValue props ]

type Output  = Text

formationVersion :: Text
formationVersion = "1"

encodingVersion :: EncodingVersion
encodingVersion = EncodingVersion "1"

seaOutputDict
  :: (Ord n, Pretty n)
  => Text
  -> [(Attribute, Program (Annot a) n Prim)]
  -> Either SeaError ByteString
seaOutputDict tombstone programs
  = do -- Start at 1 because top-level formation attribuets start at 1
       states  <- zipWithM (\ix (a, p) -> stateOfProgram ix a p) [1..] programs
       pure $ encode $ seaOutputSchema tombstone states

seaOutputSchema :: Text -> [SeaProgramState] -> Value
seaOutputSchema t st
  = schemaToJson (const encodeAttribute) encodingVersion
      $ SchemaJsonV1
      $ outputsToSchema t
      $ m
  where
    m = outputs st

--------------------------------------------------------------------------------

outputsToSchema :: Text -> [(Output, ValType)] -> Schema GlobalProps ValType
outputsToSchema tombstone attrs
  = Schema
      formationVersion
      encodingVersion
      (StringEncoding 0)
      (GlobalProps tombstone)
      (outputsToAttributeSchema attrs)

outputsToAttributeSchema :: [(Output, ValType)] -> [AttributeSchema ValType]
outputsToAttributeSchema attrs
  = let go i (k, v) = AttributeSchema k i v
    in  List.zipWith go [0..] attrs

outputs :: [SeaProgramState] -> [(Output, ValType)]
outputs states
  = let attrs = List.concatMap (fmap (unOutputName . fst) . stateOutputs) states
        types = List.concatMap (fmap (fst          . snd) . stateOutputs) states
    in  List.zip attrs types

--------------------------------------------------------------------------------

type FormationAttr = AttributeSchema FormationEncoding

data FormationEncoding
  = Primitive Primitive
  | ListOf    FormationEncoding
  | PairOf    FormationEncoding FormationEncoding
  | Struct    [FormationAttr]

data Primitive
  = PBool
  | PInt
  | PDouble
  | PString
  | PTime
  | PNull

encodingToJSON :: FormationEncoding -> Value
encodingToJSON en = case en of
  Primitive p
    -> object [ "primitive" .= go p ]
  ListOf a
    -> object [ "listof" .= encodingToJSON a ]
  PairOf a b
    -> object [ "pairof" .= array [encodingToJSON a, encodingToJSON b] ]
  Struct as
    -> array $ fmap (attributeSchemaToJson (const encodingToJSON) encodingVersion) as
  where
    go :: Primitive -> Value
    go PBool   = "bool"
    go PInt    = "int"
    go PDouble = "double"
    go PString = "string"
    go PTime   = "time"
    go PNull   = "null"

    array = Array . Vector.fromList

encodeType :: ValType -> FormationEncoding
encodeType ty = case ty of
  BoolT     -> Primitive PBool
  TimeT     -> Primitive PTime
  DoubleT   -> Primitive PDouble
  IntT      -> Primitive PInt
  StringT   -> Primitive PString
  UnitT     -> Primitive PNull
  ErrorT    -> Primitive PNull

  ArrayT a
   -> ListOf $ encodeType a

  -- Map is just an array of size-two-array
  MapT k v
   -> ListOf $ PairOf (encodeType k) (encodeType v)

  -- None becomes missing_value, Some is just the value
  OptionT a -> encodeType a

  PairT a b
   -> PairOf (encodeType a) (encodeType b)

  -- In dense PSV, an error is always missing_value
  SumT ErrorT v
    -> encodeType v
  -- Sum with anything else is not currently supported in C codegen, but is a valid type
  -- We don't do anything with it for now.
  SumT _ _
    -> Primitive PNull

  StructT s
    -> Struct
     . List.zipWith encodeStruct [0..]
     . Map.toList
     . Map.mapKeys nameOfStructField
     . getStructType $ s

  BufT _ a
    -> ListOf $ encodeType a

  where
    encodeStruct i (k, v)
      = AttributeSchema k i (encodeType v)

encodeAttribute :: ValType -> Value
encodeAttribute = encodingToJSON . encodeType
