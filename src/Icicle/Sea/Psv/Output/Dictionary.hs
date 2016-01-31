{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Sea.Psv.Output.Dictionary
  ( seaOutputDict
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy (writeFile)
import           Data.Text (Text)
import           Data.Map  (Map)
import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Vector         as Vector
import           System.IO (IO, FilePath)

import           Icicle.Avalanche.Program (Program)
import           Icicle.Avalanche.Prim.Flat (Prim)
import           Icicle.Common.Annot (Annot)
import           Icicle.Common.Base
import           Icicle.Common.Type
import           Icicle.Data (Attribute(..))
import           Icicle.Sea.FromAvalanche.State
import           Icicle.Sea.Error (SeaError(..))
import           Icicle.Internal.Pretty (Pretty)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

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
  => FilePath
  -> Text
  -> [(Attribute, Program (Annot a) n Prim)]
  -> EitherT SeaError IO ()
seaOutputDict filepath tombstone programs
  = do states  <- hoistEither
                $ zipWithM (\ix (a, p) -> stateOfProgram ix a p) [0..] programs
       let dict = encode (seaOutputSchema tombstone states)
       liftIO $ writeFile filepath dict

seaOutputSchema :: Text -> [SeaProgramState] -> Value
seaOutputSchema t st
  = schemaToJson (const encodeAttribute) encodingVersion
      $ SchemaJsonV1
      $ outputsToSchema t
      $ m
  where
    m = outputs st

--------------------------------------------------------------------------------

outputsToSchema :: Text -> Map Output ValType -> Schema GlobalProps ValType
outputsToSchema tombstone attrs
  = Schema
      formationVersion
      encodingVersion
      (StringEncoding 0)
      (GlobalProps tombstone)
      (outputsToAttributeSchema attrs)

outputsToAttributeSchema :: Map Output ValType -> [AttributeSchema ValType]
outputsToAttributeSchema attrs
  = let names       = Map.toList attrs
        go i (k, v) = AttributeSchema k i v
    in  List.zipWith go [0..] names

outputs :: [SeaProgramState] -> Map Output ValType
outputs states
  = let attrs = List.concatMap (fmap (unOutputName . fst) . stateOutputs) states
        types = List.concatMap (fmap (fst          . snd) . stateOutputs) states
    in  Map.fromList (List.zip attrs types)

--------------------------------------------------------------------------------

encodeAttributes :: Map Output ValType -> [Value]
encodeAttributes = fmap encodeAttribute . Map.elems

encodeAttribute :: ValType -> Value
encodeAttribute = encodeType

-- | Translate an Icicle output type to JSON.
--
encodeType :: ValType -> Value
encodeType ty = case ty of
  BoolT     -> Bool False
  TimeT     -> String ""
  DoubleT   -> Number 0
  IntT      -> Number 0
  StringT   -> String ""
  UnitT     -> emptyObject
  ErrorT    -> Null

  ArrayT a
   -> array [encodeType a]

  -- In Sea:
  -- Map is an array of size-two-arrays.
  MapT k v
   -> array [array [encodeType k, encodeType v]]

  -- None becomes missing_value, Some is just the value
  OptionT a -> encodeType a

  -- Pair is actually a size-two-array
  PairT a b
   -> array [encodeType a, encodeType b]

  -- In dense PSV, an error is always missing_value
  SumT ErrorT v
    -> encodeType v
  -- Sum with anything else is not currently supported in C codegen, but is a valid type
  SumT a b
    -> array [array [encodeType a, encodeType b]]

  -- Struct is just an array of size-two-arrays (field name and value)
  StructT s
    -> array $ encodeStruct s

  BufT _ a
   -> array [encodeType a]

  where
    array = Array . Vector.fromList

    encodeStruct
      = encodeAttributes . Map.mapKeys nameOfStructField . getStructType

