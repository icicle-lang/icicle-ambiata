{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Header (
    Header(..)
  , Fingerprint(..)
  , renderHeader
  , parseHeader

  , HeaderDecodeError(..)
  , renderHeaderDecodeError

  , pFingerprint
  , pStructType
  , pValType
  , pMeltedType
  , pClusterId
  , pKernelIndex
  , pKernelId
  , pSeaName
  , pInputId
  , pOutputId
  , pMeltedOutput
  , pNamedType
  , pKernel
  , pCluster
  , pHeader

  , ppFingerprint
  , ppStructType
  , ppValType
  , ppMeltedType
  , ppClusterId
  , ppKernelIndex
  , ppKernelId
  , ppSeaName
  , ppInputId
  , ppOutputId
  , ppMeltedOutput
  , ppNamedType
  , ppKernel
  , ppCluster
  , ppHeader
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.List as List
import           Data.String (IsString)
import qualified Data.Text as Text

import           Icicle.Avalanche.Prim.Flat (meltType)
import           Icicle.Common.Type
import           Icicle.Data.Name
import           Icicle.Internal.Aeson
import           Icicle.Sea.Data
import           Icicle.Sea.Name

import           GHC.Generics (Generic)

import           P

import           X.Text.Show (gshowsPrec)

data Header =
  Header {
      headerFingerprint :: !Fingerprint
    , headerClusters :: ![Cluster () ()]
    } deriving (Eq, Ord, Show, Generic)

newtype Fingerprint =
  Fingerprint {
      unFingerprint :: Text
    } deriving (Eq, Ord, IsString, Generic)

instance Show Fingerprint where
  showsPrec =
    gshowsPrec

data HeaderDecodeError =
    HeaderDecodeError !AesonDecodeError
  | HeaderMalformedEnvelope
    deriving (Eq, Show)

renderHeaderDecodeError :: HeaderDecodeError -> Text
renderHeaderDecodeError = \case
  HeaderDecodeError err ->
    renderAesonDecodeError err
  HeaderMalformedEnvelope ->
    "Failed reading header from C dictionary, envelope was malformed"

pFingerprint :: Aeson.Value -> Aeson.Parser Fingerprint
pFingerprint =
  fmap Fingerprint . pText

ppFingerprint :: Fingerprint -> Aeson.Value
ppFingerprint =
  ppText . unFingerprint

pStructType :: Aeson.Value -> Aeson.Parser StructType
pStructType =
  fmap StructType . pMap "StructType" (pure . StructField) pValType

ppStructType :: StructType -> Aeson.Value
ppStructType =
  ppMap nameOfStructField ppValType . getStructType

pValType :: Aeson.Value -> Aeson.Parser ValType
pValType =
  pEnum "ValType" $ \case
    "bool" ->
      Just $
        (BoolT <$) . pUnit

    "time" ->
      Just $
        (TimeT <$) . pUnit

    "double" ->
      Just $
        (DoubleT <$) . pUnit

    "int" ->
      Just $
        (IntT <$) . pUnit

    "string" ->
      Just $
        (StringT <$) . pUnit

    "unit" ->
      Just $
        (UnitT <$) . pUnit

    "error" ->
      Just $
        (ErrorT <$) . pUnit

    "array" ->
      Just $
        fmap ArrayT . pValType

    "map" ->
      Just . Aeson.withObject "MapT" $ \o ->
        MapT
          <$> withKey "key" o pValType
          <*> withKey "value" o pValType

    "option" ->
      Just $
        fmap OptionT . pValType

    "pair" ->
      Just . Aeson.withObject "PairT" $ \o ->
        PairT
          <$> withKey "first" o pValType
          <*> withKey "second" o pValType

    "sum" ->
      Just . Aeson.withObject "SumT" $ \o ->
        SumT
          <$> withKey "left" o pValType
          <*> withKey "right" o pValType

    "struct" ->
      Just $
        fmap StructT . pStructType

    "buf" ->
      Just . Aeson.withObject "BufT" $ \o ->
        BufT
          <$> withKey "size" o pInt
          <*> withKey "element" o pValType

    x ->
      fail $ "Unknown type: " <> Text.unpack x

ppValType :: ValType -> Aeson.Value
ppValType = \case
 BoolT ->
   ppEnum "bool" ppUnit

 TimeT ->
   ppEnum "time" ppUnit

 DoubleT ->
   ppEnum "double" ppUnit

 IntT ->
   ppEnum "int" ppUnit

 StringT ->
   ppEnum "string" ppUnit

 UnitT ->
   ppEnum "unit" ppUnit

 ErrorT ->
   ppEnum "error" ppUnit

 ArrayT x ->
   ppEnum "array" $
     ppValType x

 MapT k v ->
   ppEnum "map" $
     Aeson.object [
         "key" .=
           ppValType k
       , "value" .=
           ppValType v
       ]

 OptionT x ->
   ppEnum "option" $
     ppValType x

 PairT x y ->
   ppEnum "pair" $
     Aeson.object [
         "first" .=
           ppValType x
       , "second" .=
           ppValType y
       ]

 SumT x y ->
   ppEnum "sum" $
     Aeson.object [
         "left" .=
           ppValType x
       , "right" .=
           ppValType y
       ]

 StructT x ->
   ppEnum "struct" $
     ppStructType x

 BufT n x ->
   ppEnum "buf" $
     Aeson.object [
         "size" .=
           ppInt n
       , "element" .=
           ppValType x
       ]

pMeltedType :: Aeson.Value -> Aeson.Parser MeltedType
pMeltedType =
  Aeson.withObject "MeltedType" $ \o -> do
    logical <- withKey "logical" o pValType
    melted0 <- withKey "melted" o $ pList "Array ValType" pValType

    let
      melted =
        meltType logical

    when (melted0 /= melted) $
      fail $
        "serialised melted type did not match calculated melted type:" <>
        "\n" <>
        "\n  logical type =" <>
        "\n    " <> show logical <>
        "\n" <>
        "\n  serialised melted type =" <>
        "\n    " <> show melted0 <>
        "\n" <>
        "\n  calculated melted type =" <>
        "\n    " <> show melted

    pure $
      MeltedType logical melted

ppMeltedType :: MeltedType -> Aeson.Value
ppMeltedType x =
  Aeson.object [
      "logical" .=
        ppValType (typeLogical x)
    , "melted" .=
        ppList ppValType (typeMelted x)
    ]

pClusterId :: Aeson.Value -> Aeson.Parser ClusterId
pClusterId =
  fmap ClusterId . pInt

ppClusterId :: ClusterId -> Aeson.Value
ppClusterId =
  ppInt . unClusterId

pKernelIndex :: Aeson.Value -> Aeson.Parser KernelIndex
pKernelIndex =
  fmap KernelIndex . pInt

ppKernelIndex :: KernelIndex -> Aeson.Value
ppKernelIndex =
  ppInt . unKernelIndex

pKernelId :: Aeson.Value -> Aeson.Parser KernelId
pKernelId =
  Aeson.withObject "KernelId" $ \o ->
    KernelId
      <$> withKey "cluster" o pClusterId
      <*> withKey "kernel" o pKernelIndex

ppKernelId :: KernelId -> Aeson.Value
ppKernelId x =
  Aeson.object [
      "cluster" .=
        ppClusterId (kernelCluster x)
    , "kernel" .=
        ppKernelIndex (kernelIndex x)
    ]

pSeaName :: Aeson.Value -> Aeson.Parser SeaName
pSeaName =
  pRendered "SeaName" parseSeaName

ppSeaName :: SeaName -> Aeson.Value
ppSeaName =
  ppRendered renderSeaName

pInputId :: Aeson.Value -> Aeson.Parser InputId
pInputId =
  pRendered "InputId" parseInputId

ppInputId :: InputId -> Aeson.Value
ppInputId =
  ppRendered renderInputId

pOutputId :: Aeson.Value -> Aeson.Parser OutputId
pOutputId =
  pRendered "OutputId" parseOutputId

ppOutputId :: OutputId -> Aeson.Value
ppOutputId =
  ppRendered renderOutputId

pMeltedOutput :: Aeson.Value -> Aeson.Parser (OutputId, MeltedType)
pMeltedOutput =
  Aeson.withObject "(OutputId, MeltedType)" $ \o ->
    (,)
      <$> withKey "output_id" o pOutputId
      <*> withKey "type" o pMeltedType

ppMeltedOutput :: (OutputId, MeltedType) -> Aeson.Value
ppMeltedOutput (oid, mtype) =
  Aeson.object [
      "output_id" .=
        ppOutputId oid
    , "type" .=
        ppMeltedType mtype
    ]

pNamedType :: Aeson.Value -> Aeson.Parser (SeaName, ValType)
pNamedType =
  Aeson.withObject "(SeaName, ValType)" $ \o ->
    (,)
      <$> withKey "name" o pSeaName
      <*> withKey "type" o pValType

ppNamedType :: (SeaName, ValType) -> Aeson.Value
ppNamedType (name, vtype) =
  Aeson.object [
      "name" .=
        ppSeaName name
    , "type" .=
        ppValType vtype
    ]

pKernel :: Aeson.Value -> Aeson.Parser (Kernel ())
pKernel =
  Aeson.withObject "Kernel" $ \o ->
    Kernel
      <$> withKey "kernel_id" o pKernelId
      <*> withKey "resumables" o (pList "Array (SeaName, ValType)" pNamedType)
      <*> withKey "outputs" o (pList "Array (OutputId, MeltedType)" pMeltedOutput)
      <*> pure ()

ppKernel :: Kernel () -> Aeson.Value
ppKernel x =
  Aeson.object [
      "kernel_id" .=
        ppKernelId (kernelId x)
    , "resumables" .=
        ppList ppNamedType (kernelResumables x)
    , "outputs" .=
        ppList ppMeltedOutput (kernelOutputs x)
    ]

pCluster :: Aeson.Value -> Aeson.Parser (Cluster () ())
pCluster =
  Aeson.withObject "Cluster" $ \o ->
    Cluster
      <$> withKey "cluster_id" o pClusterId
      <*> withKey "input_id" o pInputId
      <*> withKey "input_type" o pValType
      <*> withKey "input_vars" o (pList "Array (SeaName, ValType)" pNamedType)
      <*> withKey "time_var" o pSeaName
      <*> withKey "kernels" o (pNonEmpty "NonEmpty Kernel" pKernel)
      <*> pure ()

ppCluster :: Cluster () () -> Aeson.Value
ppCluster x =
  Aeson.object [
      "cluster_id" .=
        ppClusterId (clusterId x)
    , "input_id" .=
        ppInputId (clusterInputId x)
    , "input_type" .=
        ppValType (clusterInputType x)
    , "input_vars" .=
        ppList ppNamedType (clusterInputVars x)
    , "time_var" .=
        ppSeaName (clusterTimeVar x)
    , "kernels" .=
        ppNonEmpty ppKernel (clusterKernels x)
    ]

pHeader :: Aeson.Value -> Aeson.Parser Header
pHeader =
  Aeson.withObject "Header" $ \o -> do
    expectKey "header" "version" o pText "v0"
    Header
      <$> withKey "fingerprint" o pFingerprint
      <*> withKey "clusters" o (pList "Map InputId Cluster" pCluster)

ppHeader :: Header -> Aeson.Value
ppHeader x =
  Aeson.object [
      "version" .=
        ppText "v0"
    , "fingerprint" .=
        ppFingerprint (headerFingerprint x)
    , "clusters" .=
        ppList ppCluster (headerClusters x)
    ]

headerKeyOrder :: [Text]
headerKeyOrder = [
  -- Header
    "version"
  , "fingerprint"
  , "clusters"

  -- Cluster
  , "cluster_id"
  , "input_id"
  , "input_type"
  , "input_vars"
  , "time_var"
  , "kernels"

  -- Kernel
  , "kernel_id"
  , "resumables"
  , "outputs"

  -- (Name, MeltedType)
  -- (OutputId, MeltedType)
  , "name"
  , "output_id"
  , "type"

  -- KernelId
  , "cluster"
  , "kernel"

  -- MeltedType
  , "logical"
  , "melted"

  -- ValType
  , "key"
  , "value"
  , "first"
  , "second"
  , "left"
  , "right"
  , "size"
  , "element"
  ]

divider :: [Text]
divider = [
    "//---------------------"
  , "// ICICLE C DICTIONARY"
  , "//---------------------"
  , "//"
  ]

startHeader :: Text
startHeader =
  Text.unlines divider

endHeader :: Text
endHeader =
  Text.unlines $
    List.reverse divider

comment :: Text -> Text
comment =
  Text.unlines . fmap ("// " <>) . Text.lines

dropPrefix :: Text -> Text -> Maybe Text
dropPrefix prefix x =
  let
    !n =
      Text.length prefix

    (x0, x1) =
      Text.splitAt n x
  in
    if x0 /= prefix then
      Nothing
    else
      Just x1

uncommentLine :: Text -> Maybe Text
uncommentLine =
  dropPrefix "// "

uncomment :: Text -> Maybe Text
uncomment =
  fmap Text.unlines . traverse uncommentLine . Text.lines

renderEnvelope :: Text -> Text
renderEnvelope x =
  startHeader <> comment x <> endHeader

parseEnvelope :: Text -> Either HeaderDecodeError (Text, Text)
parseEnvelope file =
  let
    (header, code) =
      Text.breakOn endHeader file
  in
    (,)
      <$> maybeToRight HeaderMalformedEnvelope (uncomment =<< dropPrefix startHeader header)
      <*> maybeToRight HeaderMalformedEnvelope (dropPrefix endHeader code)

renderHeader :: Header -> Text
renderHeader =
  renderEnvelope . encodePrettyJson headerKeyOrder . ppHeader

parseHeader :: Text -> Either HeaderDecodeError (Header, Text)
parseHeader file = do
  (header0, code) <- parseEnvelope file
  header <- first HeaderDecodeError $ decodeJson pHeader header0
  pure (header, code)
