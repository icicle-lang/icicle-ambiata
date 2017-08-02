{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Icicle.Test.Sea.Header where

import           Disorder.Corpus
import           Disorder.Jack

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Common.Type
import           Icicle.Data.Name
import           Icicle.Sea.Data
import           Icicle.Sea.Header
import           Icicle.Sea.Name

import           P

import qualified Prelude as Savage

import           System.IO (IO)


genFingerprint :: Jack Fingerprint
genFingerprint =
  Fingerprint
    <$> elements muppets

genInputId :: Jack InputId
genInputId =
  justOf . fmap parseInputId $
    (\ns n -> ns <> ":" <> n)
      <$> elements colours
      <*> elements simpsons

genOutputId :: Jack OutputId
genOutputId =
  justOf . fmap parseOutputId $
    (\ns n -> ns <> ":" <> n)
      <$> elements weather
      <*> elements cooking

genStructField :: Jack StructField
genStructField =
  StructField
    <$> elements viruses

genValType :: Jack ValType
genValType =
  oneOfRec [
      pure BoolT
    , pure TimeT
    , pure DoubleT
    , pure IntT
    , pure StringT
    , pure UnitT
    , pure ErrorT
    ] [
      ArrayT <$> genValType
    , MapT <$> genValType <*> genValType
    , OptionT <$> genValType
    , PairT <$> genValType <*> genValType
    , SumT <$> genValType <*> genValType
    , StructT . StructType . Map.fromList
        <$> listOfN 0 10 ((,) <$> genStructField <*> genValType)
    , BufT <$> choose (1, 100) <*> genValType
    ]

genMeltedType :: Jack MeltedType
genMeltedType = do
  vtype <- genValType
  pure $
    MeltedType vtype (meltType vtype)

genClusterId :: Jack ClusterId
genClusterId =
  ClusterId
    <$> choose (0, 10000)

genKernelIndex :: Jack KernelIndex
genKernelIndex =
  KernelIndex
    <$> choose (0, 10000)

genKernelId :: Jack KernelId
genKernelId =
  KernelId
    <$> genClusterId
    <*> genKernelIndex

genSeaName :: Jack SeaName
genSeaName =
  fmap mangle $
    elements (southpark :: [Text])

genKernel :: Jack Kernel
genKernel =
  Kernel
    <$> genKernelId
    <*> listOfN 0 5 ((,) <$> genSeaName <*> genValType)
    <*> listOfN 0 5 ((,) <$> genOutputId <*> genMeltedType)

genCluster :: Jack Cluster
genCluster =
  Cluster
    <$> genClusterId
    <*> genInputId
    <*> genValType
    <*> listOfN 0 5 ((,) <$> genSeaName <*> genValType)
    <*> genSeaName
    <*> (NonEmpty.fromList <$> listOfN 1 10 genKernel)

genHeader :: Jack Header
genHeader =
  Header
    <$> genFingerprint
    <*> listOfN 0 5 genCluster

snippet :: Text
snippet =
  Text.unlines [
      "int main (int argc, char **argv) {"
    , "    return 0;"
    , "}"
    ]

renderHeaderX :: Header -> Text
renderHeaderX x =
  renderHeader x <> snippet

parseHeaderX :: Text -> Either HeaderDecodeError Header
parseHeaderX x = do
  (header, code) <- parseHeader x
  if code /= snippet then
    Savage.error $
      show code <>
      "\n=/=\n" <>
      show snippet
  else
    pure header

prop_header_roundtrip :: Property
prop_header_roundtrip =
  gamble genHeader $
    tripping renderHeaderX parseHeaderX

return []
tests :: IO Bool
tests =
  $quickCheckAll
