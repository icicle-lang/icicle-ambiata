{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Icicle.Compiler.Sea (
    seaEval
  , fromInputs
  , fromOutputs

  , CompilerSeaError(..)
  , renderCompilerSeaError
  ) where

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import qualified Icicle.Avalanche.Prim.Flat as Flat
import qualified Icicle.Common.Base as Common
import qualified Icicle.Common.Data as Common
import qualified Icicle.Common.Eval as Common
import qualified Icicle.Common.Type as Common
import           Icicle.Compiler
import qualified Icicle.Compiler.Source as Source
import           Icicle.Data
import           Icicle.Data.Time
import           Icicle.Internal.Rename
import qualified Icicle.Runtime.Data as Runtime
import qualified Icicle.Runtime.Data.Logical as Logical
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped
import qualified Icicle.Runtime.Evaluator as Runtime
import qualified Icicle.Sea.Data as Sea
import qualified Icicle.Source.Query as Query

import           System.IO (IO)

import           P

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)


data CompilerSeaError =
    CompilerSeaLogicalError !Logical.LogicalError
  | CompilerSeaStripedError !Striped.StripedError
  | CompilerSeaSchemaError !Schema.SchemaError
  | CompilerSeaValueToCore !Value !Common.ValType
  | CompilerSeaValueFromCore !Common.BaseValue
  | CompilerSeaRuntimeError !Runtime.RuntimeError
  | CompilerSeaNoInputs
  | CompilerSeaNoOutputs
  | CompilerSeaNoFactLoop
    deriving (Eq, Show)

renderCompilerSeaError :: CompilerSeaError -> Text
renderCompilerSeaError = \case
  CompilerSeaLogicalError x ->
    Logical.renderLogicalError x
  CompilerSeaStripedError x ->
    Striped.renderStripedError x
  CompilerSeaSchemaError x ->
    Schema.renderSchemaError x
  CompilerSeaRuntimeError x ->
    Runtime.renderRuntimeError x
  CompilerSeaValueToCore x typ ->
    "Cannot convert value to core: " <> Text.pack (show x) <> " (should have type: " <> Text.pack (show typ) <> ")"
  CompilerSeaValueFromCore x ->
    "Cannot convert value from core: " <> Text.pack (show x)
  CompilerSeaNoFactLoop ->
    "Avalanche program had no fact loop"
  CompilerSeaNoInputs ->
    "C evaluator allowed no inputs"
  CompilerSeaNoOutputs ->
    "C evaluator produced no outputs"

fromInputs :: InputId -> Common.ValType -> Map Entity [AsAt Common.BaseValue] -> Either CompilerSeaError Runtime.Input
fromInputs iid typ kvss = do
  let
    entityKeys =
      Boxed.fromList .
      fmap (Runtime.EntityKey (Runtime.EntityHash 0) . Runtime.EntityId . Text.encodeUtf8 . getEntity) $
      Map.keys kvss

    vss =
      Map.elems kvss

    lens =
      Storable.fromList $ fmap (fromIntegral . length) vss

    times :: Storable.Vector Runtime.Time64 =
      Storable.fromList $ fmap (Runtime.Time64 . packedOfTime . atTime) (concat vss)

  (tombstones, values) <-
    fmap List.unzip . first CompilerSeaLogicalError $
      traverse (Logical.fromTopValue typ . atFact) (concat vss)

  schema <-
    first CompilerSeaSchemaError $
      Schema.fromValType typ

  column <-
    first CompilerSeaStripedError $
      Striped.fromLogical schema (Boxed.fromList values)

  let
    icolumn =
      Runtime.InputColumn lens times (Storable.fromList tombstones) column

    inputs =
      Map.singleton iid icolumn

  pure $
    Runtime.Input entityKeys inputs

fromOutputs :: Striped.Column -> Either CompilerSeaError [Common.BaseValue]
fromOutputs column = do
  xs <- bimap CompilerSeaStripedError Boxed.toList $ Striped.toLogical column
  first CompilerSeaLogicalError $ traverse (Logical.toBaseValue (Schema.toValType (Striped.schema column))) xs

fromFacts :: InputName -> Common.ValType -> [AsAt Fact] -> Either CompilerSeaError (Map Entity [AsAt Common.BaseValue])
fromFacts name typ0 facts0 = do
  let
    facts1 =
      List.filter ((== name) . factAttribute . atFact) facts0

    typ1 =
      Common.SumT Common.ErrorT typ0

    fromFact (AsAt (Fact e _ x0) t) = do
      x <- maybeToRight (CompilerSeaValueToCore x0 typ1) $ Common.valueToCore x0 typ1
      pure (e, [AsAt x t])

  facts2 <- traverse fromFact facts1
  pure $ Map.fromListWith (flip (<>)) facts2

seaEval ::
     Common.EvalContext
  -> [AsAt Fact]
  -> Source.QueryTyped Source.Var
  -> AvalProgramTyped  Source.Var Flat.Prim
  -> EitherT CompilerSeaError IO [Result]
seaEval ctx facts0 (renameQT unVar -> query) program = do
  let
    iid =
      case Query.queryInput query of
        QualifiedInput x ->
          x
        UnqualifiedInput name ->
          InputId [namespace|default|] name

    maxMapSize =
      Runtime.MaximumMapSize . fromIntegral $ Common.evalMaxMapSize ctx

    stime =
      Runtime.SnapshotTime . Runtime.Time64 . packedOfTime $ Common.evalSnapshotTime ctx

  context <- hoistEither . first CompilerSeaRuntimeError . Runtime.compileAvalanche $
    Runtime.AvalancheContext "Icicle.Compiler.Sea.seaEval" (Map.singleton iid (program :| []))
  runtime <- firstT CompilerSeaRuntimeError $ Runtime.compileSea Runtime.SkipJetskiCache context

  cluster <- hoistEither . maybeToRight CompilerSeaNoInputs . listToMaybe . Map.elems $ Runtime.runtimeClusters runtime

  let
    inputType =
      Schema.toValType . Runtime.clusterInputSchema $
        Sea.clusterAnnotation cluster

  facts1 <- hoistEither $ fromFacts (inputName iid) inputType facts0
  input <- hoistEither $ fromInputs iid inputType facts1

  output0 <- firstT CompilerSeaRuntimeError $ Runtime.snapshotBlock runtime maxMapSize stime input
  output1 <- hoistEither . maybeToRight CompilerSeaNoOutputs . listToMaybe . Map.elems $ Runtime.outputColumns output0

  let
    entities =
      fmap (Entity . Text.decodeUtf8 . Runtime.unEntityId . Runtime.entityId . Runtime.snapshotEntity) .
      Boxed.toList $
      Runtime.outputKey output0

  output2 <- hoistEither $ fromOutputs output1
  output3 <- hoistEither $ traverse (\x -> maybeToRight (CompilerSeaValueFromCore x) $ Common.valueFromCore x) output2

  pure .
    fmap Result $
    List.zipWith ((,)) entities output3
