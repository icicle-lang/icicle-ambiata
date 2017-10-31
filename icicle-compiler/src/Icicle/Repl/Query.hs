{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
module Icicle.Repl.Query (
    evaluateQuery
  , defineFunction
  ) where

import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.State (gets, modify)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Boxed

import qualified Icicle.Avalanche.Annot as Avalanche
import qualified Icicle.Avalanche.Prim.Flat as Flat
import qualified Icicle.Avalanche.Simp as Avalanche
import           Icicle.Common.Type
import qualified Icicle.Compiler as Compiler
import qualified Icicle.Compiler.Sea as Compiler
import qualified Icicle.Compiler.Source as Source
import qualified Icicle.Core.Program.Check as Core
import           Icicle.Data
import           Icicle.Data.Time (packedOfTime, exclusiveSnapshotTime)
import           Icicle.Dictionary
import           Icicle.Internal.Pretty ((<+>))
import qualified Icicle.Internal.Pretty as Pretty
import           Icicle.Repl.Data
import           Icicle.Repl.Flag
import           Icicle.Repl.Monad
import           Icicle.Repl.Option
import           Icicle.Repl.Pretty
import           Icicle.Repl.Source
import qualified Icicle.Runtime.Data as Runtime
import qualified Icicle.Runtime.Data.Logical as Logical
import           Icicle.Runtime.Data.Schema (SchemaError)
import qualified Icicle.Runtime.Data.Schema as Schema
import qualified Icicle.Runtime.Data.Striped as Striped
import qualified Icicle.Runtime.Evaluator as Runtime
import qualified Icicle.Runtime.Serial.Zebra as Runtime
import qualified Icicle.Sea.Data as Sea
import qualified Icicle.Sea.Eval.Base as Sea
import qualified Icicle.Sea.FromAvalanche.Program as Sea
import qualified Icicle.Sea.Preamble as Sea
import qualified Icicle.Serial as Serial
import qualified Icicle.Source.PrettyAnnot as Source
import qualified Icicle.Source.Query.Query as Source
import qualified Icicle.Storage.Dictionary.Toml as Toml

import           P

import           System.IO (FilePath)
import qualified System.IO as IO
import           System.IO.Error (IOError)

import qualified Text.ParserCombinators.Parsec as Parsec
import           Text.Show.Pretty (ppShow)

import           Viking (Stream, Of)
import qualified Viking.ByteStream as ByteStream
import qualified Viking.Stream as Stream

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT, firstJoin)

import           Zebra.Serial.Binary (BinaryStripedDecodeError)
import qualified Zebra.Serial.Binary as Binary
import           Zebra.Serial.Text (TextStripedEncodeError)
import qualified Zebra.Serial.Text as Text
import qualified Zebra.Table.Striped as Zebra


data CompiledQuery =
  CompiledQuery {
      compiledOutputId :: OutputId
    , compiledSource :: Source.QueryTop (Source.TypeAnnot Parsec.SourcePos) Source.Var
    , compiledCore :: Source.CoreProgramUntyped Source.Var
    , compiledAvalanche :: Maybe (Compiler.AvalProgramTyped Source.Var Flat.Prim)
    }

data QueryError =
   QueryDecodeError Serial.ParseError
 | QuerySourceError !(Source.ErrorSource Source.Var)
 | QueryCompilerError !(Compiler.ErrorCompile Source.Var)
 | QueryRuntimeError !Runtime.RuntimeError
 | QueryIOError !IOError
 | QueryRuntimeZebraSchemaError !Runtime.ZebraSchemaError
 | QueryRuntimeZebraStripedError !Runtime.ZebraStripedError
 | QueryBinaryStripedDecodeError !BinaryStripedDecodeError
 | QueryTextStripedEncodeError !TextStripedEncodeError
 | QueryZebraStripedError !Zebra.StripedError
 | QueryStripedError !Striped.StripedError
 | QueryLogicalError !Logical.LogicalError
 | QuerySchemaError !SchemaError
 | QueryUnexpectedInputType !ValType
 | QueryOutputMissing !OutputId

posOfError :: QueryError -> Maybe Parsec.SourcePos
posOfError = \case
  QuerySourceError x ->
    Source.annotOfError x
  QueryCompilerError x ->
    Compiler.annotOfError x
  _ ->
    Nothing

ppQueryError :: QueryError -> Pretty.Doc
ppQueryError = \case
  QueryDecodeError x ->
    Pretty.vsep [
        "PSV decode error:"
      , Pretty.indent 2 $ Pretty.pretty x
      ]

  QuerySourceError x ->
    Pretty.pretty x

  QueryCompilerError x ->
    Pretty.pretty x

  QueryRuntimeError x ->
    Pretty.text (ppShow x)

  QueryIOError x ->
    Pretty.text (ppShow x)

  QueryRuntimeZebraSchemaError x ->
    Pretty.text (ppShow x) -- FIXME render

  QueryRuntimeZebraStripedError x ->
    Pretty.text (ppShow x) -- FIXME render

  QueryBinaryStripedDecodeError x ->
    Pretty.text . Text.unpack $ Binary.renderBinaryStripedDecodeError x

  QueryTextStripedEncodeError x ->
    Pretty.text . Text.unpack $ Text.renderTextStripedEncodeError x

  QueryZebraStripedError x ->
    Pretty.text . Text.unpack $ Zebra.renderStripedError x

  QueryStripedError x ->
    Pretty.text . Text.unpack $ Striped.renderStripedError x

  QueryLogicalError x ->
    Pretty.text . Text.unpack $ Logical.renderLogicalError x

  QuerySchemaError x ->
    Pretty.text . Text.unpack $ Schema.renderSchemaError x

  QueryUnexpectedInputType x ->
    "Unexpected cluster input type: " <> Pretty.pretty x

  QueryOutputMissing x ->
    "Output missing for " <> Pretty.pretty x

runEitherRepl :: EitherT QueryError Repl () -> Repl ()
runEitherRepl ee = do
  e <- runEitherT ee
  case e of
    Left err ->
      putError (ppQueryError err) (posOfError err)
    Right () ->
      pure ()

readPsvFacts :: MonadIO m => Dictionary -> FilePath -> EitherT QueryError m [AsAt Fact]
readPsvFacts dictionary path = do
  psv <- Text.lines . Text.decodeUtf8 <$> liftIO (ByteString.readFile path)
  hoistEither . first QueryDecodeError $ traverse (Serial.decodeEavt dictionary) psv

ppResults :: [Compiler.Result] -> Pretty.Doc
ppResults = \case
  [] ->
    Pretty.prettyPunctuation "<no output>"
  kvs ->
    Pretty.vsep . with kvs $ \(Compiler.Result (k, v)) ->
      Pretty.align (Pretty.pretty k) <> "|" <> Pretty.align (Pretty.pretty v)

defineFunction :: String -> Repl ()
defineFunction function =
  runEitherRepl $ do
    parsed <-
      hoistEither . first QuerySourceError .
        Source.sourceParseF "<interactive>" $ Text.pack function

    dictionary <- gets stateDictionary

    let
      names =
        Set.fromList $ fmap (snd . fst) parsed

      -- Remove the old bindings with these names
      funEnv =
        List.filter (not . flip Set.member names . fst) .
        Toml.toFunEnv $
        dictionaryFunctions dictionary

    (funEnv', logs) <-
      hoistEither . first QuerySourceError $
        Source.sourceCheckFunLog funEnv parsed

    let
      fundefs =
        List.filter (flip Set.member names . fst) funEnv'

    for_ (List.zip fundefs logs) $ \((nm, (typ, annot)), log0) -> do
      whenSet FlagType $
        putSection "Type" $
          Pretty.prettyTypedBest
            (Pretty.annotate Pretty.AnnBinding $ Pretty.pretty nm)
            (Pretty.pretty typ)

      whenSet FlagAnnotated $
        putSection "Annotated" $
          Pretty.annotate Pretty.AnnBinding (Pretty.pretty nm) <+> Pretty.pretty (Source.PrettyAnnot annot)

      whenSet FlagTypeCheckLog . for_ log0 $ \x -> do
        putPretty x
        liftIO $ IO.putStrLn ""

    modify $ \s ->
      s { stateDictionary = dictionary { dictionaryFunctions = Toml.fromFunEnv funEnv' } }

compileQuery :: String -> EitherT QueryError Repl CompiledQuery
compileQuery query = do
  dictionary <- gets stateDictionary

  --
  -- Source.
  --
  let
    sourceOid =
      [outputid|repl:output|]

  timeCompileSource <- timeSection "Compile Source"

  parsed <-
    hoistEither . first QuerySourceError $
      Source.sourceParseQT sourceOid (Text.pack query)

  options <- lift getCheckOptions

  (annot, sourceType) <-
    hoistEither . first QuerySourceError $
      Source.sourceCheckQT options dictionary parsed

  whenSet FlagType $
    putSection "Type" $
      Pretty.prettyTypedBest
        (Pretty.annotate Pretty.AnnBinding $ Pretty.pretty sourceOid)
        (Pretty.pretty sourceType)

  whenSet FlagAnnotated $
    putSection "Annotated" (Source.PrettyAnnot annot)

  let
    inlined =
      Source.sourceInline Source.defaultInline dictionary annot

  blanded <-
    hoistEither . first QuerySourceError $
      Source.sourceDesugarQT inlined

  whenSet FlagInlined $
    putSection "Inlined" inlined

  whenSet FlagDesugar $
    putSection "Desugar" blanded

  (annobland, _) <-
    hoistEither . first QuerySourceError $
      Source.sourceCheckQT options dictionary blanded

  let
    reified =
      Source.sourceReifyQT annobland

  whenSet FlagReified $ do
    putSection "Reified" reified
    putSection "Reified annotated" (Source.PrettyAnnot reified)

  let
    finalSource =
      reified

  timeCompileSource
  timeCompileCore <- timeSection "Compile Core"

  --
  -- Core, simplified and unsimplified.
  --

  coreUnsimped <-
    hoistEither . first QueryCompilerError $
      Compiler.sourceConvert dictionary finalSource

  core <-
    ifSet FlagCoreSimp
      (pure $ Compiler.coreSimp coreUnsimped)
      (pure coreUnsimped)

  whenSet FlagCore $
    ifSet FlagCoreSimp
      (putSection "Core (simplified)" core)
      (putSection "Core (not simplified)" core)

  case Core.checkProgram core of
    Left err ->
      putSection "Core type error" err

    Right coreType ->
      whenSet FlagCoreType .
        putSection "Core type" . Pretty.vsep . with coreType $ \(oid, typ) ->
          Pretty.prettyTypedBest (Pretty.annotate Pretty.AnnBinding $ Pretty.pretty oid) (Pretty.pretty typ)

  timeCompileCore
  timeCompileAvalanche <- timeSection "Compile Avalanche"

  --
  -- Avalanche, simplified.
  --

  let
    avalancheSimped =
      Compiler.coreAvalanche core

  whenSet FlagAvalanche $
    putSection "Avalanche (simplified)" avalancheSimped

  timeCompileAvalanche
  timeCompileFlatten <- timeSection "Compile Flattened Avalanche"

  --
  -- Flatten Avalanche, not simplified.
  --

  case Compiler.flattenAvalancheUntyped avalancheSimped of
    Left err ->
      putSection "Flatten Avalanche (not simplified) error" err

    Right avalancheFlatUnsimped ->
      case Compiler.checkAvalanche $ Avalanche.eraseAnnotP avalancheFlatUnsimped of
        Left err -> do
          whenSet FlagFlattenNoSimp $
            putSection "Flattened Avalanche (not simplified)" avalancheFlatUnsimped
          putSection "Flattened Avalanche (not simplified) type error" err
        Right flatUnsimpedChecked ->
          whenSet FlagFlattenNoSimp $
            putSection "Flattened Avalanche (not simplified), typechecked" flatUnsimpedChecked

  --
  -- Flattened Avalanche, simplified.
  --

  avalancheFlatSimped <-
    ifSet FlagFlattenSimpCheck
      (pure $ Compiler.coreFlatten_ (Avalanche.SimpOpts True True) core)
      (pure $ Compiler.coreFlatten core)

  avalancheFlatSimped `seq` return ()
  timeCompileFlatten

  timeCompileCheck <- timeSection "Compile Flattened Avalanche Check"

  ret <- case avalancheFlatSimped of
    Left err -> do
      putSection "Flatten Avalanche (simplified) error" err
      pure $
        CompiledQuery sourceOid finalSource core Nothing

    Right flatSimped -> do
      whenSet FlagFlattenSimp $
        putSection "Flattened (simplified), not typechecked" flatSimped

      --
      -- Flattened Avalanche, simplified, check.
      --

      case Compiler.checkAvalanche flatSimped of
        Left err -> do
          putSection "Flattened Avalanche (simplified) type error" err
          pure $
            CompiledQuery sourceOid finalSource core Nothing

        Right flatChecked -> do
          whenSet FlagFlattenSimp $
            putSection "Flattened Avalanche (simplified), typechecked" flatChecked

          pure $
            CompiledQuery sourceOid finalSource core (Just flatChecked)

  timeCompileCheck
  return ret

evaluateCore :: CompiledQuery -> [AsAt Fact] -> Repl ()
evaluateCore compiled facts =
  whenSet FlagCoreEval $ timeSectionWith "Evaluate Core" $ do
    context <- getEvalContext
    case Compiler.coreEval context facts (compiledSource compiled) (compiledCore compiled) of
      Left err ->
        putSection "Core evaluation error" err
      Right x ->
        putSection "Core evaluation" $ ppResults x

evaluateAvalanche :: CompiledQuery -> [AsAt Fact] -> Repl ()
evaluateAvalanche compiled facts = do
  case compiledAvalanche compiled of
    Nothing ->
      pure ()
    Just avalanche ->
      whenSet FlagAvalancheEval $ timeSectionWith "Evaluate Avalanche" $ do
        context <- getEvalContext
        case Compiler.avalancheEval context facts (compiledSource compiled) (Avalanche.reannotP (const ()) avalanche) of
          Left err ->
            putSection "Avalanche evaluation error" err

          Right x ->
            putSection "Avalanche evaluation" $ ppResults x

renderCompiledSea :: CompiledQuery -> EitherT QueryError Repl ()
renderCompiledSea compiled =
  case compiledAvalanche compiled of
    Nothing ->
      pure ()
    Just avalanche -> do
      let
        flatList =
          avalanche :| []

      whenSet FlagSeaPreamble $
        putSection "C preamble" Sea.seaPreamble

      dictionary <- gets stateDictionary

      iid <-
        hoistEither . first QueryCompilerError $
          Compiler.sourceInputId (compiledSource compiled) dictionary

      cflags <- lift getCFlags

      whenSet FlagSea $
        case Sea.seaOfPrograms 0 iid flatList of
          Left err ->
            putSection "C error" err
          Right x ->
            putSection "C" x

      whenSet FlagSeaAssembly $ do
        result <- timeSectionWith "Compile Sea to Assembly" $ liftIO . runEitherT $ Sea.assemblyOfPrograms cflags "icicle-repl" [(iid, flatList)]
        case result of
          Left err ->
            putSection "C assembly error" err
          Right x ->
            putSection "C assembly" x

      whenSet FlagSeaLLVM $ do
        result <- timeSectionWith "Compile Sea to LLVM" $ liftIO . runEitherT $ Sea.irOfPrograms cflags "icicle-repl" [(iid, flatList)]
        case result of
          Left err ->
            putSection "C LLVM IR error" err
          Right x ->
            putSection "C LLVM IR" x

evaluateSea :: CompiledQuery -> [AsAt Fact] -> Repl ()
evaluateSea compiled facts =
  case compiledAvalanche compiled of
    Nothing ->
      pure ()
    Just avalanche ->
      -- TODO: finer-grained timing
      whenSet FlagSeaEval $ timeSectionWith "Compile & Evaluate Sea" $ do
        context <- getEvalContext
        cflags <- getCFlags
        result <- liftIO . runEitherT $ Compiler.seaEvalWith cflags context facts (compiledSource compiled) avalanche
        case result of
          Left err ->
            putSection "C evaluation error" $ Compiler.renderCompilerSeaError err
          Right x ->
            putSection "C evaluation" $ ppResults x

readStriped :: (MonadResource m, MonadCatch m) => FilePath -> Stream (Of Zebra.Table) (EitherT QueryError m) ()
readStriped path =
  hoist (firstJoin QueryBinaryStripedDecodeError) .
    Binary.decodeStriped .
  hoist (firstT QueryIOError) $
    ByteStream.readFile path

readStripedN :: (MonadResource m, MonadCatch m) => Int -> FilePath -> Stream (Of Zebra.Table) (EitherT QueryError m) ()
readStripedN n path =
  hoist (firstJoin QueryZebraStripedError) .
    Zebra.rechunk n $ readStriped path

readZebraRows :: MonadIO m => Int -> FilePath -> EitherT QueryError m (Maybe Zebra.Table)
readZebraRows limit path =
  hoist (liftIO . runResourceT) $
    Stream.head_ (readStripedN limit path)

fromOutput :: OutputId -> Runtime.Output key -> Either QueryError (Boxed.Vector (key, Pretty.Doc))
fromOutput oid output = do
  column <- maybeToRight (QueryOutputMissing oid) . Map.lookup oid $ Runtime.outputColumns output
  values <- first QueryStripedError $ Striped.toLogical column
  pvalues <- first QueryLogicalError $ traverse (Logical.prettyValue 0 $ Striped.schema column) values
  pure $
    Boxed.zip (Runtime.outputKey output) pvalues

evaluateZebra :: CompiledQuery -> FilePath -> EitherT QueryError Repl ()
evaluateZebra compiled path = do
  case compiledAvalanche compiled of
    Nothing ->
      pure ()
    Just avalanche -> do
      dictionary <- gets stateDictionary

      iid <-
        hoistEither . first QueryCompilerError $
          Compiler.sourceInputId (compiledSource compiled) dictionary
      let
        context0 =
          Runtime.AvalancheContext "Icicle.Repl.Query.evaluateZebra" $
          Map.fromList [(iid, avalanche :| [])]

      timeCompile <- timeSection "Compile Sea"

      context <-
        timeSectionWith "Compile Sea Avalanche->Sea" $
        hoistEither . first QueryRuntimeError $
          Runtime.compileAvalanche context0

      cflags <- lift getCFlags

      runtime <-
        timeSectionWith "Compile Sea Sea->object" $
        firstT QueryRuntimeError $
          Runtime.compileSeaWith cflags Sea.SkipJetskiCache context

      timeCompile

      whenSet FlagSeaRuntime $
        putSection "C runtime" (ppShow runtime)

      whenSet FlagSeaEval $ timeSectionWith "Evaluate Zebra" $ do
        mapSize <- Runtime.MaximumMapSize . fromIntegral <$> gets stateMaxMapSize
        time <- Runtime.SnapshotTime . Runtime.QueryTime . Runtime.Time64 .
          packedOfTime . exclusiveSnapshotTime <$> gets stateSnapshotDate

        limit <- gets stateLimit
        minput0 <- timeSectionWith "Evaluate Zebra Open" $ readZebraRows limit path

        case minput0 of
          Nothing ->
            pure ()

          Just input0 -> do
            let
              inputSchemas =
                fmap (Runtime.clusterInputSchema . Sea.clusterAnnotation) $ Runtime.runtimeClusters runtime

            zschema <-
              timeSectionWith "Evaluate Zebra Schema" $
              hoistEither . first QueryRuntimeZebraSchemaError $
                Runtime.encodeInputSchemas inputSchemas

            input1 <-
              timeSectionWith "Evaluate Zebra Transmute" $
              hoistEither . first QueryZebraStripedError $
                Zebra.transmute zschema (Runtime.shiftTable input0)

            input <-
              timeSectionWith "Evaluate Zebra Decode" $
              hoistEither . first QueryRuntimeZebraStripedError $
                Runtime.decodeInput input1

            output0 <-
              timeSectionWith "Evaluate Zebra Snapshot" $
              firstT QueryRuntimeError . hoist liftIO $
                Runtime.snapshotBlock runtime mapSize time input

            output1 <- 
              timeSectionWith "Evaluate Zebra Output" $
              hoistEither $ fromOutput (compiledOutputId compiled) output0

            let
              output =
                Pretty.vsep . with output1 $ \(Runtime.SnapshotKey (Runtime.EntityKey _ (Runtime.EntityId k)), v) ->
                  Pretty.annotate Pretty.AnnBinding (Pretty.pretty (Char8.unpack k)) <>
                  Pretty.prettyPunctuation "|" <>
                  v

            putSection "C evaluation" output

evaluateQuery :: String -> Repl ()
evaluateQuery query =
  runEitherRepl $ do
    compiled <- timeSectionWith "Compile Query" $ compileQuery query

    renderCompiledSea compiled

    input <- gets stateInput
    case input of
      InputNone -> do
        lift $ evaluateCore compiled []
        lift $ evaluateAvalanche compiled []
        lift $ evaluateSea compiled []

      InputPsv path -> do
        dictionary <- gets stateDictionary
        facts <- readPsvFacts dictionary path
        lift $ evaluateCore compiled facts
        lift $ evaluateAvalanche compiled facts
        lift $ evaluateSea compiled facts

      InputZebra path -> do
        evaluateZebra compiled path

