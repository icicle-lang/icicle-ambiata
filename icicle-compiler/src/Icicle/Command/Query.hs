{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Icicle.Command.Query (
    Query(..)

  , DictionarySea(..)

  , QueryInput(..)
  , InputZebra(..)

  , QueryOutput(..)
  , OutputZebra(..)
  , OutputPsv(..)
  , OutputPsvSchema(..)

  , QueryScope(..)

  , MaximumMapSize(..)

  , icicleQuery

  , QueryError(..)
  , renderQueryError
  ) where

import qualified Control.Concurrent.Async.Lifted as Async
import           Control.Monad.Base (liftBase)
import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (hoist, lift, squash)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Builder (Builder)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Icicle.Command.Timer
import           Icicle.Data.Time
import           Icicle.Runtime.Data (MaximumMapSize(..), SnapshotKey(..), ChordKey(..), ChordDescriptor(..))
import qualified Icicle.Runtime.Data as Runtime
import           Icicle.Runtime.Evaluator (Runtime, RuntimeError)
import qualified Icicle.Runtime.Evaluator as Runtime
import qualified Icicle.Runtime.Serial.Psv as SerialPsv
import qualified Icicle.Runtime.Serial.Zebra as SerialZebra
import qualified Icicle.Sea.Data as Sea

import           P

import qualified Piano
import qualified Piano.Data as Piano

import           System.IO (IO, FilePath)
import           System.IO.Error (IOError)

import           Viking (Stream, Of)
import qualified Viking.ByteStream as ByteStream
import qualified Viking.Stream as Stream

import           X.Control.Monad.Trans.Either (EitherT, firstJoin, hoistEither)

import qualified Zebra.Serial.Binary as Zebra
import qualified Zebra.Table.Striped as Zebra


data Query =
  Query {
      queryDictionary :: !DictionarySea
    , queryInput :: !QueryInput
    , queryOutput :: !QueryOutput
    , queryScope :: !QueryScope
    , queryMaxmumMapSize :: !MaximumMapSize
    } deriving (Eq, Ord, Show)

data QueryInput =
    QueryInputZebra !InputZebra
    deriving (Eq, Ord, Show)

data QueryOutput =
    QueryOutputZebra !OutputZebra
  | QueryOutputPsv !OutputPsv !(Maybe OutputPsvSchema)
    deriving (Eq, Ord, Show)

newtype DictionarySea =
  DictionarySea {
      unDictionarySea :: FilePath
    } deriving (Eq, Ord, Show)

newtype InputZebra =
  InputZebra {
      unInputZebra :: FilePath
    } deriving (Eq, Ord, Show)

newtype OutputZebra =
  OutputZebra {
      unOutputZebra :: FilePath
    } deriving (Eq, Ord, Show)

newtype OutputPsv =
  OutputPsv {
      unOutputPsv :: FilePath
    } deriving (Eq, Ord, Show)

newtype OutputPsvSchema =
  OutputPsvSchema {
      unOutputPsvSchema :: FilePath
    } deriving (Eq, Ord, Show)

data QueryScope =
    QuerySnapshot !Date
  | QueryChord !FilePath
    deriving (Eq, Ord, Show)

data QueryError =
    QueryRuntimeError !RuntimeError
  | QueryIOError !IOError
  | QueryPianoError !Piano.ParserError
  | QueryZebraBinaryStripedDecodeError !Zebra.BinaryStripedDecodeError
  | QueryZebraBinaryStripedEncodeError !Zebra.BinaryStripedEncodeError
  | QueryZebraStripedError !Zebra.StripedError
  | QueryRuntimeZebraSchemaError !SerialZebra.ZebraSchemaError
  | QueryRuntimeZebraStripedError !SerialZebra.ZebraStripedError
  | QueryRuntimeSerialPsvDataError !SerialPsv.SerialPsvDataError
  | QueryRuntimeSerialPsvSchemaError !SerialPsv.SerialPsvSchemaError
    deriving (Eq, Show)

renderQueryError :: QueryError -> Text
renderQueryError = \case
  QueryRuntimeError x ->
    Runtime.renderRuntimeError x
  QueryIOError x ->
    "IO error: " <> Text.pack (show x)
  QueryPianoError x ->
    Piano.renderParserError x
  QueryZebraBinaryStripedDecodeError x ->
    Zebra.renderBinaryStripedDecodeError x
  QueryZebraBinaryStripedEncodeError x ->
    Zebra.renderBinaryStripedEncodeError x
  QueryZebraStripedError x ->
    Zebra.renderStripedError x
  QueryRuntimeZebraSchemaError x ->
    SerialZebra.renderZebraSchemaError x
  QueryRuntimeZebraStripedError x ->
    SerialZebra.renderZebraStripedError x
  QueryRuntimeSerialPsvDataError x ->
    SerialPsv.renderSerialPsvDataError x
  QueryRuntimeSerialPsvSchemaError x ->
    SerialPsv.renderSerialPsvSchemaError x

readDictionary :: DictionarySea -> IO Runtime.SeaContext
readDictionary (DictionarySea path) =
  Runtime.SeaContext . Text.decodeUtf8 <$> ByteString.readFile path

readZebra :: (MonadResource m, MonadCatch m) => InputZebra -> Stream (Of Zebra.Table) (EitherT QueryError m) ()
readZebra (InputZebra path) =
  hoist (firstJoin QueryZebraBinaryStripedDecodeError) .
    Zebra.decodeStriped .
  hoist (firstT QueryIOError) $
    ByteStream.readFile path

writeZebra :: (MonadResource m, MonadCatch m) => OutputZebra -> Stream (Of Zebra.Table) m r -> EitherT QueryError m r
writeZebra (OutputZebra path) xs =
  let
    bss =
      hoist (firstT QueryZebraBinaryStripedEncodeError) $
        Zebra.encodeStriped xs
  in
    firstJoin QueryIOError $
      ByteStream.writeFile path bss

writePsv :: (MonadResource m, MonadCatch m) => OutputPsv -> Stream (Of Builder) m r -> EitherT QueryError m r
writePsv (OutputPsv path) builders =
  firstT QueryIOError $
    ByteStream.writeFile path (ByteStream.fromBuilders builders)

writePsvSchema :: MonadIO m => OutputPsv -> Maybe OutputPsvSchema -> ByteString -> m ()
writePsvSchema (OutputPsv opath) mspath schema =
  case mspath of
    Nothing ->
      liftIO $ ByteString.writeFile (opath <> ".schema.json") schema
    Just (OutputPsvSchema spath) ->
      liftIO $ ByteString.writeFile spath schema

decodeZebra :: Runtime -> Either QueryError (Zebra.Table -> Either QueryError Runtime.Input)
decodeZebra runtime = do
  let
    inputSchemas =
      fmap (Runtime.clusterInputSchema . Sea.clusterAnnotation) $
      Runtime.runtimeClusters runtime

  zschema <-
    first QueryRuntimeZebraSchemaError $
      SerialZebra.encodeInputSchemas inputSchemas

  pure $ \zebra0 -> do
    zebra1 <- first QueryZebraStripedError . Zebra.transmute zschema $ SerialZebra.shiftTable zebra0
    first QueryRuntimeZebraStripedError $ SerialZebra.decodeInput zebra1

fromSnapshotDate :: Date -> Runtime.SnapshotTime
fromSnapshotDate =
  Runtime.SnapshotTime . Runtime.QueryTime . Runtime.Time64 . packedOfTime . exclusiveSnapshotTime

encodeZebraSnapshot :: Runtime.Output SnapshotKey -> Either QueryError Zebra.Table
encodeZebraSnapshot =
  first QueryRuntimeZebraStripedError . SerialZebra.encodeSnapshotOutput

encodeZebraChord :: Runtime.Output ChordKey -> Either QueryError Zebra.Table
encodeZebraChord =
  first QueryRuntimeZebraStripedError . SerialZebra.encodeChordOutput

encodePsvSnapshot :: Runtime.Output SnapshotKey -> Either QueryError Builder
encodePsvSnapshot =
  first QueryRuntimeSerialPsvDataError . SerialPsv.encodeSnapshotOutput

encodePsvChord :: Runtime.Output ChordKey -> Either QueryError Builder
encodePsvChord =
  first QueryRuntimeSerialPsvDataError . SerialPsv.encodeChordOutput

encodePsvSnapshotSchema :: Runtime -> Either QueryError ByteString
encodePsvSnapshotSchema runtime = do
  schema0 <- first QueryRuntimeError $ Runtime.runtimeOutputSchema runtime
  schema <- first QueryRuntimeSerialPsvSchemaError $ SerialPsv.encodePsvSnapshotSchema schema0
  pure . Text.encodeUtf8 $ SerialPsv.renderPrettyPsvSchema schema

encodePsvChordSchema :: Runtime -> Either QueryError ByteString
encodePsvChordSchema runtime = do
  schema0 <- first QueryRuntimeError $ Runtime.runtimeOutputSchema runtime
  schema <- first QueryRuntimeSerialPsvSchemaError $ SerialPsv.encodePsvChordSchema schema0
  pure . Text.encodeUtf8 $ SerialPsv.renderPrettyPsvSchema schema

compile :: Query -> EitherT QueryError IO Runtime
compile query = do
  sea <- liftIO . readDictionary $ queryDictionary query
  firstT QueryRuntimeError $ Runtime.compileSea Runtime.UseJetskiCache sea

mapConcurrentlyN :: MonadBaseControl IO m => Int -> (a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapConcurrentlyN n f =
  Stream.concat .
  Stream.mapM (Async.mapConcurrently f) .
  Stream.mapped Stream.toList .
  Stream.chunksOf n

queryBlock ::
     forall a b k m r.
     MonadBaseControl IO m
  => (a -> Either QueryError Runtime.Input)
  -> (Runtime.Output k -> Either QueryError b)
  -> (Runtime.Input -> EitherT RuntimeError IO (Runtime.Output k))
  -> Stream (Of a) m r
  -> Stream (Of b) (EitherT QueryError m) r
queryBlock decode encode query =
  let
    run :: a -> EitherT QueryError IO b
    run input0 = do
      input <- hoistEither $ decode input0
      output <- firstT QueryRuntimeError $ query input
      hoistEither $ encode output
  in
    mapConcurrentlyN 16 (hoist liftBase . run) . hoist lift

fromPianoTime :: Piano.EndTime -> Runtime.QueryTime
fromPianoTime =
  Runtime.QueryTime . Runtime.fromIvorySeconds . Piano.unEndTime

fromPianoLabel :: Piano.Label -> Runtime.Label
fromPianoLabel (Piano.Label time tag) =
  Runtime.Label (fromPianoTime time) tag

readChordDescriptor :: FilePath -> EitherT QueryError IO ChordDescriptor
readChordDescriptor path = do
  bs <- liftIO $ ByteString.readFile path
  piano <- hoistEither . bimap QueryPianoError Piano.pianoEntities $ Piano.parsePiano bs

  pure . ChordDescriptor $ \(Runtime.EntityId entity) ->
    Set.mapMonotonic fromPianoLabel . fromMaybe Set.empty $
      Map.lookup (Piano.mkEntity entity) piano

execute :: Query -> Runtime -> EitherT QueryError IO ()
execute query runtime =
  hoist runResourceT $ do
    case queryInput query of
      QueryInputZebra qinput -> do
        decode <- hoistEither $ decodeZebra runtime

        case queryScope query of
          QuerySnapshot date ->
            case queryOutput query of
              QueryOutputZebra qoutput ->
                squash . squash .
                  writeZebra qoutput .
                  queryBlock decode encodeZebraSnapshot
                    (Runtime.snapshotBlock runtime (queryMaxmumMapSize query) (fromSnapshotDate date)) $
                  readZebra qinput

              QueryOutputPsv qoutput soutput -> do
                writePsvSchema qoutput soutput =<< hoistEither (encodePsvSnapshotSchema runtime)
                squash . squash .
                  writePsv qoutput .
                  queryBlock decode encodePsvSnapshot
                    (Runtime.snapshotBlock runtime (queryMaxmumMapSize query) (fromSnapshotDate date)) $
                  readZebra qinput

          QueryChord descriptorPath -> do
            descriptor <- hoist lift $ readChordDescriptor descriptorPath
            case queryOutput query of
              QueryOutputZebra qoutput ->
                squash . squash .
                  writeZebra qoutput .
                  queryBlock decode encodeZebraChord
                    (Runtime.chordBlock runtime (queryMaxmumMapSize query) descriptor) $
                  readZebra qinput

              QueryOutputPsv qoutput soutput -> do
                writePsvSchema qoutput soutput =<< hoistEither (encodePsvChordSchema runtime)
                squash . squash .
                  writePsv qoutput .
                  queryBlock decode encodePsvChord
                    (Runtime.chordBlock runtime (queryMaxmumMapSize query) descriptor) $
                  readZebra qinput

icicleQuery :: Query -> EitherT QueryError IO ()
icicleQuery query = do
  finishCompile <- startTimer "Compiling C -> x86_64"
  runtime <- compile query
  finishCompile

  finishQuery <- startTimer "Executing Query"
  execute query runtime
  finishQuery

-- FIXME add back these statistics:
--  liftIO (printf "icicle: query time      = %.2fs\n" secs)
--  liftIO (printf "icicle: total entities  = %d\n" entities)
--  liftIO (printf "icicle: total facts     = %d\n" facts)
--  liftIO (printf "icicle: fact throughput = %.0f facts/s\n" fps)
--  liftIO (printf "icicle: byte throughput = %.2f MiB/s\n" mbps)
