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
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Boxed
import qualified Data.Vector.Storable as Storable

import           Icicle.Command.Timer
import           Icicle.Data.Name
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

import           Text.PrettyPrint.Boxes (Box)
import qualified Text.PrettyPrint.Boxes as Box
import           Text.Printf (printf)

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

data QuerySummary =
  QuerySummary {
      summaryInputEntityCount :: !Int
    , summaryInputFactCounts :: !(Map InputId Int)
    , summaryOutputRowCount :: !Int
    } deriving (Eq, Ord, Show)

summaryInputFactCount :: QuerySummary -> Int
summaryInputFactCount =
  sum . summaryInputFactCounts

instance Monoid QuerySummary where
  mempty =
    QuerySummary 0 Map.empty 0
  mappend x y =
    QuerySummary {
        summaryInputEntityCount =
          summaryInputEntityCount x + summaryInputEntityCount y
      , summaryInputFactCounts =
          Map.unionWith (+) (summaryInputFactCounts x) (summaryInputFactCounts y)
      , summaryOutputRowCount =
          summaryOutputRowCount x + summaryOutputRowCount y
      }

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
      hoist (firstJoin QueryZebraBinaryStripedEncodeError) .
        Zebra.encodeStriped .
      hoist (firstT QueryZebraStripedError) $
        Zebra.rechunk 1024 xs
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

mkSummary :: Runtime.Input -> Runtime.Output k -> QuerySummary
mkSummary input output =
  QuerySummary {
      summaryInputEntityCount =
        Boxed.length (Runtime.inputKey input)

    , summaryInputFactCounts =
        Map.filter (/= 0) $
          fmap (Storable.length . Runtime.inputTime) (Runtime.inputColumns input)

    , summaryOutputRowCount =
        Boxed.length (Runtime.outputKey output)
    }

queryBlock ::
     forall a b k m r.
     MonadBaseControl IO m
  => IORef QuerySummary
  -> (a -> Either QueryError Runtime.Input)
  -> (Runtime.Output k -> Either QueryError b)
  -> (Runtime.Input -> EitherT RuntimeError IO (Runtime.Output k))
  -> Stream (Of a) m r
  -> Stream (Of b) (EitherT QueryError m) r
queryBlock ref decode encode query =
  let
    run :: a -> EitherT QueryError IO b
    run input0 = do
      input <- hoistEither $ decode input0
      output <- firstT QueryRuntimeError $ query input

      liftIO . IORef.atomicModifyIORef' ref $ \x ->
        (x <> mkSummary input output, ())

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

execute :: Query -> Runtime -> EitherT QueryError IO QuerySummary
execute query runtime = do
  ref <- liftIO $ IORef.newIORef mempty
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
                  queryBlock ref decode encodeZebraSnapshot
                    (Runtime.snapshotBlock runtime (queryMaxmumMapSize query) (fromSnapshotDate date)) $
                  readZebra qinput

              QueryOutputPsv qoutput soutput -> do
                writePsvSchema qoutput soutput =<< hoistEither (encodePsvSnapshotSchema runtime)
                squash . squash .
                  writePsv qoutput .
                  queryBlock ref decode encodePsvSnapshot
                    (Runtime.snapshotBlock runtime (queryMaxmumMapSize query) (fromSnapshotDate date)) $
                  readZebra qinput

          QueryChord descriptorPath -> do
            descriptor <- hoist lift $ readChordDescriptor descriptorPath
            case queryOutput query of
              QueryOutputZebra qoutput ->
                squash . squash .
                  writeZebra qoutput .
                  queryBlock ref decode encodeZebraChord
                    (Runtime.chordBlock runtime (queryMaxmumMapSize query) descriptor) $
                  readZebra qinput

              QueryOutputPsv qoutput soutput -> do
                writePsvSchema qoutput soutput =<< hoistEither (encodePsvChordSchema runtime)
                squash . squash .
                  writePsv qoutput .
                  queryBlock ref decode encodePsvChord
                    (Runtime.chordBlock runtime (queryMaxmumMapSize query) descriptor) $
                  readZebra qinput
  liftIO $ IORef.readIORef ref

boxRows :: [(Text, Text)] -> Box
boxRows rows =
  let
    keys =
      Box.vcat Box.left $ fmap (Box.text . Text.unpack . fst) rows

    preEq x =
      if null x then
        x
      else
        "= " <> x

    vals =
      Box.vcat Box.left $ fmap (Box.text . preEq . Text.unpack . snd) rows
  in
    Box.hsep 1 Box.top [
        keys
      , vals
      ]

renderSummary :: QuerySummary -> TimerDuration -> Text
renderSummary summary duration =
  let
    tr =
      (,)

    spacer =
      tr "" ""

    header x =
      tr x ""

    ruler x =
      tr (Text.replicate (Text.length x) "-") ""

    fcount (k, v) =
      tr (renderInputId k) (Text.pack $ show v <> " facts")
  in
    Text.pack . Box.render . boxRows $ [
          header "Facts"
        , ruler "Facts"

        ] <> fmap fcount (Map.toList $ summaryInputFactCounts summary) <> [
          tr "total" . Text.pack $
            show (summaryInputFactCount summary) <> " facts"

        , tr "throughput" . Text.pack $
            printf "%.0f facts/second" $
              fromIntegral (summaryInputFactCount summary) / timerDurationSeconds duration

        , spacer
        , header "Entities"
        , ruler "Entities"

        , tr "facts" . Text.pack $
            printf "%.0f facts/entity" $
              fromIntegral (summaryInputFactCount summary) / (fromIntegral (summaryInputEntityCount summary) :: Double)
        , tr "total" . Text.pack $
            show (summaryInputEntityCount summary) <> " entities"
        , tr "throughput" . Text.pack $
            printf "%.0f entities/second" $
              fromIntegral (summaryInputEntityCount summary) / timerDurationSeconds duration

        , spacer
        , header "Output"
        , ruler "Output"

        , tr "total" . Text.pack $
            show (summaryOutputRowCount summary) <> " rows"
        , tr "throughput" . Text.pack $
            printf "%.0f rows/second" $
              fromIntegral (summaryOutputRowCount summary) / timerDurationSeconds duration
        ]

icicleQuery :: Query -> EitherT QueryError IO ()
icicleQuery query = do
  finishCompile <- startTimer_ "Compiling C -> x86_64"
  runtime <- compile query
  finishCompile

  finishQuery <- startTimer "Executing Query"
  summary <- execute query runtime
  duration <- finishQuery

  liftIO . Text.putStr . Text.unlines . fmap ("icicle: " <>) . ("" :) . Text.lines $
    renderSummary summary duration
