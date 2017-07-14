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
import           Control.Monad.Morph (hoist, lift)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Builder (Builder)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time (getCurrentTime, diffUTCTime)

import           Icicle.Data.Time
import           Icicle.Runtime.Data (MaximumMapSize(..), SnapshotTime(..), SnapshotKey(..))
import qualified Icicle.Runtime.Data as Runtime
import           Icicle.Runtime.Evaluator (Runtime, RuntimeError)
import qualified Icicle.Runtime.Evaluator as Runtime
import qualified Icicle.Runtime.Serial.Psv as SerialPsv
import qualified Icicle.Runtime.Serial.Zebra as SerialZebra
import qualified Icicle.Sea.Data as Sea

import           P

import qualified Prelude as Savage

import           System.IO (IO, FilePath, putStrLn)
import           System.IO.Error (IOError)

import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either (EitherT, firstJoin, hoistEither)

import qualified Zebra.Serial.Binary as Zebra
import qualified Zebra.Table.Striped as Zebra
import qualified Zebra.X.ByteStream as ByteStream
import           Zebra.X.Stream (Stream, Of)
import qualified Zebra.X.Stream as Stream


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
  Runtime.SnapshotTime . Runtime.Time64 . packedOfTime . exclusiveSnapshotTime

encodeZebraSnapshot :: Runtime.Output SnapshotKey -> Either QueryError Zebra.Table
encodeZebraSnapshot =
  first QueryRuntimeZebraStripedError . SerialZebra.encodeSnapshotOutput

encodePsvSnapshot :: Runtime.Output SnapshotKey -> Either QueryError Builder
encodePsvSnapshot =
  first QueryRuntimeSerialPsvDataError . SerialPsv.encodeSnapshotOutput

encodePsvSchema :: Runtime -> Either QueryError ByteString
encodePsvSchema runtime = do
  schema0 <- first QueryRuntimeError $ Runtime.runtimeOutputSchema runtime
  schema <- first QueryRuntimeSerialPsvSchemaError $ SerialPsv.encodePsvSchema schema0
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

snapshot ::
     forall a b m r.
     MonadBaseControl IO m
  => (a -> Either QueryError Runtime.Input)
  -> (Runtime.Output SnapshotKey -> Either QueryError b)
  -> Runtime
  -> MaximumMapSize
  -> SnapshotTime
  -> Stream (Of a) m r
  -> Stream (Of b) (EitherT QueryError m) r
snapshot decode encode runtime maxsize stime =
  let
    run :: a -> EitherT QueryError IO b
    run input0 = do
      input <- hoistEither $ decode input0
      output <- firstT QueryRuntimeError $ Runtime.snapshotBlock runtime maxsize stime input
      hoistEither $ encode output
  in
    mapConcurrentlyN 16 (hoist liftBase . run) . hoist lift

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
                firstJoin id . firstJoin id .
                  writeZebra qoutput .
                  snapshot decode encodeZebraSnapshot runtime (queryMaxmumMapSize query) (fromSnapshotDate date) $
                  readZebra qinput

              QueryOutputPsv qoutput soutput -> do
                writePsvSchema qoutput soutput =<< hoistEither (encodePsvSchema runtime)
                firstJoin id . firstJoin id .
                  writePsv qoutput .
                  snapshot decode encodePsvSnapshot runtime (queryMaxmumMapSize query) (fromSnapshotDate date) $
                  readZebra qinput

          QueryChord x ->
            Savage.error $ "chord " <> show x

icicleQuery :: Query -> EitherT QueryError IO ()
icicleQuery query = do
  startTime <- liftIO getCurrentTime
  liftIO $
    putStrLn "icicle: starting compilation: C -> x86_64"

  runtime <- compile query

  endCompileTime <- liftIO getCurrentTime

  let
    compileSeconds =
      realToFrac (endCompileTime `diffUTCTime` startTime) :: Double

  liftIO $ do
    printf "icicle: compilation time = %.2fs\n" compileSeconds
    putStrLn "icicle: starting query"

  execute query runtime

  endTime <- liftIO getCurrentTime

  let
    querySeconds =
      realToFrac (endTime `diffUTCTime` endCompileTime) :: Double

  liftIO $
    printf "icicle: query time = %.2fs\n" querySeconds

-- FIXME add back these statistics:
--  liftIO (printf "icicle: query time      = %.2fs\n" secs)
--  liftIO (printf "icicle: total entities  = %d\n" entities)
--  liftIO (printf "icicle: total facts     = %d\n" facts)
--  liftIO (printf "icicle: fact throughput = %.0f facts/s\n" fps)
--  liftIO (printf "icicle: byte throughput = %.2f MiB/s\n" mbps)
