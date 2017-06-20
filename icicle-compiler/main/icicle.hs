{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import           BuildInfo_icicle
import           DependencyInfo_icicle

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Morph

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (getCurrentTime, diffUTCTime)

import           Icicle.Command
import           Icicle.Data.Time (timeOfText)
import           Icicle.Sea.Eval

import           P

import           System.IO (IO, FilePath, BufferMode(..))
import           System.IO (putStrLn, hSetBuffering, stdout, stderr)

import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either
import           X.Control.Monad.Trans.Either.Exit
import           X.Options.Applicative


data IcicleCommand =
    IcicleCompile FilePath FilePath InputFormat OutputFormat (Scope ()) CompilerFlags
  | IcicleQuery QueryOptions
    deriving (Eq, Ord, Show)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "icicle" buildInfoVersion dependencyInfo parser (orDie renderIcicleError . runCommand)

parser :: Parser IcicleCommand
parser =
  subparser . mconcat $ commands

commands :: [Mod CommandFields IcicleCommand]
commands = [
    command'
      "compile"
      "Compile a dictionary to its C intermediate form."
      pCompile
  , command'
      "query"
      "Run an icicle query over some data."
      pQuery
  ]

pCompile :: Parser IcicleCommand
pCompile =
  IcicleCompile
    <$> pDictionaryTomlPath
    <*> pOutputCode
    <*> pInputFormat
    <*> pOutputFormat
    <*> (pSnapshot <|> pChord)
    <*> pCompilerFlags

pQuery :: Parser IcicleCommand
pQuery =
  fmap IcicleQuery $
  QueryOptions
    <$> pDictionaryFile
    <*> pInputFile
    <*> pOutputFile
    <*> optional pOutputCode
    <*> (pSnapshot <|> pChordPath)
    <*> pLimit
    <*> pDrop
    <*> pFlagDrop
    <*> pZebraChunkFactCount
    <*> pZebraAllocLimitGB
    <*> pMaxMapSize

pDictionaryFile :: Parser DictionaryFile
pDictionaryFile =
  pDictionaryToml <|> pDictionaryCode

pDictionaryToml :: Parser DictionaryFile
pDictionaryToml =
  fmap DictionaryToml pDictionaryTomlPath

pDictionaryTomlPath :: Parser FilePath
pDictionaryTomlPath =
  strOption (long "dictionary-toml" <> metavar "DICTIONARY_TOML")

pDictionaryCode :: Parser DictionaryFile
pDictionaryCode =
  fmap DictionaryCode $
    strOption (long "dictionary-code" <> metavar "DICTIONARY_C")

pInputFile :: Parser InputFile
pInputFile =
  pInputSparse <|> pInputDense <|> pInputZebra

pInputFormat :: Parser InputFormat
pInputFormat =
  pInputSparseFormat <|> pInputDenseFormat <|> pInputZebraFormat

pInputSparse :: Parser InputFile
pInputSparse =
  fmap (InputFile InputSparsePsv) $
    strOption (long "input-sparse-psv" <> metavar "INPUT_PSV")

pInputSparseFormat :: Parser InputFormat
pInputSparseFormat =
  flag' InputSparsePsv (long "input-sparse-psv")

pInputDense :: Parser InputFile
pInputDense =
  fmap (InputFile InputDensePsv) $
    strOption (long "input-dense-psv" <> metavar "INPUT_PSV")

pInputDenseFormat :: Parser InputFormat
pInputDenseFormat =
  flag' InputDensePsv (long "input-dense-psv")

pInputZebra :: Parser InputFile
pInputZebra =
  fmap (InputFile InputZebra) $
    strOption (long "input-zebra" <> metavar "INPUT_ZEBRA")

pInputZebraFormat :: Parser InputFormat
pInputZebraFormat =
  flag' InputZebra (long "input-zebra")

pOutputFile :: Parser OutputFile
pOutputFile =
  pOutputSparse <|> pOutputDense

pOutputFormat :: Parser OutputFormat
pOutputFormat =
  pOutputSparseFormat <|> pOutputDenseFormat

pOutputSparse :: Parser OutputFile
pOutputSparse =
  fmap (\path -> OutputFile OutputSparsePsv path Nothing) $
    strOption (long "output-sparse-psv" <> metavar "OUTPUT_PSV")

pOutputSparseFormat :: Parser OutputFormat
pOutputSparseFormat =
  flag' OutputSparsePsv (long "output-sparse-psv")

pOutputDense :: Parser OutputFile
pOutputDense =
  OutputFile OutputDensePsv
    <$> strOption (long "output-dense-psv" <> metavar "OUTPUT_PSV")
    <*> optional pOutputSchema

pOutputDenseFormat :: Parser OutputFormat
pOutputDenseFormat =
  flag' OutputDensePsv (long "output-dense-psv")

pOutputCode :: Parser FilePath
pOutputCode =
  strOption (long "output-code" <> metavar "DICTIONARY_C")

pOutputSchema :: Parser FilePath
pOutputSchema =
  strOption $
       long "output-schema"
    <> metavar "OUTPUT_PSV_SCHEMA_JSON"
    <> help
         "Location to write the output schema when using dense PSV output. (defaults to <output-path>.schema.json)"

pSnapshot :: Parser (Scope a)
pSnapshot =
  fmap ScopeSnapshot . flip option (long "snapshot" <> metavar "SNAPSHOT_DATE") $
    tryRead "cannot parse snapshot date" (timeOfText . T.pack) id

pChordPath :: Parser (Scope FilePath)
pChordPath =
  fmap ScopeChord $
    strOption (long "chord" <> metavar "CHORD_DESCRIPTOR")

pChord :: Parser (Scope ())
pChord =
  flag' (ScopeChord ()) (long "chord")

pLimit :: Parser Int
pLimit =
  flip option (long "facts-limit" <> value defaultPsvFactsLimit) $
    tryRead "--facts-limit NUMBER" readMaybe id

pDrop :: Parser (Maybe FilePath)
pDrop =
  optional $ strOption (long "drop" <> metavar "DROP_FILE")

pFlagDrop :: Parser FlagUseDrop
pFlagDrop =
  flag FlagUseDropFile FlagNoUseDropFile $
    long "drop-to-output" <>
    help "write partial results to dropped-X.txt or normal output"

pZebraChunkFactCount :: Parser ZebraChunkFactCount
pZebraChunkFactCount =
  flip option (long "zebra-chunk-fact-count" <> value defaultZebraChunkFactCount) $
    tryRead "--zebra-chunk-fact-count NUMBER_FACTS" readMaybe ZebraChunkFactCount

pZebraAllocLimitGB :: Parser ZebraAllocLimitGB
pZebraAllocLimitGB =
  flip option (long "zebra-alloc-limit-gb" <> value defaultZebraAllocLimitGB) $
    tryRead "--zebra-alloc-limit-gb NUMBER_GB" readMaybe ZebraAllocLimitGB

pMaxMapSize :: Parser Int
pMaxMapSize =
  flip option (long "max-map-size" <> value defaultZebraMaxMapSize) $
    tryRead "--max-map-size NUMBER_ELEMENTS" readMaybe id

pCompilerFlags :: Parser CompilerFlags
pCompilerFlags = CompilerFlags <$> maximumQueries
 where
  maximumQueries = flip option (long "fuse-maximum-per-kernel" <> value (compilerMaximumQueriesPerKernel defaultCompilerFlags)) $
    tryRead "--fuse-maximum-per-kernel NUMBER_QUERIES" readMaybe id

tryRead :: [Char] -> ([Char] -> Maybe a) -> (a -> b) -> ReadM b
tryRead err f g =
  readerAsk >>= \s -> case f s of
    Just i  -> return $ g i
    Nothing -> readerError err

------------------------------------------------------------------------

runCommand :: IcicleCommand -> EitherT IcicleError IO ()
runCommand = \case
  IcicleCompile tomlPath opath iformat oformat scope cflags -> do
    start <- liftIO getCurrentTime
    liftIO $ putStrLn "icicle: starting compilation"

    code <- compileDictionary tomlPath iformat oformat scope cflags
    writeUtf8 opath code

    end <- liftIO getCurrentTime
    let secs = realToFrac (end `diffUTCTime` start) :: Double

    liftIO (printf "icicle: compilation time = %.2fs\n" secs)

  IcicleQuery q -> do
    liftIO . putStrLn $ "icicle: facts_limit = " <> show (optFactsLimit q)
    liftIO $ putStrLn "icicle: starting compilation"
    case inputFormat $ optInput q of
      InputSparsePsv
        -> bracketEitherT'
             (createPsvQuery q)
             (hoist liftIO . releaseQuery)
             (hoist liftIO . runQuery runPsvQuery (optOutputCode q))
      InputDensePsv
        -> bracketEitherT'
             (createPsvQuery q)
             (hoist liftIO . releaseQuery)
             (hoist liftIO . runQuery runPsvQuery (optOutputCode q))
      InputZebra
        -> bracketEitherT'
             (createZebraQuery q)
             (hoist liftIO . releaseQuery)
             (hoist liftIO . runQuery runZebraQuery (optOutputCode q))

runQuery ::
      MonadIO m
  => (Query a -> m QueryStatistics)
  -> Maybe FilePath
  -> Query a
  -> m ()
runQuery f msrc query = do
  let compSecs = realToFrac (queryCompilationTime query) :: Double
  liftIO (printf "icicle: compilation time = %.2fs\n" compSecs)

  case msrc of
    Nothing ->
      pure ()
    Just src ->
      writeUtf8 src (querySource query)

  case sfOutputSchema (queryFleet query) of
    Nothing ->
      pure ()
    Just schema -> do
      let path = queryOutputSchemaPath query
      liftIO . putStrLn $ "icicle: writing output schema to " <> path
      writeUtf8 path (renderPrettyPsvSchema schema)

  liftIO (putStrLn "icicle: starting query")

  stats <- f query

  let entities = queryEntities stats
      facts    = queryFacts stats
      bytes    = queryBytes stats
      secs     = realToFrac (queryTime stats) :: Double
      mbps     = (fromIntegral bytes / secs) / (1024 * 1024)
      fps      = fromIntegral facts / secs

  liftIO (printf "icicle: query time      = %.2fs\n" secs)
  liftIO (printf "icicle: total entities  = %d\n" entities)
  liftIO (printf "icicle: total facts     = %d\n" facts)
  liftIO (printf "icicle: fact throughput = %.0f facts/s\n" fps)
  liftIO (printf "icicle: byte throughput = %.2f MiB/s\n" mbps)

writeUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeUtf8 path txt =
  liftIO . B.writeFile path $ T.encodeUtf8 txt
