{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import           BuildInfo_icicle
import           DependencyInfo_icicle

import           Control.Monad.IO.Class (MonadIO, liftIO)

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
    IcicleCompile FilePath FilePath InputFormat OutputFormat (Scope ())
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
    <*> pZebraChunkSize
    <*> pZebraAllocLimit

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
  fmap (OutputFile OutputSparsePsv) $
    strOption (long "output-sparse-psv" <> metavar "OUTPUT_PSV")

pOutputSparseFormat :: Parser OutputFormat
pOutputSparseFormat =
  flag' OutputSparsePsv (long "output-sparse-psv")

pOutputDense :: Parser OutputFile
pOutputDense =
  fmap (OutputFile OutputDensePsv) $
    strOption (long "output-dense-psv" <> metavar "OUTPUT_PSV")

pOutputDenseFormat :: Parser OutputFormat
pOutputDenseFormat =
  flag' OutputDensePsv (long "output-dense-psv")

pOutputCode :: Parser FilePath
pOutputCode =
  strOption (long "output-code" <> metavar "DICTIONARY_C")

pSnapshot :: Parser (Scope a)
pSnapshot =
  fmap ScopeSnapshot . flip option (long "snapshot" <> metavar "SNAPSHOT_DATE") $
    readerAsk >>= \s -> case timeOfText (T.pack s) of
      Just t  -> return t
      Nothing -> readerError "cannot parse snapshot date"

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
    readerAsk >>= \s -> case readMaybe s of
      Just i  -> return i
      Nothing -> readerError "--facts-limit NUMBER"

pDrop :: Parser (Maybe FilePath)
pDrop =
  optional $ strOption (long "drop" <> metavar "DROP_FILE")

pFlagDrop :: Parser FlagUseDrop
pFlagDrop =
  flag FlagUseDropFile FlagNoUseDropFile $
    long "drop-to-output" <>
    help "write partial results to dropped-X.txt or normal output"

pZebraChunkSize :: Parser ZebraChunkSize
pZebraChunkSize =
  flip option (long "zebra-chunk-size" <> value defaultZebraChunkSize) $
    readerAsk >>= \s -> case readMaybe s of
      Just i  -> return (ZebraChunkSize i)
      Nothing -> readerError "--chunk-size NUMBER_BYTES"

pZebraAllocLimit :: Parser ZebraAllocLimit
pZebraAllocLimit =
  flip option (long "zebra-alloc-limit" <> value defaultZebraAllocLimit) $
    readerAsk >>= \s -> case readMaybe s of
      Just i  -> return (ZebraAllocLimit i)
      Nothing -> readerError "--zebra-alloc-limit NUMBER_BYTES"

------------------------------------------------------------------------

runCommand :: IcicleCommand -> EitherT IcicleError IO ()
runCommand = \case
  IcicleCompile tomlPath opath iformat oformat scope -> do
    start <- liftIO getCurrentTime
    liftIO $ putStrLn "icicle: starting compilation"

    code <- compileDictionary tomlPath iformat oformat scope
    writeUtf8 opath code

    end <- liftIO getCurrentTime
    let secs = realToFrac (end `diffUTCTime` start) :: Double

    liftIO (printf "icicle: compilation time = %.2fs\n" secs)

  IcicleQuery q -> do
    liftIO . putStrLn $ "icicle: facts_limit = " <> show (optFactsLimit q)
    liftIO $ putStrLn "icicle: starting compilation"
    case inputFormat $ optInput q of
      InputSparsePsv
        -> bracketEitherT' (createPsvQuery q) releaseQuery
         $ runQuery runPsvQuery (optOutputCode q)
      InputDensePsv
        -> bracketEitherT' (createPsvQuery q) releaseQuery
         $ runQuery runPsvQuery (optOutputCode q)
      InputZebra
        -> bracketEitherT' (createZebraQuery q) releaseQuery
         $ runQuery runZebraQuery (optOutputCode q)

runQuery ::
      MonadIO m
  => (Query a -> m QueryStatistics)
  -> Maybe FilePath
  -> Query a
  -> m ()
runQuery f msrc query = do
  let compSecs = realToFrac (queryCompilationTime query) :: Double
  liftIO (printf "icicle: compilation time = %.2fs\n" compSecs)

  liftIO (maybe (return ()) (flip writeUtf8 (querySource query)) msrc)

  liftIO (putStrLn "icicle: starting query")

  stats <- f query

  let entities = queryEntities stats
      facts    = queryFacts stats
      bytes    = queryBytes stats
      secs     = realToFrac (queryTime stats) :: Double
      mbps     = (fromIntegral bytes / secs) / (1024 * 1024)
      mfps     = (fromIntegral facts / secs) / (1000 * 1000)

  liftIO (printf "icicle: query time      = %.2fs\n"                secs)
  liftIO (printf "icicle: total entities  = %d\n"                   entities)
  liftIO (printf "icicle: total facts     = %d\n"                   facts)
  liftIO (printf "icicle: fact throughput = %.2f million facts/s\n" mfps)
  liftIO (printf "icicle: byte throughput = %.2f MB/s\n"            mbps)

writeUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeUtf8 path txt =
  liftIO . B.writeFile path $ T.encodeUtf8 txt
