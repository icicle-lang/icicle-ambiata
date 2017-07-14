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
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (getCurrentTime, diffUTCTime)

import           Icicle.Command
import           Icicle.Command.Compile
import           Icicle.Data.Time (timeOfText)
import           Icicle.Repl
import           Icicle.Sea.Eval

import           P

import           System.IO (IO, FilePath, BufferMode(..))
import           System.IO (putStrLn, hSetBuffering, stdout, stderr)

import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative
import           X.Options.Applicative (Parser, Mod, CommandFields)
import qualified X.Options.Applicative as Options


data IcicleCommand =
    IcicleRepl !ReplOptions
  | IcicleCompile !Compile
  | IcicleQuery QueryOptions
    deriving (Eq, Ord, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "icicle" buildInfoVersion dependencyInfo parser runCommand

parser :: Parser IcicleCommand
parser =
  subparser . mconcat $ commands

commands :: [Mod CommandFields IcicleCommand]
commands = [
    command'
      "repl"
      "Interactively evaluate icicle expressions."
      (IcicleRepl <$> pRepl)
  , command'
      "compile"
      "Compile a dictionary to its C intermediate form."
      (IcicleCompile <$> pCompile)
  , command'
      "query"
      "Run an icicle query over some data."
      pQuery
  ]

pRepl :: Parser ReplOptions
pRepl =
  ReplOptions
    <$> pUseDotfiles
    <*> many pReplCommand
    <*> many (pReplZebra <|> pReplPSV <|> pReplTOML)

pUseDotfiles :: Parser UseDotfiles
pUseDotfiles =
  flag UseDotfiles SkipDotfiles $
    long "skip-dotfiles" <>
    help "Don't load the .icicle file from $HOME or the current directory"

pReplZebra :: Parser FilePath
pReplZebra =
  argument str $
    metavar "INPUT_ZEBRA" <>
    help "Path to a Zebra binary file to load"

pReplPSV :: Parser FilePath
pReplPSV =
  argument str $
    metavar "INPUT_PSV" <>
    help "Path to a PSV file to load"

pReplTOML :: Parser FilePath
pReplTOML =
  argument str $
    metavar "DICTIONARY_TOML" <>
    help "Path to a TOML dictionary to load"

pCompile :: Parser Compile
pCompile =
  Compile
    <$> pure icicleFingerprint
    <*> pMaximumQueriesPerKernel
    <*> pInputDictionaryToml
    <*> pOutputDictionarySea

pMaximumQueriesPerKernel :: Parser MaximumQueriesPerKernel
pMaximumQueriesPerKernel =
  fmap MaximumQueriesPerKernel .
  Options.option Options.auto $
    Options.value 100 <>
    Options.long "max-queries-per-kernel" <>
    Options.metavar "QUERY_COUNT" <>
    Options.help "The maximum number of queries to include in each compute kernel. (defaults to 100)"

pInputDictionaryToml :: Parser InputDictionaryToml
pInputDictionaryToml =
  fmap InputDictionaryToml .
  Options.option Options.str $
    Options.short 'i' <>
    Options.long "input-toml" <>
    Options.metavar "DICTIONARY_TOML" <>
    Options.help "Path to a dictionary to compile (in TOML format)"

pOutputDictionarySea :: Parser OutputDictionarySea
pOutputDictionarySea =
  fmap OutputDictionarySea .
  Options.option Options.str $
    Options.short 'o' <>
    Options.long "output-c" <>
    Options.metavar "DICTIONARY_C" <>
    Options.help "Path to write the compiled dictionary (as C source code)"

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
    <*> pMaxMapSize

pReplCommand :: Parser String
pReplCommand =
  strOption $
    long "init" <>
    metavar "COMMAND" <>
    help "A command to execute in the REPL before interative evaluation begins."

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

pInputSparse :: Parser InputFile
pInputSparse =
  fmap (InputFile InputSparsePsv) $
    strOption (long "input-sparse-psv" <> metavar "INPUT_PSV")

pInputDense :: Parser InputFile
pInputDense =
  fmap (InputFile InputDensePsv) $
    strOption (long "input-dense-psv" <> metavar "INPUT_PSV")

pInputZebra :: Parser InputFile
pInputZebra =
  fmap (InputFile InputZebra) $
    strOption (long "input-zebra" <> metavar "INPUT_ZEBRA")

pOutputFile :: Parser OutputFile
pOutputFile =
  pOutputSparse <|> pOutputDense

pOutputSparse :: Parser OutputFile
pOutputSparse =
  fmap (\path -> OutputFile OutputSparsePsv path Nothing) $
    strOption (long "output-sparse-psv" <> metavar "OUTPUT_PSV")

pOutputDense :: Parser OutputFile
pOutputDense =
  OutputFile OutputDensePsv
    <$> strOption (long "output-dense-psv" <> metavar "OUTPUT_PSV")
    <*> optional pOutputSchema

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

pMaxMapSize :: Parser Int
pMaxMapSize =
  flip option (long "max-map-size" <> value defaultZebraMaxMapSize) $
    tryRead "--max-map-size NUMBER_ELEMENTS" readMaybe id

tryRead :: [Char] -> ([Char] -> Maybe a) -> (a -> b) -> ReadM b
tryRead err f g =
  readerAsk >>= \s -> case f s of
    Just i  -> return $ g i
    Nothing -> readerError err

------------------------------------------------------------------------

icicleFingerprint :: Fingerprint
icicleFingerprint =
  Fingerprint $
    "icicle-" <> T.pack buildInfoVersion

runCommand :: IcicleCommand -> IO ()
runCommand = \case
  IcicleRepl options ->
    repl options

  IcicleCompile options ->
    orDie renderCompileError $ do
      start <- liftIO getCurrentTime

      liftIO $
        putStrLn "icicle: starting compilation"

      icicleCompile options
      end <- liftIO getCurrentTime

      let
        seconds =
          realToFrac (end `diffUTCTime` start) :: Double

      liftIO $
        printf "icicle: compilation time = %.2fs\n" seconds

  IcicleQuery q -> do
    orDie renderIcicleError $ do
      liftIO . putStrLn $ "icicle: facts_limit = " <> show (optFactsLimit q)
      liftIO $ putStrLn "icicle: starting compilation"
      case inputFormat $ optInput q of
        InputSparsePsv
          -> bracketEitherT'
               (createPsvQuery icicleFingerprint q)
               (hoist liftIO . releaseQuery)
               (hoist liftIO . runQuery runPsvQuery (optOutputCode q))
        InputDensePsv
          -> bracketEitherT'
               (createPsvQuery icicleFingerprint q)
               (hoist liftIO . releaseQuery)
               (hoist liftIO . runQuery runPsvQuery (optOutputCode q))
        InputZebra
          -> bracketEitherT'
               (createZebraQuery icicleFingerprint q)
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
