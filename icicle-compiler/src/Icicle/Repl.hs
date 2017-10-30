{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl (
    ReplOptions(..)
  , UseDotfiles(..)
  , repl
  , repl_
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import qualified Data.Set as Set
import           Data.String (String)

import           Icicle.Data.Time
import           Icicle.Dictionary
import           Icicle.Repl.Data
import           Icicle.Repl.Help
import           Icicle.Repl.Load
import           Icicle.Repl.Monad
import           Icicle.Repl.Option
import           Icicle.Repl.Parser
import           Icicle.Repl.Pretty
import           Icicle.Repl.Query

import           P

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import           System.IO (IO, FilePath)
import qualified System.IO as IO


evaluateCommand :: Command -> Repl ()
evaluateCommand = \case
  CommandBlank ->
    pure ()
  CommandHelp ->
    showHelp
  CommandSet options ->
    traverse_ setOption options
  CommandDictionary ->
    showDictionary
  CommandLoad path ->
    loadFile path
  CommandComment comment ->
    liftIO $ IO.putStrLn comment
  CommandLet function ->
    defineFunction function
  CommandQuery query ->
    evaluateQuery query

evaluate :: String -> Repl ()
evaluate line =
  case parseCommand line of
    Left err ->
      liftIO $ IO.putStr err
    Right command ->
      evaluateCommand command

getInitialState :: IO State
getInitialState = do
  date <- getCurrentDate
  use <- getUseColorDefault
  pure $
    State {
        stateInput =
          InputNone

      , stateDictionary =
          emptyDictionary

      , stateSnapshotDate =
          date

      , stateMaxMapSize =
          1024 * 1024

      , stateLimit =
          10

      , stateFlags =
          Set.fromList $ [
              FlagCoreEval
            , FlagCoreSimp
            , FlagSeaEval
            ] <>
            case use of
              NoColor ->
                []
              UseColor ->
                [FlagColor]

      , stateCFlags = "-O0 -DICICLE_ASSERT=0"
      }

readDotFile :: MonadIO m => FilePath -> m [String]
readDotFile path =
  liftIO $ do
    ok <- Directory.doesFileExist path
    if ok then
      List.lines <$> IO.readFile path
    else
      pure []

readDotFiles :: MonadIO m => m [String]
readDotFiles = do
  home <- liftIO Directory.getHomeDirectory
  current <- liftIO Directory.getCurrentDirectory

  commands1 <- readDotFile $ home </> ".icicle"
  commands2 <- readDotFile $ current </> ".icicle"

  pure $ commands1 <> commands2

putBanner :: Repl ()
putBanner = do
  use <- getUseColor

  let
    dblue =
      sgrSetColor use Dull Blue

    vblue =
      sgrSetColor use Vivid Blue

    dyellow =
      sgrSetColor use Dull Yellow

    reset =
      sgrReset use

    mrepl =
      vblue <> "REPL" <> dblue

    mhelp =
      dyellow <> ":help for help" <> dblue

    banner =
      List.unlines [
          ""
        , "  ██▓ ▄████▄   ██▓ ▄████▄   ██▓    ▓█████"
        , " ▓██▒▒██▀ ▀█  ▓██▒▒██▀ ▀█  ▓██▒    ▓█   ▀"
        , " ▒██▒▒▓█    ▄ ▒██▒▒▓█    ▄ ▒██░    ▒███"
        , " ░██░▒▓▓▄ ▄██▒░██░▒▓▓▄ ▄██▒▒██░    ▒▓█  ▄"
        , " ░██░▒ ▓███▀ ░░██░▒ ▓███▀ ░░██████▒░▒████▒"
        , " ░▓  ░ ░▒ ▒  ░░▓  ░ ░▒ ▒  ░░ ▒░▓  ░░░ ▒░ ░"
        , "  ▒ ░  ░  ▒    ▒ ░  ░  ▒   ░ ░ ▒  ░ ░ ░  ░"
        , "  ▒ ░░         ▒ ░░          ░ ░ " <> mrepl <> " ░"
        , "  ░  ░ ░       ░  ░ ░          ░  ░   ░  ░"
        , "     ░            ░"
        , "                  ░     " <> mhelp
        ]

  liftIO . IO.putStrLn $
    dblue <> banner <> reset

repl :: ReplOptions -> IO ()
repl options = do
  initial <- getInitialState
  runRepl initial $ do
    case replDotfiles options of
      UseDotfiles -> do
        commands <- readDotFiles
        traverse_ evaluate commands
      SkipDotfiles ->
        pure ()

    traverse_ evaluate $ replInit options
    traverse_ loadFile $ replLoad options

    putBanner

    withUserInput evaluate

repl_ :: IO ()
repl_ =
  repl $ ReplOptions SkipDotfiles [] []
