{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Option (
    setOption
  , setFlagOn
  , setFlagOff
  , setSnapshot
  , setMaxMapSize
  , showOptions

  , takeOptionInfo
  , putAllOption
  , putOption
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State (MonadState(..), modify)

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (String)
import qualified Data.Text as Text

import           Icicle.Data.Time
import           Icicle.Repl.Data
import           Icicle.Repl.Flag
import           Icicle.Repl.Monad

import           P

import qualified System.IO as IO

putOption :: Int -> OptionInfo -> Repl ()
putOption n x =
  let
    m =
      length (optionName x)
  in
    liftIO . IO.putStrLn $
         "    "
      <> optionName x
      <> ":"
      <> List.replicate (n - m) ' '
      <> "    "
      <> optionValue x

putAllOption :: [OptionInfo] -> Repl ()
putAllOption xs =
  let
    n =
      List.maximum $ fmap (length . optionName) xs
  in
    mapM_ (putOption n) xs

takeOptionInfo :: FlagInfo -> Repl OptionInfo
takeOptionInfo (FlagInfo flag name _ _ _) = do
  x <- get
  if Set.member flag (stateFlags x) then
    pure $ OptionInfo name "✓"
  else
    pure $ OptionInfo name "-"

showOptions :: Repl ()
showOptions = do
  liftIO $ IO.putStrLn ""
  liftIO $ IO.putStrLn "  Current settings:"
  liftIO $ IO.putStrLn ""

  s <- get
  xs <- traverse takeOptionInfo allFlags
  putAllOption xs
  liftIO $ IO.putStrLn ""
  putAllOption [
      OptionInfo "max-map-size" .
        show $ stateMaxMapSize s
    , OptionInfo "snapshot-date" .
        Text.unpack . renderDate $ stateSnapshotDate s
    , OptionInfo "input" .
        show $ stateInput s
    ]

  liftIO $ IO.putStrLn ""

setFlagOn :: Flag -> Repl ()
setFlagOn flag = do
  case Map.lookup flag flagInfo of
    Nothing ->
      liftIO . IO.putStrLn $ "Unknown flag: " <> show flag
    Just x -> do
      liftIO . IO.putStrLn $ flagOn x
      modify $ \s ->
        s { stateFlags = Set.insert flag (stateFlags s) }

setFlagOff :: Flag -> Repl ()
setFlagOff flag =
  case Map.lookup flag flagInfo of
    Nothing ->
      liftIO . IO.putStrLn $ "Unknown flag: " <> show flag
    Just x -> do
      liftIO . IO.putStrLn $ flagOff x
      modify $ \s ->
        s { stateFlags = Set.delete flag (stateFlags s) }

setSnapshot :: Date -> Repl ()
setSnapshot date = do
  liftIO . IO.putStrLn $
    "Snapshot mode activated with a snapshot date of " <> Text.unpack (renderDate date) <> "."
  modify $ \s ->
    s { stateSnapshotDate = date }

pluralise :: Int -> String -> String -> String
pluralise n single plural =
  if n == 1 then
    single
  else
    plural

setMaxMapSize :: Int -> Repl ()
setMaxMapSize n = do
  liftIO . IO.putStrLn $
    "The maximum map size has been set to " <> show n <> " " <> pluralise n "element" "elements" <> "."
  modify $ \s ->
    s { stateMaxMapSize = n }

setLimit :: Int -> Repl ()
setLimit n = do
  liftIO . IO.putStrLn $
    "The output limit has been set to " <> show n <> " " <> pluralise n "row" "rows" <> "."
  modify $ \s ->
    s { stateLimit = n }

setOption :: SetOption -> Repl ()
setOption = \case
  SetShowOptions ->
    showOptions
  SetFlagOn flag ->
    setFlagOn flag
  SetFlagOff flag ->
    setFlagOff flag
  SetSnapshot time ->
    setSnapshot time
  SetMaxMapSize size ->
    setMaxMapSize size
  SetLimit n ->
    setLimit n
