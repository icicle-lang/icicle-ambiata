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
  , putCustomFlagMessage
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
takeOptionInfo (FlagInfo flag name _) = do
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

putCustomFlagMessage :: Flag -> Repl ()
putCustomFlagMessage = \case
  FlagSeaEval ->
    liftIO $ do
      IO.putStrLn "                   _________-----_____"
      IO.putStrLn "        _____------           __      ----_"
      IO.putStrLn " ___----             ___------              \\"
      IO.putStrLn "    ----________        ----                 \\"
      IO.putStrLn "                -----__    |             _____)"
      IO.putStrLn "                     __-                /     \\"
      IO.putStrLn "         _______-----    ___--          \\    /)\\"
      IO.putStrLn "   ------_______      ---____            \\__/  /"
      IO.putStrLn "                -----__    \\ --    _          /\\"
      IO.putStrLn "                       --__--__     \\_____/   \\_/\\"
      IO.putStrLn "                               ----|   /          |"
      IO.putStrLn "                                   |  |___________|"
      IO.putStrLn "                                   |  | ((_(_)| )_)"
      IO.putStrLn "                                   |  \\_((_(_)|/(_)"
      IO.putStrLn "                                   \\             ("
      IO.putStrLn "                                    \\_____________)"
  _ ->
    pure ()

setFlagOn :: Flag -> Repl ()
setFlagOn flag = do
  putCustomFlagMessage flag
  case Map.lookup flag flagNames of
    Nothing ->
      liftIO . IO.putStrLn $ "Unknown flag: " <> show flag
    Just name -> do
      liftIO . IO.putStrLn $
        "OK, " <> name <> " is now on."
      modify $ \s ->
        s { stateFlags = Set.insert flag (stateFlags s) }

setFlagOff :: Flag -> Repl ()
setFlagOff flag =
  case Map.lookup flag flagNames of
    Nothing ->
      liftIO . IO.putStrLn $ "Unknown flag: " <> show flag
    Just name -> do
      liftIO . IO.putStrLn $
        "OK, " <> name <> " is now off."
      modify $ \s ->
        s { stateFlags = Set.delete flag (stateFlags s) }

setSnapshot :: Date -> Repl ()
setSnapshot date = do
  liftIO . IO.putStrLn $
    "OK, snapshot mode activated with a snapshot date of " <> Text.unpack (renderDate date) <> "."
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
    "OK, the maximum map size has been set to " <> show n <> " " <> pluralise n "element" "elements" <> "."
  modify $ \s ->
    s { stateMaxMapSize = n }

setLimit :: Int -> Repl ()
setLimit n = do
  liftIO . IO.putStrLn $
    "OK, the output limit has been set to " <> show n <> " " <> pluralise n "row" "rows" <> "."
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
