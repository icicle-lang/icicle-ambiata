{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Pretty (
    Color(..)
  , ColorIntensity(..)

  , UseColor(..)
  , getUseColor
  , getUseColorDefault

  , putPretty
  , putError
  , putErrorPosition
  , putSection

  , withColor
  , ppType

  , sgrColor
  , sgrSetColor
  , sgrReset

  , getTerminalWidth
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State (MonadState(..))

import qualified Data.List as List
import           Data.String (String)

import           Icicle.Internal.Pretty (Pretty)
import qualified Icicle.Internal.Pretty as Pretty
import           Icicle.Repl.Data
import           Icicle.Repl.Flag

import           P

import           System.Console.ANSI (Color(..), ColorIntensity(..))
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as Terminal
import qualified System.IO as IO

import qualified Text.ParserCombinators.Parsec as Parsec


data UseColor =
    NoColor
  | UseColor
    deriving (Eq, Ord, Show)

getTerminalWidth :: MonadIO m => m (Maybe Int)
getTerminalWidth =
  liftIO . fmap (fmap Terminal.width) $ Terminal.size

getUseColor :: MonadState State m => m UseColor
getUseColor =
  ifSet FlagColor (pure UseColor) (pure NoColor)

getUseColorDefault :: MonadIO m => m UseColor
getUseColorDefault =
  liftIO $ do
    ok <- ANSI.hSupportsANSI IO.stdout
    if ok then
      pure UseColor
    else
      pure NoColor

sgrReset :: UseColor -> String
sgrReset = \case
  NoColor ->
    mempty
  UseColor ->
    ANSI.setSGRCode [ANSI.Reset]

sgrSetColor :: UseColor -> ColorIntensity -> Color -> String
sgrSetColor use intensity color =
  case use of
    NoColor ->
      mempty
    UseColor ->
      ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color]

sgrColor :: UseColor -> ColorIntensity -> Color -> String -> String
sgrColor use intensity color str =
  sgrSetColor use intensity color <> str <> sgrReset use

ppType :: Pretty a => a -> String
ppType =
  List.concat .
  List.lines .
  Pretty.display .
  Pretty.renderCompact .
  Pretty.pretty

withColor :: UseColor -> Pretty.Annotation -> String -> String
withColor use annot str =
  case annot of
    Pretty.AnnError ->
      sgrColor use Dull Red str

    Pretty.AnnErrorHeading ->
      sgrColor use Vivid Red str

    Pretty.AnnHeading ->
      sgrColor use Vivid Yellow str

    Pretty.AnnPunctuation ->
      sgrColor use Vivid Black str

    Pretty.AnnKeyword ->
      sgrColor use Dull Blue str

    Pretty.AnnConstant ->
      sgrColor use Vivid Red str

    Pretty.AnnPrimitive ->
      sgrColor use Dull Yellow str

    Pretty.AnnBinding ->
      sgrColor use Dull Magenta str

    Pretty.AnnVariable ->
      sgrColor use Dull Cyan str

    Pretty.AnnConstructor ->
      sgrColor use Dull Green str

putPretty :: (MonadState State m, MonadIO m, Pretty a) => a -> m ()
putPretty x = do
  use <- getUseColor
  width <- fromMaybe 80 <$> getTerminalWidth
  liftIO .
    IO.putStrLn .
    Pretty.displayDecorated (withColor use) .
    Pretty.renderPretty 0.4 width $
    Pretty.pretty x

putErrorPosition :: (MonadState State m, MonadIO m) => Maybe Parsec.SourcePos -> m ()
putErrorPosition = \case
  Nothing ->
    pure ()

  Just x -> do
    use <- getUseColor
    liftIO . IO.putStrLn $
      List.replicate (Parsec.sourceColumn x + 1) ' ' <>
      withColor use Pretty.AnnError "^"

putError :: (MonadState State m, MonadIO m) => Pretty.Doc -> Maybe Parsec.SourcePos -> m ()
putError x mpos = do
  putErrorPosition mpos
  putPretty . Pretty.reannotate Pretty.AnnErrorHeading $ Pretty.prettyH1 "Error"
  liftIO $ IO.putStrLn ""
  putPretty x
  liftIO $ IO.putStrLn ""

putSection :: (MonadState State m, MonadIO m, Pretty a) => String -> a -> m ()
putSection heading x = do
  putPretty $ Pretty.prettyH1 heading
  liftIO $ IO.putStrLn ""
  putPretty x
  liftIO $ IO.putStrLn ""
