{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Repl.Parser (
    parseCommand

  , pCommand

  , pBlank
  , pH
  , pHelp

  , pSet
  , pSetOption
  , pSetSnapshot
  , pSetMaxMapSize
  , pSetFlagOn
  , pSetFlagOff
  , pFlag

  , pDate
  , pLexeme
  ) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.String (String)

import           Icicle.Data.Time (Date, dateOfYMD)
import           Icicle.Repl.Data
import           Icicle.Repl.Flag

import           P

import           System.IO (FilePath)

import           Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Lexer as Lexer
import qualified Text.Megaparsec.String as Mega
import           Text.Printf (printf)


pLexeme :: Mega.Parser a -> Mega.Parser a
pLexeme =
  Lexer.lexeme Mega.space

pBlank :: Mega.Parser Command
pBlank =
  CommandBlank <$ Mega.space

pH :: Mega.Parser Command
pH =
  CommandHelp <$ pLexeme (Mega.string ":h")

pHelp :: Mega.Parser Command
pHelp =
  CommandHelp <$ pLexeme (Mega.string ":help")

pFlag :: Mega.Parser Flag
pFlag = do
  name <- pLexeme $ some (Mega.alphaNumChar <|> Mega.char' '-')
  case Map.lookup name namedFlags of
    Nothing ->
      fail $ "Unknown flag: " <> name
    Just flag ->
      pure flag

pSetFlagOn :: Mega.Parser SetOption
pSetFlagOn =
  SetFlagOn <$> (Mega.char '+' *> pFlag)

pSetFlagOff :: Mega.Parser SetOption
pSetFlagOff =
  SetFlagOff <$> (Mega.char '-' *> pFlag)

pDate :: Mega.Parser Date
pDate =
  pLexeme $ do
    year <- fromIntegral <$> Lexer.integer <?> "year"
    _ <- Mega.char '-'
    month <- fromIntegral <$> Lexer.integer <?> "month"
    _ <- Mega.char '-'
    day <- fromIntegral <$> Lexer.integer <?> "day"

    case dateOfYMD year month day of
      Nothing ->
        fail $ printf "Not a valid gregorian calendar date: %04d-%02d-%02d"  year month day
      Just x ->
        pure x

pSetSnapshot :: Mega.Parser SetOption
pSetSnapshot = do
  _ <- pLexeme $ Mega.string "snapshot"
  SetSnapshot <$> pDate <?> "snapshot date"

pSetMaxMapSize :: Mega.Parser SetOption
pSetMaxMapSize = do
  _ <- pLexeme $ Mega.string "max-map-size"
  SetMaxMapSize . fromIntegral
    <$> pLexeme Lexer.integer
    <?> "integer value for maximum map size"

pSetLimit :: Mega.Parser SetOption
pSetLimit = do
  _ <- pLexeme $ Mega.string "limit"
  SetLimit . fromIntegral
    <$> pLexeme Lexer.integer
    <?> "integer value for output limit"

pSetCFlags :: Mega.Parser SetOption
pSetCFlags = do
  _ <- pLexeme $ Mega.string "cflags"
  SetCFlags
    <$> Mega.manyTill Mega.anyChar Mega.eol

pSetFlag :: Mega.Parser SetOption
pSetFlag =
  Mega.choice [
      pSetFlagOn
    , pSetFlagOff
    ]

pSetOption :: Mega.Parser SetOption
pSetOption =
  Mega.choice [
      pSetSnapshot
    , pSetMaxMapSize
    , pSetLimit
    , pSetCFlags
    ]

pSet :: Mega.Parser Command
pSet = do
  _ <- pLexeme $ Mega.string ":set"
  Mega.choice [
      CommandSet . pure <$> pSetOption
    , CommandSet <$> some pSetFlag
    , CommandSet <$> pure [SetShowOptions]
    ]

pDictionary :: Mega.Parser Command
pDictionary = do
  CommandDictionary <$ pLexeme (Mega.string ":dictionary")

pFilePath :: Mega.Parser FilePath
pFilePath =
  pLexeme . Mega.some $ Mega.satisfy (not . Char.isSpace)

pLoad :: Mega.Parser Command
pLoad = do
  _ <- pLexeme $ Mega.string ":load"
  CommandLoad <$> pFilePath <?> "path to dictionary, data or icicle functions"

pQuery :: Mega.Parser Command
pQuery =
  CommandQuery <$> Mega.someTill Mega.anyChar Mega.eol

pLet :: Mega.Parser Command
pLet = do
  _ <- pLexeme $ Mega.string ":let"
  pos <- Mega.getPosition

  let
    padding =
      List.replicate (fromIntegral . Mega.unPos $ Mega.sourceColumn pos) ' '

  CommandLet . (padding <>) <$> Mega.someTill Mega.anyChar Mega.eol

pComment :: Mega.Parser Command
pComment =
  fmap (CommandComment . ("--" <>)) $
    Mega.string "--" *> Mega.manyTill Mega.anyChar Mega.eol

pCommand :: Mega.Parser Command
pCommand =
  Mega.choice [
      Mega.try pHelp
    , pH
    , pSet
    , pDictionary
    , pLoad
    , pLet
    , pComment
    , pQuery
    , pBlank
    ]

parseCommand :: String -> Either String Command
parseCommand line =
  first Mega.parseErrorPretty $
    Mega.parse (Mega.space *> pCommand <* Mega.eof) "<interactive>" (line <> "\n")
