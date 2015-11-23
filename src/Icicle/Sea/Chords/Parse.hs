{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Chords.Parse (
    parseChordLine
  , parseChordFile
) where

import           Data.Attoparsec.Text as A

import           Control.Applicative
import           Icicle.Data
import           Icicle.Data.DateTime

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           Data.Either.Combinators

import           P

data ChordParseError
 = ChordParseError T.Text
 deriving (Eq, Ord, Show)


parserOfRows :: Parser (Entity, [DateTime])
parserOfRows =
  (,) <$> (Entity <$> A.takeWhile (/= '|'))
      <*  pipe
      <*> (pure <$> pDate)
      <*  endOfInput

parseChordLine :: T.Text -> Either ChordParseError (Entity, [DateTime])
parseChordLine s
 = mapLeft (ChordParseError . T.pack)
 $ parseOnly parserOfRows s

pipe :: Parser ()
pipe = () <$ char '|'

parseChordFile :: TL.Text -> Either ChordParseError (Map.Map Entity [DateTime])
parseChordFile file
 = do let lines = fmap TL.toStrict
                $ TL.lines file
      entries <- mapM parseChordLine lines
      return $ Map.fromListWith (<>) entries

