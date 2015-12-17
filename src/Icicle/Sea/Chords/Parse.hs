{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Sea.Chords.Parse (
    ChordParseError(..)
  , parseChordLine
  , parseChordFile
) where

import           Data.Attoparsec.Text as A

import           Control.Applicative
import           Icicle.Data
import           Icicle.Data.Time

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           P

data ChordParseError
 = ChordParseError T.Text
 deriving (Eq, Ord, Show)


parserOfRows :: Parser (Entity, [Time])
parserOfRows =
  (,) <$> (Entity <$> A.takeWhile (/= '|'))
      <*  pipe
      <*> (pure <$> pTime)
      <*  endOfInput

parseChordLine :: T.Text -> Either ChordParseError (Entity, [Time])
parseChordLine s
 = first (ChordParseError . T.pack)
 $ parseOnly parserOfRows s

pipe :: Parser ()
pipe = () <$ char '|'

parseChordFile :: TL.Text -> Either ChordParseError (Map.Map Entity [Time])
parseChordFile file
 = do let lines = fmap TL.toStrict
                $ TL.lines file
      entries <- mapM parseChordLine lines
      return $ Map.fromListWith (<>) entries

