{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Icicle.Dictionary.Parse.Toml
  ( module Icicle.Dictionary.Parse.Toml
  , module Icicle.Dictionary.Parse.Types
  ) where

import           Prelude             hiding (concat, takeWhile)

import           Control.Applicative hiding (many, optional, (<|>))
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import qualified Data.Set            as S
import           Data.Text           (Text, pack, unpack)

import           Data.Time.Format    (parseTime)
import           System.Locale       (defaultTimeLocale, iso8601DateFormat)

import           Numeric             (readHex)
import           Text.Parsec
import           Text.Parsec.String

import           Icicle.Dictionary.Parse.Types

{-

This parser is forked from htmol on hackage, as we want to be able to provide
error messages for the dictionary, which includes knowing the locations
of items within the document. We have therefore removed the array of tables construct,
as these have no natural base location.
Strings are especially important, as the lexer needs to know where string characters
exist, so instead of Data.Text, we have [(Char, SourcePos)].

-}

-- | Parses a complete document formatted according to the TOML spec.
tomlDoc :: Parser Table
tomlDoc = do
    skipBlanks
    topTable <- table
    namedSections <- many namedSection
    eof  -- ensures input is completely consumed
    case join topTable (reverse namedSections) of
      Left msg -> fail (unpack msg)  -- TODO: allow Text in Parse Errors
      Right r  -> return $ r
  where
    join tbl []     = Right tbl
    join tbl (x:xs) = case join tbl xs of Left msg -> Left msg
                                          Right r  -> insert x r

-- | Parses a table of key-value pairs.
table :: Parser Table
table = do
    pairs <- try (many (assignment <* skipBlanks)) <|> (try skipBlanks >> return [])
    case hasDup (map sndOf3 pairs) of
      Just k  -> fail $ "Cannot redefine key " ++ (unpack k)
      Nothing -> return $ M.fromList (map (\(p, k, v) -> (k, (NTValue v, p))) pairs)
  where
    sndOf3 (_, a, _) = a
    hasDup        :: Ord a => [a] -> Maybe a
    hasDup xs     = dup' xs S.empty
    dup' []     _ = Nothing
    dup' (x:xs) s = if S.member x s then Just x else dup' xs (S.insert x s)


-- | Parses a 'Table' or 'TableArray' with its header.
-- The resulting tuple has the header's value in the first position, and the
-- 'NTable' or 'NTArray' in the second.

namedSection :: Parser ([Text], (Node, SourcePos))
namedSection = do
    pos <- getPosition
    hdr <- try (tableHeader)
    skipBlanks
    tbl <- table
    skipBlanks
    return $ (hdr, (NTable tbl, pos))

-- | Parses a table header.
tableHeader :: Parser [Text]
tableHeader = between (char '[') (char ']') headerValue


-- | Parses a table array header.
tableArrayHeader :: Parser [Text]
tableArrayHeader = between (twoChar '[') (twoChar ']') headerValue
  where
    twoChar c = count 2 (char c)


-- | Parses the value of any header (names separated by dots), into a list of 'Text'.
headerValue :: Parser [Text]
headerValue = (pack <$> many1 headerNameChar) `sepBy1` (char '.')
  where
    headerNameChar = satisfy (\c -> c /= ' ' && c /= '\t' && c /= '\n' &&
                                    c /= '[' && c /= ']'  && c /= '.'  && c /= '#')


-- | Parses a key-value assignment.
assignment :: Parser (SourcePos, Text, TValue)
assignment = do
    pos <- getPosition
    k <- pack <$> many1 keyChar
    skipBlanks >> char '=' >> skipBlanks
    v <- value
    return (pos, k, v)
  where
    -- TODO: Follow the spec, e.g.: only first char cannot be '['.
    keyChar = satisfy (\c -> c /= ' ' && c /= '\t' && c /= '\n' &&
                             c /= '=' && c /= '#'  && c /= '[')


-- | Parses a value.
value :: Parser TValue
value = (try array    <?> "array")
    <|> (try boolean  <?> "boolean")
    <|> (try anyStr   <?> "string")
    <|> (try datetime <?> "datetime")
    <|> (try float    <?> "float")
    <|> (try integer  <?> "integer")


--
-- | * Toml value parsers
--

array :: Parser TValue
array = (try (arrayOf array)    <?> "array of arrays")
    <|> (try (arrayOf boolean)  <?> "array of booleans")
    <|> (try (arrayOf anyStr)   <?> "array of strings")
    <|> (try (arrayOf datetime) <?> "array of datetimes")
    <|> (try (arrayOf float)    <?> "array of floats")
    <|> (try (arrayOf integer)  <?> "array of integers")


boolean :: Parser TValue
boolean = VBoolean <$> ( (try . string $ "true")  *> return True  <|>
                         (try . string $ "false") *> return False )


anyStr :: Parser TValue
anyStr = try multiBasicStr <|> try basicStr <|> try multiLiteralStr <|> try literalStr


basicStr :: Parser TValue
basicStr = VString <$> between dQuote dQuote (many (flip (,) <$> getPosition <*> strChar))
  where
    strChar = try escSeq <|> try (satisfy (\c -> c /= '"' && c /= '\\'))
    dQuote  = char '\"'


multiBasicStr :: Parser TValue
multiBasicStr = VString <$> (openDQuote3 *> (manyTill (flip (,) <$> getPosition <*> strChar) dQuote3))
  where
    -- | Parse the a tripple-double quote, with possibly a newline attached
    openDQuote3 = try (dQuote3 <* char '\n') <|> try dQuote3
    -- | Parse tripple-double quotes
    dQuote3     = count 3 $ char '"'
    -- | Parse a string char, accepting escaped codes, ignoring escaped white space
    strChar     = escWhiteSpc *> (escSeq <|> (satisfy (/= '\\'))) <* escWhiteSpc
    -- | Parse escaped white space, if any
    escWhiteSpc = many $ char '\\' >> char '\n' >> (many $ satisfy (\c -> isSpc c || c == '\n'))


literalStr :: Parser TValue
literalStr = VString <$> between sQuote sQuote (many (flip (,) <$> getPosition <*> satisfy (/= '\'')))
  where
    sQuote = char '\''


multiLiteralStr :: Parser TValue
multiLiteralStr = VString <$> (openSQuote3 *> (manyTill (flip (,) <$> getPosition <*> anyChar ) sQuote3))
  where
    -- | Parse the a tripple-single quote, with possibly a newline attached
    openSQuote3 = try (sQuote3 <* char '\n') <|> try sQuote3
    -- | Parse tripple-single quotes
    sQuote3     = try . count 3 . char $ '\''


datetime :: Parser TValue
datetime = do
    d <- manyTill anyChar (try $ char 'Z')
    let  mt = parseTime defaultTimeLocale (iso8601DateFormat $ Just "%X") d
    case mt of Just t  -> return $ VDatetime t
               Nothing -> fail "parsing datetime failed"


-- | Attoparsec 'double' parses scientific "e" notation; reimplement according to Toml spec.
float :: Parser TValue
float = VFloat <$> do
    n <- intStr <* lookAhead (satisfy (\c -> c == '.' || c == 'e' || c == 'E'))
    d <- try (satisfy (== '.') *> uintStr) <|> return "0"
    e <- try (satisfy (\c -> c == 'e' || c == 'E') *> intStr) <|> return "0"
    return . read . L.concat $ [n, ".", d, "e", e]
  where
    sign    = try (string "-") <|> (try (char '+') >> return "") <|> return ""
    uintStr = many1 digit
    intStr  = do s <- sign
                 u <- uintStr
                 return . L.concat $ [s, u]


integer :: Parser TValue
integer = VInteger <$> (signed $ read <$> (many1 digit))



--
-- * Utility functions
--

-- | Parses the elements of an array, while restricting them to a certain type.
arrayOf :: Parser TValue -> Parser TValue
arrayOf p = VArray <$> between (char '[') (char ']') (skipBlanks *> separatedValues)
  where
    separatedValues = sepEndBy (skipBlanks *> p <* skipBlanks) comma <* skipBlanks
    comma           = skipBlanks >> char ',' >> skipBlanks


-- | Parser for escape sequences.
escSeq :: Parser Char
escSeq = char '\\' *> escSeqChar
  where
    escSeqChar =  try (char '"')  *> return '"'
              <|> try (char '\\') *> return '\\'
              <|> try (char '/')  *> return '/'
              <|> try (char 'b')  *> return '\b'
              <|> try (char 't')  *> return '\t'
              <|> try (char 'n')  *> return '\n'
              <|> try (char 'f')  *> return '\f'
              <|> try (char 'r')  *> return '\r'
              <|> try (char 'u')  *> unicodeHex 4
              <|> try (char 'U')  *> unicodeHex 8
              <?> "escape character"


-- | Parser for unicode hexadecimal values of representation length 'n'.
unicodeHex :: Int -> Parser Char
unicodeHex n = do
    h <- count n (satisfy isHex)
    let v = fst . head . readHex $ h
    return $ if v <= maxChar then toEnum v else '_'
  where
    isHex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
    maxChar = fromEnum (maxBound :: Char)


-- | Parser for signs (a plus or a minus).
signed :: Num a => Parser a -> Parser a
signed p =  try (negate <$> (char '-' *> p))
        <|> try (char '+' *> p)
        <|> try p


-- | Parses the (rest of the) line including an EOF, whitespace and comments.
skipBlanks :: Parser ()
skipBlanks = skipMany blank
  where
    blank   = try ((many1 $ satisfy isSpc) >> return ()) <|> try comment <|> try eol
    comment = char '#' >> (many $ satisfy (/= '\n')) >> return ()


-- | Results in 'True' for whitespace chars, tab or space, according to spec.
isSpc :: Char -> Bool
isSpc c = c == ' ' || c == '\t'


-- | Parse an EOL, as per TOML spec this is 0x0A a.k.a. '\n' or 0x0D a.k.a. '\r'.
eol :: Parser ()
eol = satisfy (\c -> c == '\n' || c == '\r') >> return ()
