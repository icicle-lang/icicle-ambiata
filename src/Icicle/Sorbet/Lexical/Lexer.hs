{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Icicle.Sorbet.Lexical.Lexer (
    Lexer
  , lexProgram

  , LexerError(..)
  , renderLexerError
  ) where

import qualified Data.Char as Char
import           Data.Scientific (Scientific, scientific)
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.String (String)
import qualified Data.Text as T
import           Data.Thyme (Day, YearMonthDay(..), gregorianValid)
import           Data.Word (Word)

import           Icicle.Sorbet.Lexical.Escape
import           Icicle.Sorbet.Lexical.Syntax
import           Icicle.Sorbet.Position

import           P hiding (exp)

import           Text.Megaparsec (Dec, try, manyTill)
import           Text.Megaparsec (ErrorComponent(..), ShowErrorComponent(..))
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Lexer as Lexer
import           Text.Megaparsec.Prim (MonadParsec)
import           Text.Printf (printf)


type Lexer s m =
  (MonadParsec LexerError s m, Mega.Token s ~ Char)

data LexerError =
    LexerInvalidDate !YearMonthDay
  | LexerUnescapeError !UnescapeError
  | LexerDefault !Dec
    deriving (Eq, Ord, Show)

renderLexerError :: LexerError -> Text
renderLexerError = \case
  LexerInvalidDate (YearMonthDay y m d) ->
    T.pack $
      printf "%04d-%02d-%02d" y m d <>
      " is not a valid gregorian calendar date."
  LexerUnescapeError err ->
    renderUnescapeError err
  LexerDefault dec ->
    T.pack $ showErrorComponent dec

instance ErrorComponent LexerError where
  representFail =
    LexerDefault . representFail
  representIndentation old ref actual =
    LexerDefault $ representIndentation old ref actual

instance ShowErrorComponent LexerError where
  showErrorComponent =
    T.unpack . renderLexerError

lexProgram :: Lexer s m => m [Positioned Token]
lexProgram =
  lexSpace *> many lexToken <* Mega.eof

lexToken :: Lexer s m => m (Positioned Token)
lexToken =
  Mega.choice [
      lexDelimiter
    , lexLiteral
    , lexPrjId
    , lexVarId
    , lexConId
    , lexVarOp
    , lexConOp
    ]

lexDelimiter :: Lexer s m => m (Positioned Token)
lexDelimiter =
  Mega.choice [
      fmap (const Tok_LParen) <$> symbol "("
    , fmap (const Tok_RParen) <$> symbol ")"
    , fmap (const Tok_LBrace) <$> symbol "{"
    , fmap (const Tok_RBrace) <$> symbol "}"
    , fmap (const Tok_LBracket) <$> symbol "["
    , fmap (const Tok_RBracket) <$> symbol "]"
    , fmap (const Tok_Semi) <$> symbol ";"
    , fmap (const Tok_Comma) <$> symbol ","
    , fmap (const Tok_Backtick) <$> symbol "`"
    ]

-- The period '.' character is lexed as a projection when it placed immediately
-- preceding a variable identifier with no space between the two tokens. If
-- there is a space it will be lexed as an operator instead.
lexPrjId :: Lexer s m => m (Positioned Token)
lexPrjId =
  try $ do
    _ <- Mega.char '.'
    lexIdentifier Tok_PrjId lexVarIdHead

lexVarId :: Lexer s m => m (Positioned Token)
lexVarId =
  lexIdentifier Tok_VarId lexVarIdHead

lexVarIdHead :: Lexer s m => m Char
lexVarIdHead =
  Mega.lowerChar <|> Mega.char '_'

lexConId :: Lexer s m => m (Positioned Token)
lexConId =
  lexIdentifier Tok_ConId Mega.upperChar

lexIdentifier :: Lexer s m => (Text -> Token) -> m Char -> m (Positioned Token)
lexIdentifier f lexHead =
  lexeme $ do
    hd <- lexHead
    tl <- lexIdentifierTail
    let
      xs = hd : tl
    case Map.lookup xs reservedIdentifiers of
      Just tok ->
        pure tok
      Nothing ->
        pure . f $ T.pack xs

lexIdentifierTail :: Lexer s m => m [Char]
lexIdentifierTail =
  many (Mega.alphaNumChar <|> Mega.oneOf "_'")

reservedIdentifiers :: Map [Char] Token
reservedIdentifiers =
  Map.fromList [
      ("_", Tok_Wild)
    , ("of", Tok_Of)
    , ("if", Tok_If)
    , ("then", Tok_Then)
    , ("else", Tok_Else)
    , ("from", Tok_From)
    , ("in", Tok_In)
    , ("let", Tok_Let)
    , ("windowed", Tok_Windowed)
    , ("group", Tok_Group)
    , ("distinct", Tok_Distinct)
    , ("filter", Tok_Filter)
    , ("latest", Tok_Latest)
    , ("between", Tok_Between)
    , ("and", Tok_And)
    , ("days", Tok_Days)
    , ("months", Tok_Months)
    , ("weeks", Tok_Weeks)
    ]

lexVarOp :: Lexer s m => m (Positioned Token)
lexVarOp =
  lexOperator Tok_VarOp $
    Mega.oneOf commonSymbols

lexConOp :: Lexer s m => m (Positioned Token)
lexConOp =
  lexOperator Tok_ConOp $
    Mega.char ':'

lexOperator :: Lexer s m => (Text -> Token) -> m Char -> m (Positioned Token)
lexOperator f lexHead =
  lexeme $ do
    hd <- lexHead
    tl <- lexOperatorTail
    let
      xs = hd : tl
    case Map.lookup xs reservedOperators of
      Just tok ->
        pure tok
      Nothing ->
        pure . f $ T.pack xs

lexOperatorTail :: Lexer s m => m [Char]
lexOperatorTail =
  many . Mega.oneOf $ ':' : commonSymbols

commonSymbols :: [Char]
commonSymbols =
  "!#$%&*+./<=>?@\\^-~|"

reservedOperators :: Map [Char] Token
reservedOperators =
  Map.fromList [
      ("<-", Tok_LArrowDash)
    , ("->", Tok_RArrowDash)
    , ("=>", Tok_RArrowEquals)
    , ("=", Tok_Equals)
    , (":", Tok_Colon)
    , ("@", Tok_At)
    ]

lexLiteral :: Lexer s m => m (Positioned Token)
lexLiteral =
  Mega.choice [
      fmap Tok_Date <$> lexDate
    , fmap Tok_Rational <$> lexRational
    , fmap Tok_Integer <$> lexInteger
    , fmap Tok_String <$> lexString
    ]

lexDate :: Lexer s m => m (Positioned Day)
lexDate =
  lexeme $ do
    --
    -- We want the 'try' in here rather than somewhere further out because once
    -- we've successfully parsed the sequence XXXX-XX-XX, we want to lock it
    -- in.
    --
    -- If it turns out to be an invalid date, we don't want to backtrack and try
    -- other possibilities as that will ruin our chance to give a reasonable
    -- error message (i.e. that the date was not in the gregorian calendar.)
    --
    -- The scenario we want to avoid is that 2016-12-31 is parsed as a date
    -- literal, but 2016-12-32 is parsed as the expression: (2016 - 12) - 32
    --
    ymd <-
      try $ YearMonthDay
        <$> lexDecimalN 4 <* Mega.char '-'
        <*> lexDecimalN 2 <* Mega.char '-'
        <*> lexDecimalN 2

    case gregorianValid ymd of
      Nothing ->
        failWith $ LexerInvalidDate ymd
      Just day ->
        pure day

lexRational :: Lexer s m => m (Positioned Scientific)
lexRational =
  try . lexeme $ do
    integral <- lexDecimal

    let
      expOnly = do
        exp0 <- lexExponent
        pure $ scientific integral exp0

      fracExp = do
        (exp0, coeff) <- lexFractional integral
        exp1 <- lexExponent <|> pure 0
        pure $ scientific coeff (exp1 - exp0)

    expOnly <|> fracExp

lexFractional :: (Lexer s m, Num a) => a -> m (Int, a)
lexFractional coeff0 = do
  (_, exp, coeff1) <- Mega.char '.' *> diff (lexDecimal' coeff0)
  pure (exp, coeff1)

lexExponent :: Lexer s m => m Int
lexExponent =
  Mega.char 'e' *> Lexer.signed (pure ()) lexDecimal

lexInteger :: Lexer s m => m (Positioned Integer)
lexInteger =
  lexeme $ Mega.choice [
      lexDecimal
    , Mega.string "0b" *> lexBinary
    , Mega.string "0o" *> lexOctal
    , Mega.string "0x" *> lexHexadecimal
    ]

lexBinary :: (Lexer s m, Num a) => m a
lexBinary =
  lexNumber some isBit 2 0

lexOctal :: (Lexer s m, Num a) => m a
lexOctal =
  lexNumber some isOctit 8 0

lexDecimal :: (Lexer s m, Num a) => m a
lexDecimal =
  lexNumber some isDigit 10 0

lexDecimal' :: (Lexer s m, Num a) => a -> m a
lexDecimal' start =
  lexNumber some isDigit 10 start

lexDecimalN :: (Lexer s m, Num a) => Int -> m a
lexDecimalN n =
  lexNumber (Mega.count n) isDigit 10 0

lexHexadecimal :: (Lexer s m, Num a) => m a
lexHexadecimal =
  lexNumber some isHexit 16 0

isBit :: Char -> Bool
isBit c =
  (fromIntegral (Char.ord c - Char.ord '0') :: Word) <= 1

isOctit :: Char -> Bool
isOctit =
  Char.isOctDigit

isDigit :: Char -> Bool
isDigit =
  Char.isDigit

isHexit :: Char -> Bool
isHexit =
  Char.isHexDigit

lexNumber :: (Lexer s m, Num a) => (m Char -> m [Char]) -> (Char -> Bool) -> a -> a -> m a
lexNumber repeat isNumeral base start = do
  xs <- repeat $ Mega.satisfy isNumeral
  pure $
    fromBaseDigits base start xs

fromBaseDigits :: Num a => a -> a -> [Char] -> a
fromBaseDigits base !start = \case
  [] ->
    start
  x : xs ->
    let
      n = fromIntegral $ Char.digitToInt x
    in
      fromBaseDigits base (start * base + n) xs

lexString :: Lexer s m => m (Positioned Text)
lexString =
  lexeme $ do
    xs <- concat <$> (Mega.char '"' *> lexStringChar `manyTill` Mega.char '"')
    fmap T.pack . liftE . first LexerUnescapeError $ unescapeChars xs

lexStringChar :: Lexer s m => m [Char]
lexStringChar = do
  c <- Mega.anyChar
  case c of
    '\\' ->
      (c:) . (:[]) <$> Mega.anyChar
    '\"' ->
      mzero
    _ ->
      pure [c]

lexeme :: Lexer s m => m a -> m (Positioned a)
lexeme p =
  positioned p <* lexSpace

symbol :: Lexer s m => String -> m (Positioned ())
symbol sym =
  fmap (const ()) <$> lexeme (Mega.string sym)

lexSpace :: Lexer s m => m ()
lexSpace =
  Lexer.space lexSpaceChar lexLineComment lexBlockComment

lexSpaceChar :: Lexer s m => m ()
lexSpaceChar =
  -- keep it clean - no tabs or carriage returns allowed!
  void . Mega.satisfy $ \c ->
    c == '\n' ||
    c == ' '

lexLineComment :: Lexer s m => m ()
lexLineComment =
  Lexer.skipLineComment "--"

lexBlockComment :: Lexer s m => m ()
lexBlockComment =
  Lexer.skipBlockCommentNested "{-" "-}"

positioned :: Lexer s m => m a -> m (Positioned a)
positioned lex = do
  start <- position
  x <- lex
  Position efile eline ecol <- position
  pure $ Positioned start (Position efile eline $ ecol - 1) x

position :: Lexer s m => m Position
position = do
  Mega.SourcePos file line col <- Mega.getPosition
  pure $
    Position file
      (fromIntegral $ Mega.unPos line)
      (fromIntegral $ Mega.unPos col)

diff :: Lexer s m => m a -> m (Int, Int, a)
diff lex = do
  Mega.SourcePos _ line0 col0 <- Mega.getPosition
  x <- lex
  Mega.SourcePos _ line1 col1 <- Mega.getPosition

  let
    !n0 = fromIntegral $ Mega.unPos line0
    !n1 = fromIntegral $ Mega.unPos line1
    !n  = n1 - n0

    !c0 = fromIntegral $ Mega.unPos col0
    !c1 = fromIntegral $ Mega.unPos col1
    !c  = c1 - c0

  pure (n, c, x)

failWith :: Lexer s m => LexerError -> m a
failWith err =
  Mega.failure Set.empty Set.empty (Set.singleton err)

liftE :: Lexer s m => Either LexerError a -> m a
liftE =
  either (Mega.failure Set.empty Set.empty . Set.singleton) pure
