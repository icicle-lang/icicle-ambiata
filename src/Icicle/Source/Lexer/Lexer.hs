{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Lexer.Lexer (
    lexer
  ) where

import Icicle.Source.Lexer.Token

import                  P

import qualified        Data.Char as C
import qualified        Data.List as L
import qualified        Data.Text as T

import                  Text.Read as R

import qualified        Text.Parsec.Pos as Pos



lexer :: Pos.SourceName -> T.Text -> [TOK]
lexer file inp
 = lexerString file $ T.unpack inp

lexerString :: Pos.SourceName -> [Char] -> [TOK]
lexerString file inp
 = let pos  = L.scanl Pos.updatePosChar (Pos.initialPos file) inp
       both = inp `L.zip` pos
   in  lexerPositions both

lexerPositions :: [(Char, Pos.SourcePos)] -> [TOK]
lexerPositions ts
 = go $ L.dropWhile (C.isSpace . fst) ts
 where
  go []
   = []
  go ((c,pos) : t')
   | isVarStart c
   =  let (a,b) = L.span (isVarRest . fst) t'
          v     = cons c a
      in  (keywordOrVar v, pos) : lexerPositions b

   | C.isDigit c
   =  let (a,b) = L.span (isFloatDigit . fst) t'
          v     = c : fmap fst a
          lit
           | Just i <- R.readMaybe v
           = TLiteral $ LitInt i
           | Just d <- R.readMaybe v
           = TLiteral $ LitDouble d
           | otherwise
           = TUnexpected $ T.pack v
      in  (lit, pos) : lexerPositions b

   | c == '('
   =  (TParenL, pos) : lexerPositions t'
   | c == ')'
   =  (TParenR, pos) : lexerPositions t'

   | c == '"'
   -- TODO: handle escapes.
   =  let (a,b) = L.span ((/='"') . fst) t'
      in  case b of
           ((q,_) : rest)
            | q == '"'
            -> (TLiteral $ LitString $ T.pack $ fmap fst a, pos) : lexerPositions rest
           _
            -> [(TUnexpected T.empty, pos)]

   | otherwise
   =  let (a,b) = L.span (isOperator . fst) t'
          v     = cons c a
      in  (operator v, pos) : lexerPositions b


  cons a b = T.pack (a : fmap fst b)

  isVarStart c
   = C.isAlpha c || c == '_'
  isVarRest c
   = C.isAlphaNum c || c == '_'

  isFloatDigit c
   = C.isDigit c || c == '.' || c == 'e'

  isOperator c
   =  not (isVarRest c)
   && not (C.isSpace c)
   && c /= '(' && c /= ')'


