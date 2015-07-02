{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Lexer.Lexer (
    lexer
  ) where

import Icicle.Source.Lexer.Token

import                  P

import qualified        Data.Char as C
import qualified        Data.List as L
import qualified        Data.Text as T

import                  Prelude   (read)

import qualified        Text.Parsec.Pos as Pos



lexer :: Pos.SourceName -> T.Text -> [TOK]
lexer file inp
 = lexerString file $ T.unpack inp

lexerString :: Pos.SourceName -> [Char] -> [TOK]
lexerString file inp
 = let pos  = L.scanl Pos.updatePosChar (Pos.initialPos file) inp
       both = inp `L.zip` pos
   in  lexerPositions both

-- TODO: handle strings and floats.
-- TODO: strings will require error handling
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
   =  let (a,b) = L.span (C.isDigit . fst) t'
          v     = T.unpack $ cons c a
          -- Read will always 'succeed' since we know it's just digits
          -- (If it's too big 'read Int' just silently overflows)
          -- TODO error on overflow
          i     = read v
      in  (TLiteral (LitInt i), pos) : lexerPositions b

   | c == '('
   =  (TParenL, pos) : lexerPositions t'
   | c == ')'
   =  (TParenR, pos) : lexerPositions t'

   | otherwise
   =  let (a,b) = L.span (isOperator . fst) t'
          v     = cons c a
      in  (operator v, pos) : lexerPositions b


  cons a b = T.pack (a : fmap fst b)

  isVarStart c
   = C.isAlpha c || c == '_'
  isVarRest c
   = C.isAlphaNum c || c == '_'

  isOperator c
   =  not (isVarRest c)
   && not (C.isSpace c)
   && c /= '(' && c /= ')'


