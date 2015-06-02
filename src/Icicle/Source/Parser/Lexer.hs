{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Parser.Lexer (
    lexer
  ) where

import Icicle.Source.Parser.Token

import                  P

import qualified        Data.Char as C
import qualified        Data.Text as T

import                  Prelude   (read)


lexer :: T.Text -> [Token]
lexer ts
 = go $ T.stripStart ts
 where
  go t
   = case T.uncons t of
      Nothing
       -> []
      Just (c,t')
       | isVarStart c
       -> let (a,b) = T.span isVarRest t'
              v     = T.cons c a
          in  keywordOrVar v : go b

       | C.isDigit c
       -> let (a,b) = T.span C.isDigit t'
              v     = T.unpack $ T.cons c a
          in  TLiteral (LitInt (read v)) : go b

       | c == '('
       -> TParenL : go t'
       | c == ')'
       -> TParenR : go t'

       | otherwise
       -> let (a,b) = T.span (not . isVarRest) t'
              v     = T.cons c a
          in  operator v : go b

  isVarStart c
   = C.isAlpha c || c == '_'
  isVarRest c
   = C.isAlphaNum c || c == '_'


