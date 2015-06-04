{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.Lexer.Lexer (
    lexer
  ) where

import Icicle.Source.Lexer.Token

import                  P

import qualified        Data.Char as C
import qualified        Data.Text as T

import                  Prelude   (read)

-- TODO: handle strings and floats.
-- TODO: strings will require error handling
-- TODO: we also need source positions here
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
          in  keywordOrVar v : lexer b

       | C.isDigit c
       -> let (a,b) = T.span C.isDigit t'
              v     = T.unpack $ T.cons c a
              -- Read will always 'succeed' since we know it's just digits
              -- (If it's too big 'read Int' just silently overflows)
              -- TODO error on overflow
              i     = read v
          in  TLiteral (LitInt i) : lexer b

       | c == '('
       -> TParenL : lexer t'
       | c == ')'
       -> TParenR : lexer t'

       | otherwise
       -> let (a,b) = T.span isOperator t'
              v     = T.cons c a
          in  operator v : lexer b

  isVarStart c
   = C.isAlpha c || c == '_'
  isVarRest c
   = C.isAlphaNum c || c == '_'

  isOperator c
   =  not (isVarRest c)
   && not (C.isSpace c)
   && c /= '(' && c /= ')'


