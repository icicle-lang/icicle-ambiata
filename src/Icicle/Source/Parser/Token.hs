{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Parser.Token (
    Parser
  , Var
  , pTok
  , pSatisfy
  , pEq
  , pKeyword
  , pVariable
  , pLitInt
  , pParenL
  , pParenR
  , pFlowsInto
  ) where

import qualified        Icicle.Source.Lexer.Token as T

import                  P

import                  Text.Parsec

type Parser a
 = Parsec [T.Token] () a

type Var = T.Variable

pTok :: (T.Token -> Maybe a) -> Parser a
pTok p
 = tokenPrim show pos p
 where
  -- TODO: source positions
  pos s _ _ = s


pSatisfy :: (T.Token -> Bool) -> Parser T.Token
pSatisfy p
 = pTok (\t -> if p t then Just t else Nothing)

pEq :: T.Token -> Parser ()
pEq t
 = pSatisfy (==t) >> return ()


pKeyword :: T.Keyword -> Parser ()
pKeyword kw
 = pEq (T.TKeyword kw)


pVariable :: Parser Var
pVariable 
 = pTok get
 where
  get (T.TVariable v) = Just v
  get  _              = Nothing



pLitInt :: Parser Int
pLitInt
 = pTok get
 where
  get (T.TLiteral (T.LitInt i))
   = Just i
  get _
   = Nothing

pParenL :: Parser ()
pParenL = pEq T.TParenL

pParenR :: Parser ()
pParenR = pEq T.TParenR

pFlowsInto :: Parser ()
pFlowsInto = pEq T.TDataFlow

