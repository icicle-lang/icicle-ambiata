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
  , pOperator
  , pLitInt
  , pLitDouble
  , pLitString
  , pParenL
  , pParenR
  , pFlowsInto
  ) where

import qualified        Icicle.Source.Lexer.Token as T
import                  Icicle.Common.Base

import                  P

import                  Text.Parsec
import                  Data.Text (Text)

type Parser a
 = Parsec [T.TOK] () a

type Var = T.Variable

pTok :: (T.Token -> Maybe a) -> Parser a
pTok p
 = tokenPrim show pos (p.fst)
 where
  pos _ (_,sp) _ = sp


pSatisfy :: (T.Token -> Bool) -> Parser T.Token
pSatisfy p
 = pTok (\t -> if p t then Just t else Nothing)

pEq :: T.Token -> Parser ()
pEq t
 = pSatisfy (==t) >> return ()


pKeyword :: T.Keyword -> Parser ()
pKeyword kw
 = pEq (T.TKeyword kw) <?> ("keyword " <> show kw)

pOperator :: Parser T.Operator
pOperator 
 = pTok get <?> "operator"
 where
  get (T.TOperator op) = Just op
  get  _               = Nothing


pVariable :: Parser (Name Var)
pVariable 
 = pTok get <?> "variable"
 where
  get (T.TVariable v) = Just (Name v)
  get  _              = Nothing



pLitInt :: Parser Int
pLitInt
 = pTok get <?> "number literal"
 where
  get (T.TLiteral (T.LitInt i))
   = Just i
  get _
   = Nothing

pLitDouble :: Parser Double
pLitDouble
 = pTok get <?> "double literal"
 where
  get (T.TLiteral (T.LitDouble i))
   = Just i
  get _
   = Nothing

pLitString :: Parser Text
pLitString
 = pTok get <?> "string literal"
 where
  get (T.TLiteral (T.LitString i))
   = Just i
  get _
   = Nothing


pParenL :: Parser ()
pParenL = pEq T.TParenL <?> "left parenthesis"

pParenR :: Parser ()
pParenR = pEq T.TParenR <?> "right parenthesis"

pFlowsInto :: Parser ()
pFlowsInto = pEq T.TDataFlow <?> "flows into (~>)"

