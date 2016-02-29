{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Parser.Token (
    Parser
  , Var
  , pTok
  , pSatisfy
  , pEq
  , pKeyword
  , pConstructor
  , pVariable
  , pProjections
  , pOperator
  , pLitInt
  , pLitDouble
  , pLitString
  , pLitTime
  , pParenL
  , pParenR
  , pFlowsInto
  ) where

import qualified        Icicle.Source.Lexer.Token as T
import                  Icicle.Common.Base
import                  Icicle.Data.Time

import                  P hiding ((<|>))

import                  Text.Parsec
import                  Data.Text (Text)
import qualified        Data.Text as Text

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
 = pTok get <|> days <|> weeks <|> months <?> "operator"
 where
  get (T.TOperator op) = Just op
  get  _               = Nothing

  days   = pKeyword T.Days *> (T.Operator ("days before") <$ pKeyword T.Before
                           <|> T.Operator ("days after")  <$ pKeyword T.After)

  weeks  = pKeyword T.Weeks *> (T.Operator ("weeks before") <$ pKeyword T.Before
                            <|> T.Operator ("weeks after")  <$ pKeyword T.After)

  months = pKeyword T.Months *> (T.Operator ("months before") <$ pKeyword T.Before
                             <|> T.Operator ("months after")  <$ pKeyword T.After)


pVariable :: Parser (Name Var)
pVariable
 = pTok get <?> "variable"
 where
  get (T.TVariable v) = Just (nameOf (NameBase v))
  get  _              = Nothing

pConstructor :: Parser Var
pConstructor
 = pTok get <?> "constructor"
 where
  get (T.TConstructor v) = Just v
  get  _                 = Nothing

pProjections :: Parser (Name Var)
pProjections
  = do v     <-        pTok var <?> "variable"
       ps    <- many1 (pTok proj <?> "nested struct projections")
       let n  = NameBase $ T.Variable $ Text.intercalate "." (v:ps)
       return $ nameOf n
 where
  var  (T.TVariable (T.Variable v))   = Just v
  var  _                              = Nothing
  proj (T.TProjection (T.Variable p)) = Just p
  proj _                              = Nothing


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

pLitTime :: Parser Time
pLitTime
 = pTok get <?> "time literal"
 where
  get (T.TLiteral (T.LitTime i))
   = Just i
  get _
   = Nothing


pParenL :: Parser ()
pParenL = pEq T.TParenL <?> "left parenthesis"

pParenR :: Parser ()
pParenR = pEq T.TParenR <?> "right parenthesis"

pFlowsInto :: Parser ()
pFlowsInto = pEq T.TDataFlow <?> "flows into (~>)"

