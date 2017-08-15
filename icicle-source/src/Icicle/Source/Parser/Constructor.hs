{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Parser.Constructor (
    constructor
  , pattern
  ) where

import qualified        Icicle.Source.Lexer.Token  as T
import                  Icicle.Source.Parser.Token
import qualified        Icicle.Source.Query        as Q

import                  Icicle.Common.Base

import                  P hiding (exp)

import                  Data.List (lookup)

import                  Text.Parsec (parserFail)



constructor :: Parser Q.Constructor
constructor
 = do T.Variable n <- pConstructor
      case lookup n constructors of
       Just c -> return c
       Nothing -> parserFail ("Not a known constructor: " <> show n)

constructors :: [(Text, Q.Constructor)]
constructors
 = [("Some", Q.ConSome)
   ,("None", Q.ConNone)
   ,("True", Q.ConTrue)
   ,("False",Q.ConFalse)
   ,("Left", Q.ConLeft)
   ,("Right",Q.ConRight)
   ,("ExceptTombstone",         Q.ConError ExceptTombstone)
   ,("ExceptFold1NoValue",      Q.ConError ExceptFold1NoValue)
   ,("ExceptCannotCompute",     Q.ConError ExceptCannotCompute)
   ,("ExceptNotANumber",        Q.ConError ExceptNotANumber)
   ,("ExceptIndexOutOfBounds",  Q.ConError ExceptIndexOutOfBounds)
   ]


pattern :: Parser (Q.Pattern Var)
pattern
 = pat

 where
  pat
   = do p <- patVar <|> patCon <|> patParens
        tup p <|> return p

  tup p
   = do pEq (T.TOperator $ T.Operator ",")
        r <- pat
        return (Q.PatCon Q.ConTuple [p, r])

  patVar
   = do v <- pVariable
        return $ if   nameBase v == NameBase (T.Variable "_")
                 then Q.PatDefault
                 else Q.PatVariable v

  patCon
   = Q.PatCon <$> constructor <*> many patNested

  patNested
   = patVar <|> patParens <|> (flip Q.PatCon [] <$> constructor)

  patParens
   = pParenL *> pat <* pParenR
