{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Parser.Parser (
    top
  , query
  , context
  , exp
  , windowUnit
  ) where

import qualified        Icicle.Source.Lexer.Token  as T
import                  Icicle.Source.Parser.Token
import qualified        Icicle.Source.Query        as Q

import                  P hiding (exp)

top :: Parser (Q.QueryTop Var)
top
 = do   pKeyword T.Feature
        v <- pVariable
        pFlowsInto
        q <- query
        return $ Q.QueryTop v q


query :: Parser (Q.Query Var)
query
 = do   cs <- many context
        x  <- exp
        return $ Q.Query cs x


context :: Parser (Q.Context Var)
context
 = do   c <- context1
        pFlowsInto
        return c
 where
  context1
   =   pKeyword T.Windowed *> cwindowed
   <|> pKeyword T.Group    *> (Q.GroupBy  <$> exp)
   <|> pKeyword T.Distinct *> (Q.Distinct <$> exp)
   <|> pKeyword T.Filter   *> (Q.Filter   <$> exp)
   <|> pKeyword T.Let      *> (cletfold <|> clet)

  cwindowed
   = cwindowed2 <|> cwindowed1

  cwindowed1
   = do t1 <- windowUnit
        return $ Q.Windowed t1 Nothing

  cwindowed2
   = do pKeyword T.Between 
        t1 <- windowUnit
        pKeyword T.And
        t2 <- windowUnit
        return $ Q.Windowed t1 $ Just t2

  clet
   = do n <- pVariable
        pEq T.TEqual
        x <- exp
        return $ Q.Let Nothing n x

  cletfold
   = do pKeyword T.Fold
        n <- pVariable
        pEq T.TEqual
        z <- exp
        pEq T.TFollowedBy
        k <- exp
        return $ Q.LetFold (Q.Fold n z k Q.FoldTypeFoldl1)


exp :: Parser (Q.Exp Var)
exp
 -- TODO: handle operators
 =   (Q.Var     <$> var)
 <|> (Q.Agg     <$> agg)
 <|> (Q.Nested  <$> parens)
 where
  var
   = pVariable
  agg
   =   pKeyword T.Count  *> return Q.Count
   <|> pKeyword T.Newest *> (Q.Newest <$> exp) 
   <|> pKeyword T.Oldest *> (Q.Oldest <$> exp) 

  parens
   =   pParenL *> query <* pParenR


windowUnit :: Parser Q.WindowUnit
windowUnit
 = do   i <- pLitInt
        unit T.Days (Q.Days i) <|> unit T.Months (Q.Months i) <|> unit T.Weeks (Q.Weeks i)
 where
  unit kw q
   = pKeyword kw >> return q
