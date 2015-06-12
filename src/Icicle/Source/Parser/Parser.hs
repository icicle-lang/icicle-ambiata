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
import                  Icicle.Source.Parser.Operators
import qualified        Icicle.Source.Query        as Q

import                  P hiding (exp)

import                  Text.Parsec (many1, parserFail, getPosition)

top :: Parser (Q.QueryTop T.SourcePos Var)
top
 = do   pKeyword T.Feature
        v <- pVariable
        pFlowsInto
        q <- query
        return $ Q.QueryTop v q


query :: Parser (Q.Query T.SourcePos Var)
query
 = do   cs <- many context
        x  <- exp
        return $ Q.Query cs x


context :: Parser (Q.Context T.SourcePos Var)
context
 = do   c <- context1
        pFlowsInto
        return c
 where
  context1
   =   pKeyword T.Windowed *> cwindowed
   <|> pKeyword T.Group    *> (Q.GroupBy  <$> getPosition <*> exp)
   <|> pKeyword T.Distinct *> (Q.Distinct <$> getPosition <*> exp)
   <|> pKeyword T.Filter   *> (Q.Filter   <$> getPosition <*> exp)
   <|> pKeyword T.Latest   *> (Q.Latest   <$> getPosition <*> pLitInt)
   <|> pKeyword T.Let      *> (cletfold <|> clet)

  cwindowed
   = cwindowed2 <|> cwindowed1

  cwindowed1
   = do p  <- getPosition
        t1 <- windowUnit
        return $ Q.Windowed p t1 Nothing

  cwindowed2
   = do p <- getPosition
        pKeyword T.Between 
        t1 <- windowUnit
        pKeyword T.And
        t2 <- windowUnit
        return $ Q.Windowed p t2 $ Just t1

  clet
   = do p <- getPosition
        n <- pVariable
        pEq T.TEqual
        x <- exp
        return $ Q.Let p n x

  cletfold
   = do pKeyword T.Fold
        p <- getPosition
        n <- pVariable
        pEq T.TEqual
        z <- exp
        pEq T.TFollowedBy
        k <- exp
        return $ Q.LetFold p (Q.Fold n z k Q.FoldTypeFoldl1)

exp :: Parser (Q.Exp T.SourcePos Var)
exp
 = do   xs <- many1 ((Left <$> exp1) <|> op)
        either (parserFail.show) return
               (defix xs)
 where
  -- Get the position before reading the operator
  op = do p <- getPosition
          o <- pOperator
          return (Right (o,p))

exp1 :: Parser (Q.Exp T.SourcePos Var)
exp1
 =   (Q.Var     <$> getPosition <*> var)
 <|> (Q.Prim    <$> getPosition <*> prims)
 <|> (simpNested<$> getPosition <*> parens)
 where
  var
   = pVariable

  -- TODO: this should be a lookup rather than asum
  prims
   =  asum (fmap (\(k,q) -> pKeyword k *> return q) primitives)
   <|> ((Q.Lit . Q.LitInt) <$> pLitInt)

  simpNested _ (Q.Query [] x)
   = x
  simpNested p q
   = Q.Nested p q

  parens
   =   pParenL *> query <* pParenR


windowUnit :: Parser Q.WindowUnit
windowUnit
 = do   i <- pLitInt
        unit T.Days (Q.Days i) <|> unit T.Months (Q.Months i) <|> unit T.Weeks (Q.Weeks i)
 where
  unit kw q
   = pKeyword kw *> return q


primitives :: [(T.Keyword, Q.Prim)]
primitives
 = [(T.Newest, Q.Agg Q.Newest)
   ,(T.Count,  Q.Agg Q.Count)
   ,(T.Oldest, Q.Agg Q.Oldest)
   ]

