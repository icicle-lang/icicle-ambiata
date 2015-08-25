{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS -fno-warn-orphans #-}
module Icicle.Source.Lexer.Token (
    TOK
  , Token    (..)
  , Keyword  (..)
  , Operator (..)
  , Literal  (..)
  , Variable (..)
  , keywordOrVar
  , operator
  , SourcePos
  ) where

import Icicle.Internal.Pretty

import                  P

import qualified        Data.Char as C
import                  Data.String
import qualified        Data.Text as T
import                  Data.Text (Text)
import                  Data.List (lookup)

-- Bounded hack for getting names of all keywords
import                  Prelude (Enum(..), Bounded(..), minBound,maxBound)

-- Export source position type
import                  Text.Parsec (SourcePos)

type TOK = (Token, SourcePos)

data Token
 -- | Primitive keywords
 = TKeyword  Keyword
 -- | Ints, strings, whatever
 | TLiteral  Literal
 -- | Function operators like (+) (>)
 | TOperator Operator
 -- | Names. I dunno
 | TVariable Variable
 | TConstructor Variable

 -- | '('
 | TParenL
 -- | ')'
 | TParenR
 -- | '=' as in let bindings
 | TEqual
 -- | ':' as in cons or "followed by" for folds
 | TFollowedBy
 -- | '.' for separating function definitions.
 | TStatementEnd

 -- | '~>' for composition
 | TDataFlow

 -- | '->' for case patterns and their expressions
 | TFunctionArrow
 -- | '|' for separating case alternatives
 | TAlternative

 -- | An error
 | TUnexpected Text
 deriving (Eq, Ord, Show)

data Keyword
 = And
 | Average
 | Between
 | Case
 | Days
 | Distinct
 | End
 | Feature
 | Filter
 | Fold
 | Fold1
 | Group
 | Latest
 | Let
 | Months
 | Weeks
 | Windowed
 | Log
 | Exp
 | Double
 | Int
 deriving (Eq, Ord, Show, Enum, Bounded)



-- TODO: Strings, floats
data Literal
 = LitInt Int
 | LitDouble Double
 | LitString Text
 deriving (Eq, Ord, Show)

data Operator
 = Operator Text
 deriving (Eq, Ord, Show)

data Variable
 = Variable Text
 deriving (Eq, Ord, Show)


-- | Each keyword with their name
keywords :: [(Text, Keyword)]
keywords
 = fmap (\k -> (T.toLower $ T.pack $ show k, k))
  [minBound .. maxBound]

keywordOrVar :: Text -> Token
keywordOrVar t
 | Just k <- lookup t keywords
 = TKeyword    k
 | Just (c,_) <- T.uncons t
 , C.isUpper c
 = TConstructor $ Variable t
 | otherwise
 = TVariable $ Variable t

operator :: Text -> Token
operator t
 | t == "="
 = TEqual
 | t == ":"
 = TFollowedBy
 | t == "."
 = TStatementEnd
 | t == "~>"
 = TDataFlow
 | t == "->"
 = TFunctionArrow
 | t == "|"
 = TAlternative
 | otherwise
 = TOperator $ Operator t


instance Pretty Variable where
 pretty (Variable v) = text $ T.unpack v

instance IsString Variable where
 fromString s = Variable $ T.pack s

instance Pretty SourcePos where
 pretty sp = text $ show sp

