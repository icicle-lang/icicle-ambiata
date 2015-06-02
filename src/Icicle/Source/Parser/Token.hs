{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Parser.Token (
    Token    (..)
  , Keyword  (..)
  , Operator (..)
  , Literal  (..)
  , Variable (..)
  , keywordOrVar
  , operator
  ) where

import                  P

import qualified        Data.Text as T
import                  Data.Text (Text)
import                  Data.List (lookup)
import                  Prelude (Enum(..), Bounded(..), minBound,maxBound)

data Token
 -- | Primitive keywords
 = TKeyword  Keyword
 -- | Ints, strings, whatever
 | TLiteral  Literal
 -- | Function operators like (+) (>)
 | TOperator Operator
 -- | Names. I dunno
 | TVariable Variable

 -- | '('
 | TParenL
 -- | ')'
 | TParenR
 -- | '=' as in let bindings
 | TEqual
 -- | ':' as in cons or "followed by" for folds
 | TFollowedBy

 -- | '~>' for composition
 | TDataFlow
 deriving (Eq, Ord, Show)

data Keyword
 = Feature
 | Let
 | Windowed
 | Between
 | And
 | Group
 | Distinct
 | Latest
 | Filter
 | Newest
 | Oldest
 | Count
 | Max
 | Average
 deriving (Eq, Ord, Show, Enum, Bounded)



data Literal
 = LitInt Int
 deriving (Eq, Ord, Show)

data Operator
 = Operator Text
 deriving (Eq, Ord, Show)

data Variable
 = Variable Text
 deriving (Eq, Ord, Show)


keywords :: [(Text, Keyword)]
keywords
 = fmap (\k -> (T.toLower $ T.pack $ show k, k))
  [minBound .. maxBound]

keywordOrVar :: Text -> Token
keywordOrVar t
 | Just k <- lookup t keywords
 = TKeyword    k
 | otherwise
 = TVariable $ Variable t

operator :: Text -> Token
operator t
 | t == "="
 = TEqual
 | t == ":"
 = TFollowedBy
 | t == "~>"
 = TDataFlow
 | otherwise
 = TOperator $ Operator t

