{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS -fno-warn-orphans  #-}

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
import Icicle.Data.Time

import                  P

import qualified        Data.Char as C
import                  Data.String
import qualified        Data.Text as T
import                  Data.Text (Text)
import                  Data.List (lookup)
import                  Data.Hashable (Hashable)

import                  GHC.Generics

-- Export source position type
import                  Text.Parsec (SourcePos, sourceLine, sourceColumn, sourceName)

type TOK = (Token, SourcePos)

data Token
 -- | Primitive keywords
 = TKeyword  !Keyword
 -- | Ints, strings, whatever
 | TLiteral  !Literal
 -- | Function operators like (+) (>)
 | TOperator    !Operator
 -- | Names. I dunno
 | TVariable    !Variable
 | TConstructor !Variable
 -- | Nested struct projections
 | TProjection  !Variable

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
 | TUnexpected !Text
 deriving (Eq, Ord, Show)

data Keyword =
 -- Date
   And
 | After
 | Before
 | Between
 | Days
 | Months
 | Weeks
 | Windowed

 -- Syntax
 | Case
 | Distinct
 | End
 | Feature
 | Filter
 | Fold
 | Fold1
 | Group
 | Latest
 | Let

 -- Builtin
 | Log
 | Exp
 | Sqrt
 | Abs
 | Double
 | Floor
 | Ceil
 | Round
 | Trunc
 | Keys
 | Vals
 | Seq
 | Box
 deriving (Eq, Ord, Show, Enum, Bounded)



data Literal
 = LitInt    !Int
 | LitDouble !Double
 | LitString !Text
 | LitTime   !Time
 deriving (Eq, Ord, Show)

newtype Operator
 = Operator Text
 deriving (Eq, Ord, Show)

newtype Variable
 = Variable Text
 deriving (Eq, Ord, Show, Generic)

instance Hashable Variable
instance NFData Variable

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
 pretty (Variable v)
   = annotate AnnVariable
   $ text
   $ T.unpack v

instance IsString Variable where
 fromString s = Variable $ T.pack s

instance Pretty SourcePos where
 pretty sp
  = pretty (sourceLine sp) <> ":" <> pretty (sourceColumn sp)
  <> (if sourceName sp == ""
      then ""
      else ":" <> pretty (sourceName sp))

