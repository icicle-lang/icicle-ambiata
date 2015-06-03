{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Operators (
    Op     (..)
  , Fixity (..)
  , Assoc  (..)
  , fixity
  , symbol
  ) where

import                  P

import                  Data.Text

data Op
 = Div
 | Mul
 | Add
 | Sub
 | Negate
 deriving (Show, Eq, Ord)

data Fixity
 = Infix Assoc Int
 | Prefix
 deriving (Show, Eq, Ord)

data Assoc
 = AssocLeft | AssocRight
 deriving (Show, Eq, Ord)


fixity :: Op -> Fixity
fixity o
 = case o of
    Div -> Infix AssocLeft 7
    Mul -> Infix AssocLeft 7
    Add -> Infix AssocLeft 6
    Sub -> Infix AssocLeft 6
    Negate
        -> Prefix

-- XXX: this should return
-- > (Maybe InfixOp, Maybe PrefixOp)
-- for a given symbol.
symbol :: Text -> [Op]
symbol s
 = case s of
    "/" -> [Div]
    "*" -> [Mul]
    "+" -> [Add]
    "-" -> [Sub, Negate]
    _   -> []

