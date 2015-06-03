{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Operators (
    Op          (..)
  , Fixity      (..)
  , Infixity    (..)
  , Assoc       (..)
  , OpsOfSymbol (..)
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
 = FInfix  Infixity
 | FPrefix
 deriving (Show, Eq, Ord)

data Infixity
 = Infix Assoc Int
 deriving (Show, Eq, Ord)

data Assoc
 = AssocLeft | AssocRight
 deriving (Show, Eq, Ord)


fixity :: Op -> Fixity
fixity o
 = case o of
    Div -> FInfix $ Infix AssocLeft 7
    Mul -> FInfix $ Infix AssocLeft 7
    Add -> FInfix $ Infix AssocLeft 6
    Sub -> FInfix $ Infix AssocLeft 6
    Negate
        -> FPrefix


data OpsOfSymbol
 = OpsOfSymbol
 { opInfix  :: Maybe Op
 , opPrefix :: Maybe Op }
 deriving (Show, Eq, Ord)


symbol :: Text -> OpsOfSymbol
symbol s
 = case s of
    "/" -> inf Div
    "*" -> inf Mul
    "+" -> inf Add
    "-" -> OpsOfSymbol (Just Sub) (Just Negate)

    _   -> OpsOfSymbol  Nothing    Nothing
 where
  inf o = OpsOfSymbol (Just o) Nothing

