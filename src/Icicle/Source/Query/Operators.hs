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
  , precedencePrefix
  , precedenceApplication
  , precedenceAlwaysParens
  , precedenceNeverParens
  ) where

import                  Icicle.Internal.Pretty

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


-- | Prefix operators are baked in to the parser, but these are used for pretty printing.
precedencePrefix :: (Int,Assoc)
precedencePrefix = (9, AssocLeft)

-- | Applications are baked in to the parser, but these are used for pretty printing.
precedenceApplication :: (Int,Assoc)
precedenceApplication = (10, AssocLeft)

-- | Wrap this in parentheses no matter what.
precedenceAlwaysParens :: (Int,Assoc)
precedenceAlwaysParens = (-1, AssocLeft)

-- | Never wrap this in parentheses: variable names, primitives etc
precedenceNeverParens :: (Int,Assoc)
precedenceNeverParens = (11, AssocLeft)


instance Pretty Op where
 pretty Div = "/"
 pretty Mul = "*"
 pretty Add = "+"
 pretty Sub = "-"
 pretty Negate = "-"

