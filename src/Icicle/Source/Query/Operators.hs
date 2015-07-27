{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Operators (
    Op          (..)
  , ArithUnary  (..)
  , ArithBinary (..)
  , ArithDouble (..)
  , Relation    (..)
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
 = ArithUnary  ArithUnary
 | ArithBinary ArithBinary
 | ArithDouble ArithDouble
 | Relation Relation

 | TupleComma
 deriving (Show, Eq, Ord)

data ArithUnary
 = Negate
 deriving (Show, Eq, Ord)

data ArithBinary
 = Mul
 | Add
 | Sub
 | Pow
 deriving (Show, Eq, Ord)

data ArithDouble
 = Div
 deriving (Show, Eq, Ord)


data Relation
 = Lt
 | Le
 | Gt
 | Ge
 | Eq
 | Ne
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
    ArithUnary _
     -> FPrefix

    ArithBinary Mul
     -> FInfix $ Infix AssocLeft 7
    ArithBinary Add
     -> FInfix $ Infix AssocLeft 6
    ArithBinary Sub
     -> FInfix $ Infix AssocLeft 6
    ArithBinary Pow
     -> FInfix $ Infix AssocRight 8

    ArithDouble Div
     -> FInfix $ Infix AssocLeft 7

    Relation _
     -> FInfix $ Infix AssocLeft 4

    TupleComma
        -> FInfix $ Infix AssocLeft 0


data OpsOfSymbol
 = OpsOfSymbol
 { opInfix  :: Maybe Op
 , opPrefix :: Maybe Op }
 deriving (Show, Eq, Ord)


symbol :: Text -> OpsOfSymbol
symbol s
 = case s of
    "/" -> inf (ArithDouble Div)
    "*" -> inf (ArithBinary Mul)
    "+" -> inf (ArithBinary Add)
    "^" -> inf (ArithBinary Pow)
    "-" -> OpsOfSymbol (Just $ ArithBinary Sub) (Just $ ArithUnary Negate)

    ">" -> inf $ Relation Gt
    ">="-> inf $ Relation Ge
    "<" -> inf $ Relation Lt
    "<="-> inf $ Relation Le
    "=="-> inf $ Relation Eq
    "/="-> inf $ Relation Ne


    "," -> inf TupleComma

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
 pretty (ArithUnary Negate)     = "-"
 pretty (ArithBinary Mul)       = "*"
 pretty (ArithBinary Add)       = "+"
 pretty (ArithBinary Sub)       = "-"
 pretty (ArithBinary Pow)       = "^"
 pretty (ArithDouble Div)       = "/"

 pretty (Relation Lt)           = "<"
 pretty (Relation Le)           = "<="
 pretty (Relation Gt)           = ">"
 pretty (Relation Ge)           = ">="
 pretty (Relation Eq)           = "=="
 pretty (Relation Ne)           = "/="

 pretty TupleComma              = ","

