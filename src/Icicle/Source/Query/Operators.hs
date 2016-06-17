{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Operators (
    Op            (..)
  , ArithUnary    (..)
  , ArithBinary   (..)
  , ArithDouble   (..)
  , Relation      (..)
  , LogicalBinary (..)
  , LogicalUnary  (..)
  , TimeBinary    (..)
  , Fixity        (..)
  , Infixity      (..)
  , Assoc         (..)
  , OpsOfSymbol   (..)
  , fixity
  , symbol
  , precedencePrefix
  , precedenceApplication
  , precedenceAlwaysParens
  , precedenceNeverParens
  ) where

import                  Icicle.Internal.Pretty

import                  P

data Op
 = ArithUnary    ArithUnary
 | ArithBinary   ArithBinary
 | ArithDouble   ArithDouble
 | Relation      Relation
 | LogicalUnary  LogicalUnary
 | LogicalBinary LogicalBinary
 | TimeBinary    TimeBinary

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

data LogicalUnary
 = Not
 deriving (Show, Eq, Ord)

data LogicalBinary
 = And
 | Or
 deriving (Show, Eq, Ord)

data TimeBinary
 = DaysBefore
 | DaysAfter
 | WeeksBefore
 | WeeksAfter
 | MonthsBefore
 | MonthsAfter
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

    LogicalUnary Not
     -> FPrefix
    LogicalBinary And
     -> FInfix $ Infix AssocLeft 3
    LogicalBinary Or
     -> FInfix $ Infix AssocLeft 2

    TimeBinary _
     -> FInfix $ Infix AssocLeft 6

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

    "!" -> pre $ LogicalUnary  Not
    "&&"-> inf $ LogicalBinary  And
    "||"-> inf $ LogicalBinary  Or

    "days before"   -> inf $ TimeBinary DaysBefore
    "days after"    -> inf $ TimeBinary DaysAfter
    "weeks before"  -> inf $ TimeBinary WeeksBefore
    "weeks after"   -> inf $ TimeBinary WeeksAfter
    "months before" -> inf $ TimeBinary MonthsBefore
    "months after"  -> inf $ TimeBinary MonthsAfter

    "," -> inf TupleComma

    _   -> OpsOfSymbol  Nothing    Nothing
 where
  inf o = OpsOfSymbol (Just o) Nothing
  pre o = OpsOfSymbol Nothing (Just o)

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

 pretty (LogicalUnary Not)      = "!"
 pretty (LogicalBinary And)     = "&&"
 pretty (LogicalBinary Or)      = "||"

 pretty (TimeBinary DaysAfter)    = "days after"
 pretty (TimeBinary DaysBefore)   = "days before"
 pretty (TimeBinary WeeksAfter)   = "weeks after"
 pretty (TimeBinary WeeksBefore)  = "weeks before"
 pretty (TimeBinary MonthsAfter)  = "months after"
 pretty (TimeBinary MonthsBefore) = "months before"

 pretty TupleComma              = ","

