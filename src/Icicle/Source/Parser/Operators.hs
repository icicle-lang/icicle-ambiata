{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Parser.Operators (
    Fixity (..)
  , operators
  ) where

import qualified        Icicle.Source.Query        as Q

import                  P

import                  Data.Text

data Fixity
 = Infixl Int
 | Infixr Int
 | Prefix

operators :: Text -> [(Fixity,Q.Op)]
operators op
 | op == "/"
 = [(Infixl 7, Q.Div)]
 | op == "*"
 = [(Infixl 7, Q.Mul)]
 | op == "-"
 = [(Infixl 6, Q.Sub)
   ,(Prefix, Q.Negate)]
 | op == "+"
 = [(Infixl 6, Q.Add)]

 | otherwise
 = []
