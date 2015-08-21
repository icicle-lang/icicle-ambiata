-- | Constructors, like Some, None, tuples etc
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Constructor (
    Constructor (..)
  , arityOfConstructor
  ) where

import                  Icicle.Internal.Pretty

import                  P

data Constructor
 -- Option
 = ConSome
 | ConNone

 -- ,
 | ConTuple

 -- Bool
 | ConTrue
 | ConFalse
 deriving (Eq, Ord, Show)


arityOfConstructor :: Constructor -> Int
arityOfConstructor cc
 = case cc of
    ConSome -> 1
    ConNone -> 0

    ConTuple -> 2

    ConTrue -> 0
    ConFalse -> 0

instance Pretty Constructor where
 pretty ConSome = "Some"
 pretty ConNone = "None"

 pretty ConTuple = "Tuple"

 pretty ConTrue = "True"
 pretty ConFalse = "False"

