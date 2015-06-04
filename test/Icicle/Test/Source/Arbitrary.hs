{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Source.Arbitrary where

import           Icicle.Source.Query
import qualified Icicle.Source.Lexer.Token as T

import           Icicle.Test.Arbitrary.Base
import           Orphanarium.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P


instance Arbitrary T.Variable where
 arbitrary
  = T.Variable <$> elements muppets

instance Arbitrary n => Arbitrary (Exp n) where
 arbitrary
  = oneof_sized
        [ Var    <$> arbitrary
        , Prim   <$> arbitrary ]
        [ (simplifyNestedX . Nested) <$> arbitrary
        , App    <$> arbitrary <*> arbitrary
        , preop
        , inop ]

  where
   preop
    = App
    <$> (Prim <$> operator_pre)
    <*> arbitrary

   inop
    = App
    <$> (App
            <$> (Prim <$> operator_bin)
            <*> arbitrary)
    <*> arbitrary

   operator_bin
    = oneof_vals
        [ Op    Div
        , Op    Mul
        , Op    Add
        , Op    Sub ]

   operator_pre
    = return $ Op Negate

instance Arbitrary Prim where
 arbitrary
  = oneof_vals
        [ Lit $ LitInt 0
        , Lit $ LitInt 1
        , Lit $ LitInt 2
        , Agg   Count
        , Agg   Newest
        , Agg   Oldest ]

instance Arbitrary n => Arbitrary (Context n) where
 arbitrary
  = oneof_sized
        [ Windowed <$> arbitrary <*> arbitrary
        , Latest . abs <$> arbitrary ]
        [ GroupBy  <$> arbitrary
        , Distinct <$> arbitrary
        , Filter   <$> arbitrary
        , LetFold  <$> arbitrary
        , Let Nothing
                   <$> arbitrary <*> arbitrary
        ]

instance Arbitrary WindowUnit where
 arbitrary
  = oneof
        [ Days   <$> pos
        , Months <$> pos
        , Weeks  <$> pos ]
  where
   pos = abs <$> arbitrary

instance Arbitrary n => Arbitrary (Fold (Query n) n) where
 arbitrary
  = Fold <$> arbitrary <*> arbitrary <*> arbitrary <*> return FoldTypeFoldl1


instance Arbitrary n => Arbitrary (Query n) where
 arbitrary
  = Query <$> arbitrary <*> arbitrary

instance Arbitrary n => Arbitrary (QueryTop n) where
 arbitrary
  = QueryTop <$> arbitrary <*> arbitrary
