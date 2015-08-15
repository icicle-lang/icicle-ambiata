{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Arbitrary where

import           Icicle.Source.Query
import qualified Icicle.Source.Lexer.Token as T
import           Icicle.Common.Fresh

import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Core.Arbitrary ()
import           Orphanarium.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P

import                  Control.Monad.Trans.Either
import qualified Data.Text as Text

freshnamer = (counterPrefixNameState (T.Variable . Text.pack . show) (T.Variable "v"))
freshtest p
 = snd <$> runFreshT p freshnamer

freshcheck
 = snd
 . flip runFresh freshnamer
 . runEitherT

instance Arbitrary T.Variable where
 arbitrary
  = T.Variable <$> elements muppets

instance Arbitrary n => Arbitrary (Exp () n) where
 arbitrary
  = oneof_sized
        [ Var    () <$> arbitrary
        , Prim   () <$> arbitrary ]
        [ (simplifyNestedX . Nested ()) <$> arbitrary
        , App    () <$> arbitrary <*> arbitrary
        , preop
        , inop ]

  where
   preop
    = App ()
    <$> (Prim () <$> operator_pre)
    <*> arbitrary

   inop
    = App ()
    <$> (App ()
            <$> (Prim () <$> operator_bin)
            <*> arbitrary)
    <*> arbitrary

   operator_bin
    = oneof_vals
        [ Op    (ArithDouble Div)
        , Op    (ArithBinary Mul)
        , Op    (ArithBinary Add)
        , Op    (ArithBinary Sub)
        , Op    (ArithBinary Pow)
        , Op    (Relation Gt)
        , Op    (Relation Eq) ]

   operator_pre
    = return $ Op (ArithUnary Negate)

instance Arbitrary Prim where
 arbitrary
  = oneof_vals
        [ Lit $ LitInt 0
        , Lit $ LitInt 1
        , Lit $ LitInt 2
        ]

instance Arbitrary n => Arbitrary (Context () n) where
 arbitrary
  = oneof_sized
        [ Windowed () <$> arbitrary <*> arbitrary
        , Latest   () . abs <$> arbitrary ]
        [ GroupBy  () <$> arbitrary
        , Distinct () <$> arbitrary
        , Filter   () <$> arbitrary
        , LetFold  () <$> arbitrary
        , Let      () <$> arbitrary <*> arbitrary
        ]

instance Arbitrary WindowUnit where
 arbitrary
  = oneof
        [ Days   <$> pos
        , Months <$> pos
        , Weeks  <$> pos ]
  where
   pos = abs <$> arbitrary

instance Arbitrary n => Arbitrary (Fold (Query () n) () n) where
 arbitrary
  = Fold <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FoldType where
 arbitrary
  = oneof
        [ return FoldTypeFoldl1
        , return FoldTypeFoldl ]


instance Arbitrary n => Arbitrary (Query () n) where
 arbitrary
  = Query <$> arbitrary <*> arbitrary

instance Arbitrary n => Arbitrary (QueryTop () n) where
 arbitrary
  = QueryTop <$> arbitrary <*> arbitrary
