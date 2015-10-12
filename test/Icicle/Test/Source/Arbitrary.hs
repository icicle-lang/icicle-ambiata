{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Icicle.Test.Source.Arbitrary where

import           Icicle.Source.Query
import qualified Icicle.Source.Lexer.Token as T
import           Icicle.Common.Fresh

import           Icicle.Test.Arbitrary.Base
import           Icicle.Test.Core.Arbitrary ()
import           Disorder.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P

import           Control.Monad.Trans.Either
import qualified Data.Text as Text
import qualified Data.List as List


freshnamer = counterPrefixNameState (T.Variable . Text.pack . show) (T.Variable "v")

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
        , Case   () <$> arbitrary
                    <*> arbitrary `suchThat` (not . null)
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
        , Op    (Relation Eq)
        , Op    (LogicalBinary And)
        , Op    (LogicalBinary Or)
        , Op    (DateBinary DaysBefore)
        , Op    (DateBinary DaysAfter)
        , Op    (DateBinary MonthsBefore)
        , Op    (DateBinary MonthsAfter)
        , Fun   (DaysBetween)
        , Fun   (DaysEpoch) ]

   operator_pre
    = oneof_vals
        [ Op (ArithUnary Negate)
        , Op (LogicalUnary Not)]

instance Arbitrary Prim where
 arbitrary
  = oneof_vals
        [ Lit $ LitInt 0
        , Lit $ LitInt 1
        , Lit $ LitInt 2 ]

instance Arbitrary Constructor where
 arbitrary
  = oneof_vals [ ConSome, ConNone, ConTuple, ConTrue, ConFalse, ConLeft, ConRight ]

instance Arbitrary n => Arbitrary (Pattern n) where
 arbitrary
  = oneof_sized [ return PatDefault, PatVariable <$> arbitrary ]
                [ patcon ]
  where
   patcon
    = do con  <- arbitrary
         args <- List.take (arityOfConstructor con) <$> (infiniteList :: Gen [Pattern n])
         return (PatCon con args)


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
        -- no reason to generate group-folds that are not on groups
        , GroupFold ()
            <$> arbitrary
            <*> arbitrary
            <*> (Nested () <$> arbitrary `suchThat` hasGroup)
        ]
  where
   hasGroup q
    | (GroupBy _ _ : _) <- contexts q = True
    | otherwise = False


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
  = QueryTop <$> arbitrary <*> arbitrary <*> arbitrary
