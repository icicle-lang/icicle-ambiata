{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Core.Arbitrary where

import qualified Icicle.Internal.Pretty as PP

import           Icicle.Core.Base
import           Icicle.Core.Exp
import           Icicle.Core.Type

import           Orphanarium.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P

import           Data.Text


data Var
 = Var Text Int
 deriving (Eq,Ord,Show)

instance PP.Pretty Var where
 pretty (Var t i) = PP.text (show t) <> PP.text (show i)

instance Arbitrary Var where
  arbitrary =
    Var <$> elements viruses <*> oneof (fmap return [1..5])

instance Arbitrary n => Arbitrary (Name n) where
  arbitrary =
    Name <$> arbitrary

instance Arbitrary Prim where
  arbitrary =
    oneof $ fmap return
          [ PrimArith PrimArithMinus
          , PrimArith PrimArithPlus
          , PrimConst (PrimConstInt 0)
          , PrimConst (PrimConstInt 1)
          , PrimConst (PrimConstInt 2)
          ]
          
instance Arbitrary ValType where
  arbitrary =
   oneof [ return  $  IntT
         , ArrayT <$> arbitrary
         , PairT  <$> arbitrary <*> arbitrary
         ]

instance Arbitrary n => Arbitrary (Exp n) where
  arbitrary =
    oneof [ XVar  <$> arbitrary
          , XApp  <$> smaller arbitrary <*> smaller arbitrary
          , XPrim <$> arbitrary
          , XLam  <$> arbitrary <*> arbitrary <*> smaller arbitrary
          , XLet  <$> arbitrary <*> smaller arbitrary <*> smaller arbitrary
          ]
   where
    smaller g
     = sized (\s -> resize (s `div` 2) g)

