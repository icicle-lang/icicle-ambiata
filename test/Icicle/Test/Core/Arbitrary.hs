{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Icicle.Test.Core.Arbitrary where

import qualified Icicle.Internal.Pretty as PP

import           Icicle.Core.Base
import           Icicle.Core.Exp
import           Icicle.Core.Type
import           Icicle.Core.Eval.Exp

import           Icicle.Core.Stream
import           Icicle.Core.Reduce
import           Icicle.Core.Program.Program

import           Icicle.Test.Arbitrary.Base
import           Orphanarium.Corpus

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           P

import           Data.Text


data Var
 = Var Text Int
 deriving (Eq,Ord,Show)

-- | Generate a fresh variable name that isn't mentioned elsewhere in the program,
-- (assuming that the generated program doesn't mention it)
fresh :: Int -> Name Var
fresh = Name . Var "_fresh"

-- | Check if values are equal except for functions/closures
-- Because closure heaps can differ..
equalExceptFunctions :: Eq n => Value n -> Value n -> Bool
equalExceptFunctions p q
 | VFun{} <- p
 , VFun{} <- q
 = True
 | otherwise
 = p == q

equalExceptFunctionsE :: (Eq n, Eq l) => Either l (Value n) -> Either l (Value n) -> Bool
equalExceptFunctionsE p q
 | Right p' <- p
 , Right q' <- q
 = p' `equalExceptFunctions` q'
 | otherwise
 = p == q


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
   -- Need to be careful about making smaller things.
   -- It's fine if they're big, but they have to fit in memory.
   oneof_sized_vals
         [ IntT ]
         [ ArrayT <$> arbitrary
         , PairT  <$> arbitrary <*> arbitrary
         ]

instance Arbitrary n => Arbitrary (Exp n) where
  arbitrary =
    oneof_sized
          [ XVar  <$> arbitrary
          , XPrim <$> arbitrary ]
          [ XApp  <$> arbitrary <*> arbitrary
          , XLam  <$> arbitrary <*> arbitrary <*> arbitrary
          , XLet  <$> arbitrary <*> arbitrary <*> arbitrary
          ]

instance Arbitrary n => Arbitrary (Stream n) where
 arbitrary =
   oneof_sized_vals
         [ Source ]
         [ STrans <$> st <*> arbitrary <*> arbitrary ]
  where
   st = oneof [ SFilter <$> arbitrary
              , SMap    <$> arbitrary <*> arbitrary
              , STake   <$> arbitrary ]

instance Arbitrary n => Arbitrary (Reduce n) where
 arbitrary =
   oneof [ RFold   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
         , RLatest <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary n => Arbitrary (Program n) where
 arbitrary =
   Program <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

