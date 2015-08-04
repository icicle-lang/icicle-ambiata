{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Constraints (
    DataConstraint (..)
  , DataConstraintError (..)
  , Subst(..)
  , unifyData
  , substData
  ) where

import                  Icicle.Common.Type
import                  Icicle.Source.Type.Base
-- import                  Icicle.Source.Type.Subtypes

import qualified        Data.Map as Map

import P


data DataConstraint n
 = Subtype (BaseType n) (BaseType n)
 deriving (Eq, Ord, Show)

data DataConstraintError
 = DataConstraintErrorTodo
 deriving (Eq, Ord, Show)

data Subst n
 = Subst (Map.Map n (BaseType n))

unifyData   :: [DataConstraint n]
            -> Either DataConstraintError (Subst n)
unifyData _cs
 = return $ Subst Map.empty
{-
 = foldl merge
 $ fmap gen cs
 where
  gen (DCSubtype lo hi)
   = gen' lo hi

  gen' lo hi
   | lo == hi
   = []
   | otherwise
   = case (lo, hi) of
      (
-}

substData :: Ord n => Subst n -> BaseType n -> BaseType n
substData (Subst s) tt
 = go tt
 where
  go t
   = case t of
      BaseType IntT      -> t
      BaseType DoubleT   -> t
      BaseType UnitT     -> t
      BaseType BoolT     -> t
      BaseType DateTimeT -> t
      BaseType (ArrayT a) -> BaseType $ ArrayT  (go a)
      BaseType (MapT a b)  -> BaseType $ MapT    (go a) (go b)
      BaseType (OptionT a) -> BaseType $ OptionT (go a)
      BaseType (PairT a b) -> BaseType $ PairT   (go a) (go b)
      BaseType (StructT st) -> BaseType $ StructT (StructType $ Map.map go $ getStructType st)
      BaseType StringT   -> t
      BaseTypeVar v
       | Just t' <- Map.lookup v s
       -> t'
       | otherwise
       -> t
      BaseTypeExistential _
       -> t


