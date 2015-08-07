-- | Substitution into types.
-- This is very simple because there are no binders inside actual types
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Subst (
    SubstT
  , substT
  , substC
  , canonT
  , getTemporality
  , getPossibility
  ) where


import                  Icicle.Common.Base
import                  Icicle.Source.Type.Base

import                  P

import                  Data.List (zipWith, zip)
import qualified        Data.Map as Map

type SubstT n = Map.Map (Name n) (Type n)

substT :: Ord n => SubstT n -> Type n -> Type n
substT ss tt
 = canonT
 $ go tt
 where

  go t
   = case t of
      BoolT         -> t
      DateTimeT     -> t
      DoubleT       -> t
      IntT          -> t
      StringT       -> t
      UnitT         -> t

      ArrayT  a     -> ArrayT  (go a)
      GroupT  a b   -> GroupT  (go a) (go b)
      OptionT a     -> OptionT (go a)
      PairT   a b   -> PairT   (go a) (go b)
      StructT fs    -> StructT (Map.map go fs)

      Temporality a b       -> Temporality (go a) (go b)
      TemporalityPure       -> t
      TemporalityElement    -> t
      TemporalityAggregate  -> t

      Possibility a b       -> Possibility (go a) (go b)
      PossibilityPossibly   -> t
      PossibilityDefinitely -> t

      TypeVar n
       | Just t' <- Map.lookup n ss
       -> t'
       | otherwise
       -> t


substC :: Ord n => SubstT n -> Constraint n -> Constraint n
substC ss cc
 = case cc of
    CEquals p q
     -> CEquals (substT ss p) (substT ss q)
    CIsNum p
     -> CIsNum (substT ss p)


canonT :: Type n -> Type n
canonT tt
 = temper
 $ posten tt
 where
  temper t
   = case getTemporality t of
      Just (a,b) -> Temporality a b
      Nothing    -> Temporality TemporalityPure t

  posten t
   = case getPossibility t of
      Just (a,b) -> Possibility a b
      Nothing    -> Possibility PossibilityDefinitely t


getTemporality :: Type n -> Maybe (Type n, Type n)
getTemporality tt
 = case tt of
    BoolT         -> Nothing
    DateTimeT     -> Nothing
    DoubleT       -> Nothing
    IntT          -> Nothing
    StringT       -> Nothing
    UnitT         -> Nothing

    ArrayT  a     -> wrap go ArrayT  a
    GroupT  a b   -> wrap2 go GroupT a b
    OptionT a     -> wrap go OptionT a
    PairT   a b   -> wrap2 go PairT  a b
    StructT fs    -> let fs' = Map.toList fs
                         ks  = fmap fst fs'
                         vs  = fmap snd fs'
                     in  wrapN go (\t vs' -> Just (t, StructT $ Map.fromList (ks `zip` vs'))) vs

    Temporality a b
     -> case go b of
         Just (_,b') -> Just (a, b')
         Nothing     -> Just (a, b)

    TemporalityPure       -> Nothing
    TemporalityElement    -> Nothing
    TemporalityAggregate  -> Nothing

    Possibility _ _       -> Nothing
    PossibilityPossibly   -> Nothing
    PossibilityDefinitely -> Nothing

    TypeVar _             -> Nothing

 where
  go = getTemporality


getPossibility :: Type n -> Maybe (Type n, Type n)
getPossibility tt
 = case tt of
    BoolT         -> Nothing
    DateTimeT     -> Nothing
    DoubleT       -> Nothing
    IntT          -> Nothing
    StringT       -> Nothing
    UnitT         -> Nothing

    ArrayT  a     -> wrap go ArrayT  a
    GroupT  a b   -> wrap2 go GroupT a b
    OptionT a     -> wrap go OptionT a
    PairT   a b   -> wrap2 go PairT  a b
    StructT fs    -> let fs' = Map.toList fs
                         ks  = fmap fst fs'
                         vs  = fmap snd fs'
                     in  wrapN go (\t vs' -> Just (t, StructT $ Map.fromList (ks `zip` vs'))) vs

    Temporality _ _       -> Nothing
    TemporalityPure       -> Nothing
    TemporalityElement    -> Nothing
    TemporalityAggregate  -> Nothing

    Possibility a b
     -> case go b of
         Just (_,b') -> Just (a, b')
         Nothing     -> Just (a, b)

    PossibilityPossibly   -> Nothing
    PossibilityDefinitely -> Nothing

    TypeVar _             -> Nothing

 where
  go = getPossibility



-- Temporality and possibility helpers --

wrap :: (Type n -> Maybe (Type n, Type n)) -> (Type n -> Type n) -> Type n -> Maybe (Type n, Type n)
wrap go f a
 = case go a of
    Just (tmp,typ) -> Just (tmp, f typ)
    Nothing        -> Nothing

wrap2 :: (Type n -> Maybe (Type n, Type n)) -> (Type n -> Type n -> Type n) -> Type n -> Type n -> Maybe (Type n, Type n)
wrap2 go f a b
 = wrapN go (wrap2' f) [a,b]

wrap2' :: (Type n -> Type n -> Type n) -> Type n -> [Type n] -> Maybe (Type n, Type n)
wrap2' f t xs
 = case xs of
    [a',b'] -> Just (t, f a' b')
    _       -> Nothing

wrapN :: (Type n -> Maybe (Type n, Type n)) -> (Type n -> [Type n] -> Maybe (Type n, Type n)) -> [Type n] -> Maybe (Type n, Type n)
wrapN go f ts
 = let res = fmap go ts
       tmp  = listToMaybe $ fmap fst $ catMaybes res
       args = zipWith (\t r -> fromMaybe t (snd <$> r)) ts res
   in case tmp of
       Nothing -> Nothing
       Just tmp' -> f tmp' args
