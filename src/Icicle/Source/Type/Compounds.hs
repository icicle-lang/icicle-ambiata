-- | Helpers on Types
--
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Source.Type.Compounds (
    function0
  , freeT
  , canonT
  , decomposeT
  , recomposeT
  , getTemporality
  , getPossibility
  , getTemporalityOrPure
  ) where


import                  Icicle.Common.Base
import                  Icicle.Source.Type.Base

import                  P

import                  Data.List (zipWith, zip)
import qualified        Data.Map as Map
import qualified        Data.Set as Set


function0 :: Type n -> FunctionType n
function0 u
 = FunctionType [] [] [] u

freeT :: Ord n => Type n -> Set.Set (Name n)
freeT t
 = case t of
    BoolT                   -> Set.empty
    DateTimeT               -> Set.empty
    DoubleT                 -> Set.empty
    IntT                    -> Set.empty
    StringT                 -> Set.empty
    UnitT                   -> Set.empty

    ArrayT      a           -> freeT a
    GroupT      a b         -> Set.union (freeT a) (freeT b)
    OptionT     a           -> freeT a
    PairT       a b         -> Set.union (freeT a) (freeT b)
    StructT ms              -> Set.unions
                             $ fmap freeT
                             $ Map.elems ms

    Temporality a b         -> Set.union (freeT a) (freeT b)
    TemporalityPure         -> Set.empty
    TemporalityElement      -> Set.empty
    TemporalityAggregate    -> Set.empty

    Possibility a b         -> Set.union (freeT a) (freeT b)
    PossibilityPossibly     -> Set.empty
    PossibilityDefinitely   -> Set.empty

    TypeVar n               -> Set.singleton n


canonT :: Type n -> Type n
canonT
 = recomposeT . decomposeT


decomposeT :: Type n -> (Maybe (Type n), Maybe (Type n), Type n)
decomposeT t
 = let tmp  = getTemporality t
       tmpR = maybe t snd tmp
       pos  = getPossibility tmpR
       posR = maybe tmpR snd pos
   in (fst <$> tmp, fst <$> pos, posR)

recomposeT :: (Maybe (Type n), Maybe (Type n), Type n) -> Type n
recomposeT (tmp,pos,dat)
 = maybe id Temporality tmp
 $ maybe id Possibility pos
 $ dat


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

getTemporalityOrPure :: Type n -> Type n
getTemporalityOrPure t
 = case getTemporality t of
    Just (a,_) -> a
    Nothing -> TemporalityPure


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

