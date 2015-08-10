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
  , substFT
  , compose
  , unifyT
  , canonT
  , decomposeT
  , recomposeT
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


-- | Substitute into a function type.
-- It is very important that any binders (foralls) in here are unique.
-- If the binders mention any names in the substitution, things WILL go awry.
-- This is not a problem for type checking since all names are fresh.
--
-- Because the function constraints will only mention newly bound type variables,
-- it is not necessary to substitute into the constraints.
--
-- Perhaps this should actually be called "unsafeSubstFT" because none of these
-- invariants are checked.
substFT :: Ord n => SubstT n -> FunctionType n -> FunctionType n
substFT ss ff
 = ff
 { functionArguments = fmap (substT ss) (functionArguments ff)
 , functionReturn    =       substT ss  (functionReturn    ff) }


-- | Compose two substitutions together, in order.
-- `compose s1 s2` performs s1 then s2.
--
-- If the same name is mentioned in both, s1 takes priority since it happens first:
--
-- > compose {a := T} {a := U}
-- > = {a := T}
--
-- Although if the name is mentioned in the result, s2 must apply to the results
--
-- > compose {a := T a b} {a := U}
-- > = {a := T U b}
--
-- TODO: a quickcheck property would be ideal for this
--
-- > forall (t : Type n) (s1 s2 : Subst n),
-- > substT s2 (substT s1 t) = substT (compose s1 s2) t
--
-- If the substitutions don't mention the same variables at all,
-- the ordering should not matter.
--
-- > fv s1  \cap fv s1 == {}
-- > ==> compose s1 s2 == compose s2 s1
--
compose :: Ord n => SubstT n -> SubstT n -> SubstT n
compose s1 s2
 = Map.map (substT s2) s1
  `Map.union` s2


-- | Attempt to find a substitution that makes the two types equal.
--
-- >     unifyT t1 t2 == Just   s
-- > ==> substT s  t1 == substT s  t2
--
-- >     unifyT t1 t2 == Nothing
-- > ==>           t1 /= t2
--
unifyT :: Ord n => Type n -> Type n -> Maybe (SubstT n)
unifyT t1 t2
 = case t1 of
    TypeVar a
     -> return $ Map.singleton a t2
    _
     | TypeVar b <- t2
     -> return $ Map.singleton b t1

    BoolT       -> eq
    DateTimeT   -> eq
    DoubleT     -> eq
    IntT        -> eq
    StringT     -> eq
    UnitT       -> eq

    ArrayT a
     | ArrayT b <- t2
     -> unifyT a b
     | otherwise
     -> Nothing

    GroupT ak av
     | GroupT bk bv <- t2
     -> compose <$> unifyT ak bk <*> unifyT av bv
     | otherwise
     -> Nothing

    OptionT a
     | OptionT b <- t2
     -> unifyT a b
     | otherwise
     -> Nothing

    PairT a1 a2
     | PairT b1 b2 <- t2
     -> compose <$> unifyT a1 b1 <*> unifyT a2 b2
     | otherwise
     -> Nothing

    StructT as
     | StructT bs <- t2
     , Map.keysSet as == Map.keysSet bs
     , m' <- Map.intersectionWith (,) as bs
     ->  foldl compose Map.empty
     <$> mapM (uncurry unifyT) m'
     | otherwise
     -> Nothing

    Temporality at ar
     | Temporality bt br <- t2
     -> compose <$> unifyT at bt <*> unifyT ar br
     | otherwise
     -> Nothing

    TemporalityPure         -> eq
    TemporalityElement      -> eq
    TemporalityAggregate    -> eq

    Possibility at ar
     | Possibility bt br <- t2
     -> compose <$> unifyT at bt <*> unifyT ar br
     | otherwise
     -> Nothing

    PossibilityPossibly     -> eq
    PossibilityDefinitely   -> eq

 where
  eq
   | t1 == t2
   = Just Map.empty
   | otherwise
   = Nothing



canonT :: Type n -> Type n
canonT
 = recomposeT . decomposeT


decomposeT :: Type n -> (Maybe (Type n), Maybe (Type n), Type n)
decomposeT t
 = let can  = canonT t
       tmp  = getTemporality can
       tmpR = maybe can snd tmp
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
