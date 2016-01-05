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
  ) where


import                  Icicle.Common.Base
import                  Icicle.Source.Type.Base
import                  Icicle.Source.Type.Compounds

import                  P

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
      TimeT         -> t
      DoubleT       -> t
      IntT          -> t
      StringT       -> t
      UnitT         -> t
      ErrorT        -> t

      ArrayT  a     -> ArrayT  (go a)
      GroupT  a b   -> GroupT  (go a) (go b)
      OptionT a     -> OptionT (go a)
      PairT   a b   -> PairT   (go a) (go b)
      SumT    a b   -> SumT    (go a) (go b)
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
    CTemporalityJoin a b c
     -> CTemporalityJoin (substT ss a) (substT ss b) (substT ss c)
    CReturnOfLetTemporalities ret def body
     -> CReturnOfLetTemporalities (substT ss ret) (substT ss def) (substT ss body)
    CDataOfLatest ret tmp pos dat
     -> CDataOfLatest (substT ss ret) (substT ss tmp) (substT ss pos) (substT ss dat)
    CPossibilityOfLatest ret tmp pos
     -> CPossibilityOfLatest (substT ss ret) (substT ss tmp) (substT ss pos)
    CPossibilityJoin ret b c
     -> CPossibilityJoin (substT ss ret) (substT ss b) (substT ss c)


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
 { functionConstraints  = fmap (substC ss') (functionConstraints ff)
 , functionArguments    = fmap (substT ss') (functionArguments   ff)
 , functionReturn       =       substT ss'  (functionReturn      ff) }
 where
  ss' = foldl (flip Map.delete) ss
      $ functionForalls ff


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
     | TypeVar b <- t2
     , a == b
     -> return $ Map.empty
    TypeVar a
     -> return $ Map.singleton a t2
    _
     | TypeVar b <- t2
     -> return $ Map.singleton b t1

    BoolT       -> eq
    TimeT       -> eq
    DoubleT     -> eq
    IntT        -> eq
    StringT     -> eq
    UnitT       -> eq
    ErrorT      -> eq

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

    SumT  a1 a2
     | SumT  b1 b2 <- t2
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

