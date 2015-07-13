{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Icicle.Source.ToCore.Prim (
    convertPrim
  ) where

import                  Icicle.Source.Query
import                  Icicle.Source.ToCore.Base
import                  Icicle.Source.Type

import qualified        Icicle.Core as C
import qualified        Icicle.Core.Exp.Combinators as CE
import qualified        Icicle.Common.Exp           as CE

import qualified        Icicle.Common.Exp.Prim.Minimal as Min

import                  P


-- | Convert a primitive application.
-- All the arguments are already converted, so we just need to
-- look up the new primitive and apply the converted arguments.
--
-- This only works for *expression* primitives, not aggregates.
-- Aggregates must be handled earlier.
-- If they show up here it means a type error, for example
-- an aggregate in a filter expression:
--
-- > filter sum(value) > 5
--
-- is ill typed.
convertPrim
        :: Prim -> a
        -> [(C.Exp n, UniverseType)]
        -> ConvertM a n (C.Exp n)
convertPrim p ann xts
 = do   p' <- go p
        return $ CE.makeApps p' $ fmap fst xts
 where

  go (Op o)
   = (CE.XPrim . C.PrimMinimal) <$> goop o
  go (Lit (LitInt i))
   = return $ CE.constI i
  go (Agg agg)
   = convertError
   $ ConvertErrorPrimAggregateNotAllowedHere ann agg

  goop Add
   = return $ Min.PrimArith Min.PrimArithPlus
  goop Sub
   = return $ Min.PrimArith Min.PrimArithMinus
  goop Div
   = return $ Min.PrimArith Min.PrimArithDiv
  goop Mul
   = return $ Min.PrimArith Min.PrimArithMul
  goop Negate
   = return $ Min.PrimArith Min.PrimArithNegate
  goop Gt
   = Min.PrimRelation Min.PrimRelationGt <$> t1 2
  goop Ge
   = Min.PrimRelation Min.PrimRelationGe <$> t1 2
  goop Lt
   = Min.PrimRelation Min.PrimRelationLt <$> t1 2
  goop Le
   = Min.PrimRelation Min.PrimRelationLe <$> t1 2
  goop Eq
   = Min.PrimRelation Min.PrimRelationEq <$> t1 2
  goop Ne
   = Min.PrimRelation Min.PrimRelationNe <$> t1 2
  goop TupleComma
   | [(_,a),(_,b)] <- xts
   = return $ Min.PrimConst $ Min.PrimConstPair (baseType a) (baseType b)
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p


  t1 num_args
   = case xts of
      ((_,tt):_) -> return
                  $ baseType tt
      []         -> convertError
                  $ ConvertErrorPrimNoArguments ann num_args p


