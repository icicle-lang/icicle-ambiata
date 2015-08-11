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
import qualified        Icicle.Common.Base          as V
import qualified        Icicle.Common.Exp           as CE
import qualified        Icicle.Common.Type          as T

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
        -> [(C.Exp n, Type n)]
        -> ConvertM a n (C.Exp n)
convertPrim p ann xts
 = do   p' <- go p
        return $ CE.makeApps p' $ fmap fst xts
 where

  go (Op o)
   = (CE.XPrim . C.PrimMinimal) <$> goop o
  go (Lit (LitInt i))
   = return $ CE.constI i
  go (Lit (LitDouble i))
   = return $ CE.XValue T.DoubleT (V.VDouble i)
  go (Lit (LitString i))
   = return $ CE.XValue T.StringT (V.VString i)

  go (Fun f)
   = (CE.XPrim . C.PrimMinimal) <$> gofun f

  goop (ArithUnary Negate)
   = Min.PrimArithUnary Min.PrimArithNegate <$> tArithArg 1

  goop (ArithBinary Add)
   = Min.PrimArithBinary Min.PrimArithPlus <$> tArithArg 2
  goop (ArithBinary Sub)
   = Min.PrimArithBinary Min.PrimArithMinus <$> tArithArg 2
  goop (ArithBinary Mul)
   = Min.PrimArithBinary Min.PrimArithMul <$> tArithArg 2
  goop (ArithBinary Pow)
   = Min.PrimArithBinary Min.PrimArithPow <$> tArithArg 2

  goop (ArithDouble Div)
   = return $ Min.PrimDouble Min.PrimDoubleDiv

  goop (Relation Gt)
   = Min.PrimRelation Min.PrimRelationGt <$> t1 2
  goop (Relation Ge)
   = Min.PrimRelation Min.PrimRelationGe <$> t1 2
  goop (Relation Lt)
   = Min.PrimRelation Min.PrimRelationLt <$> t1 2
  goop (Relation Le)
   = Min.PrimRelation Min.PrimRelationLe <$> t1 2
  goop (Relation Eq)
   = Min.PrimRelation Min.PrimRelationEq <$> t1 2
  goop (Relation Ne)
   = Min.PrimRelation Min.PrimRelationNe <$> t1 2

  goop TupleComma
   | [(_,a),(_,b)] <- xts
   = do a' <- convertValType ann a
        b' <- convertValType ann b
        return $ Min.PrimConst $ Min.PrimConstPair a' b'

   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p

  gofun Log
   = return $ Min.PrimDouble Min.PrimDoubleLog
  gofun Exp
   = return $ Min.PrimDouble Min.PrimDoubleExp
  gofun ToDouble
   = return $ Min.PrimCast Min.PrimCastDoubleOfInt
  gofun ToInt
   = return $ Min.PrimCast Min.PrimCastIntOfDouble

  t1 num_args
   = case xts of
      ((_,tt):_) -> convertValType ann tt
      []         -> convertError
                  $ ConvertErrorPrimNoArguments ann num_args p

  tArithArg num_args
   = do t' <- t1 num_args
        case T.arithTypeOfValType t' of
         Nothing -> convertError $ ConvertErrorPrimNoArguments ann num_args p
         Just a' -> return a'

