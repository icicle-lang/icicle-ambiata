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
        -> Type n
        -> [(C.Exp () n, Type n)]
        -> ConvertM a n (C.Exp () n)
convertPrim p ann resT xts
 = go p
 where

  args = fmap fst xts
  applies p' = CE.makeApps () p' args

  primmin p' = applies $ CE.XPrim () $ C.PrimMinimal p'

  go (Lit (LitInt i))
   | (_, _, DoubleT) <- decomposeT resT
   = return $ CE.XValue () T.DoubleT (V.VDouble $ fromIntegral i)
   | otherwise
   = return $ CE.constI i
  go (Lit (LitDouble i))
   = return $ CE.XValue () T.DoubleT (V.VDouble i)
  go (Lit (LitString i))
   = return $ CE.XValue () T.StringT (V.VString i)

  go (PrimCon ConSome)
   = primmin <$> (Min.PrimConst <$> (Min.PrimConstSome <$> t1 1))
  go (PrimCon ConNone)
   = flip (CE.XValue ()) V.VNone <$> convertValType ann resT
  go (PrimCon ConTuple)
   = go $ Op TupleComma
  go (PrimCon ConTrue)
   = return $ CE.XValue () T.BoolT $ V.VBool True
  go (PrimCon ConFalse)
   = return $ CE.XValue () T.BoolT $ V.VBool False
  go (PrimCon ConLeft)
   | (_, _, SumT a b) <- decomposeT resT
   = do a' <- convertValType ann a
        b' <- convertValType ann b
        return $ primmin $ Min.PrimConst $ Min.PrimConstLeft a' b'
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p
  go (PrimCon ConRight)
   | (_, _, SumT a b) <- decomposeT resT
   = do a' <- convertValType ann a
        b' <- convertValType ann b
        return $ primmin $ Min.PrimConst $ Min.PrimConstRight a' b'
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p
  go (PrimCon (ConError err))
   = return $ CE.XValue () T.ErrorT $ V.VError err


  go (Fun f)
   = gofun f
  go (Op o)
   = goop o

  goop (ArithUnary Negate)
   = primmin <$> (Min.PrimArithUnary Min.PrimArithNegate <$> tArithArg 1)

  goop (ArithBinary Add)
   = primmin <$> (Min.PrimArithBinary Min.PrimArithPlus <$> tArithArg 2)
  goop (ArithBinary Sub)
   = primmin <$> (Min.PrimArithBinary Min.PrimArithMinus <$> tArithArg 2)
  goop (ArithBinary Mul)
   = primmin <$> (Min.PrimArithBinary Min.PrimArithMul <$> tArithArg 2)
  goop (ArithBinary Pow)
   = primmin <$> (Min.PrimArithBinary Min.PrimArithPow <$> tArithArg 2)

  goop (ArithDouble Div)
   = return $ primmin $ Min.PrimDouble Min.PrimDoubleDiv

  goop (LogicalUnary Not)
   = return $ primmin $ Min.PrimLogical Min.PrimLogicalNot

  goop (LogicalBinary And)
   = return $ primmin $ Min.PrimLogical Min.PrimLogicalAnd
  goop (LogicalBinary Or)
   = return $ primmin $ Min.PrimLogical Min.PrimLogicalOr
  goop (DateBinary DaysBefore)
   | [(a,_),(b,_)] <- xts
   = return (CE.xPrim (C.PrimMinimal $ Min.PrimDateTime Min.PrimDateTimeMinusDays) CE.@~ b CE.@~ a)
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p
  goop (DateBinary WeeksBefore)
   | [(a,_),(b,_)] <- xts
   = return (CE.xPrim (C.PrimMinimal $ Min.PrimDateTime Min.PrimDateTimeMinusDays) CE.@~ b CE.@~ (CE.constI 7 CE.*~ a))
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p
  goop (DateBinary MonthsBefore)
   | [(a,_),(b,_)] <- xts
   = return (CE.xPrim (C.PrimMinimal $ Min.PrimDateTime Min.PrimDateTimeMinusMonths) CE.@~ b CE.@~ a)
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p
  goop (DateBinary DaysAfter)
   | [(a,_),(b,_)] <- xts
   = return (CE.xPrim (C.PrimMinimal $ Min.PrimDateTime Min.PrimDateTimeMinusDays) CE.@~ b CE.@~ (CE.negate a))
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p
  goop (DateBinary WeeksAfter)
   | [(a,_),(b,_)] <- xts
   = return (CE.xPrim (C.PrimMinimal $ Min.PrimDateTime Min.PrimDateTimeMinusDays) CE.@~ b CE.@~ (CE.negate (CE.constI 7 CE.*~ a)))
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p
  goop (DateBinary MonthsAfter)
   | [(a,_),(b,_)] <- xts
   = return (CE.xPrim (C.PrimMinimal $ Min.PrimDateTime Min.PrimDateTimeMinusMonths) CE.@~ b CE.@~ (CE.negate a))
   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p
  goop (Relation Gt)
   = primmin <$> (Min.PrimRelation Min.PrimRelationGt <$> t1 2)
  goop (Relation Ge)
   = primmin <$> (Min.PrimRelation Min.PrimRelationGe <$> t1 2)
  goop (Relation Lt)
   = primmin <$> (Min.PrimRelation Min.PrimRelationLt <$> t1 2)
  goop (Relation Le)
   = primmin <$> (Min.PrimRelation Min.PrimRelationLe <$> t1 2)
  goop (Relation Eq)
   = primmin <$> (Min.PrimRelation Min.PrimRelationEq <$> t1 2)
  goop (Relation Ne)
   = primmin <$> (Min.PrimRelation Min.PrimRelationNe <$> t1 2)

  goop TupleComma
   | [(_,a),(_,b)] <- xts
   = do a' <- convertValType ann a
        b' <- convertValType ann b
        return $ primmin $ Min.PrimConst $ Min.PrimConstPair a' b'

   | otherwise
   = convertError
   $ ConvertErrorPrimNoArguments ann 2 p

  gofun Log
   = return $ primmin $ Min.PrimDouble Min.PrimDoubleLog
  gofun Exp
   = return $ primmin $ Min.PrimDouble Min.PrimDoubleExp
  gofun ToDouble
   = case xts of
      ((xx,tt):_)
       | (_, _, DoubleT) <- decomposeT tt
       -> return xx
      _
       -> return $ primmin $ Min.PrimCast Min.PrimCastDoubleOfInt
  gofun ToInt
   = case xts of
      ((xx,tt):_)
       | (_, _, IntT) <- decomposeT tt
       -> return xx
      _
       -> return $ primmin $ Min.PrimCast Min.PrimCastIntOfDouble
  gofun DaysBetween
   = return $ primmin $ Min.PrimDateTime Min.PrimDateTimeDaysDifference

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

