-- | Types of primitives.
-- It is odd that we need to look up inside a Fresh monad.
-- However, because the types are polymorphic in the variable type, we have no way of saying "forall a".
-- So we must generate a fresh name for any forall binders.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Icicle.Source.Query.Prim (
    primLookup'
  ) where

import                  Icicle.Source.Query.Constructor
import                  Icicle.Source.Query.Exp
import                  Icicle.Source.Query.Operators
import                  Icicle.Source.Type

import qualified        Icicle.Common.Fresh         as Fresh

import                  P

import                  Data.Hashable (Hashable)


primLookup' :: Hashable n => Prim -> Fresh.Fresh n (FunctionType n)
primLookup' p
 = case p of
    Op (ArithUnary _)
     -> fNum $ \at -> ([at], at)
    Op (ArithBinary _)
     -> fNum $ \at -> ([at, at], at)
    Op (ArithDouble Div)
     -> f0 [DoubleT, DoubleT] DoubleT

    Op (Relation _)
     -> f1 $ \a at -> FunctionType [a] [] [at, at] BoolT

    Op (LogicalUnary _)
     -> f0 [BoolT] BoolT
    Op (LogicalBinary _)
     -> f0 [BoolT, BoolT] BoolT
    Op (TimeBinary _)
     -> f0 [IntT, TimeT] TimeT

    Op  TupleComma
     -> do a <- Fresh.fresh
           b <- Fresh.fresh
           let at = TypeVar a
           let bt = TypeVar b
           return $ FunctionType [a,b] [] [at, bt] (PairT at bt)

    Lit (LitInt _)
     -> fNum $ \at -> ([], at)
    Lit (LitDouble _)
     -> f0 [] DoubleT
    Lit (LitString _)
     -> f0 [] StringT
    Lit (LitTime _)
     -> f0 [] TimeT

    Fun (BuiltinMath Log)
     -> f0 [DoubleT] DoubleT
    Fun (BuiltinMath Exp)
     -> f0 [DoubleT] DoubleT
    Fun (BuiltinMath Sqrt)
     -> f0 [DoubleT] DoubleT
    Fun (BuiltinMath ToDouble)
     -> fNum $ \at -> ([at], DoubleT)
    Fun (BuiltinMath Abs)
     -> fNum $ \at -> ([at], at)
    Fun (BuiltinMath Floor)
     -> fNum $ \at -> ([at], IntT)
    Fun (BuiltinMath Ceiling)
     -> fNum $ \at -> ([at], IntT)
    Fun (BuiltinMath Round)
     -> fNum $ \at -> ([at], IntT)
    Fun (BuiltinMath Truncate)
     -> fNum $ \at -> ([at], IntT)

    Fun (BuiltinTime DaysBetween)
     -> f0 [TimeT, TimeT] IntT
    Fun (BuiltinTime DaysJulianEpoch)
     -> f0 [TimeT] IntT
    Fun (BuiltinTime SecondsBetween)
     -> f0 [TimeT, TimeT] IntT
    Fun (BuiltinTime SecondsJulianEpoch)
     -> f0 [TimeT] IntT
    Fun (BuiltinTime ProjectDay)
     -> f0 [TimeT] IntT
    Fun (BuiltinTime ProjectMonth)
     -> f0 [TimeT] IntT
    Fun (BuiltinTime ProjectYear)
     -> f0 [TimeT] IntT

    Fun (BuiltinData Seq)
     -> f2 $ \a at b bt -> FunctionType [a,b] [] [at,bt] bt
    Fun (BuiltinData Box)
     -> f1 $ \a at -> FunctionType [a] [] [OptionT at] (Possibility PossibilityPossibly at)

    Fun (BuiltinArray ArraySort)
     -> f1 $ \a at -> FunctionType [a] [] [ArrayT at] (ArrayT at)
    Fun (BuiltinArray ArrayLength)
     -> f1 $ \a at -> FunctionType [a] [] [ArrayT at] IntT
    Fun (BuiltinArray ArrayIndex)
     -> f1 $ \a at -> FunctionType [a] [] [ArrayT at, IntT] at

    Fun (BuiltinMap MapKeys)
     -> f2 $ \a at b bt -> FunctionType [a,b] [] [GroupT at bt] (ArrayT at)
    Fun (BuiltinMap MapValues)
     -> f2 $ \a at b bt -> FunctionType [a,b] [] [GroupT at bt] (ArrayT bt)
    Fun (BuiltinMap MapCreate)
     -> f2 $ \k kt v vt -> FunctionType [k, v] [] [] (GroupT kt vt)
    Fun (BuiltinMap MapInsert)
     -> f2 $ \k kt v vt -> FunctionType [k, v] [] [kt, vt, GroupT kt vt] (GroupT kt vt)
    Fun (BuiltinMap MapDelete)
     -> f2 $ \k kt v vt -> FunctionType [k, v] [] [kt, GroupT kt vt] (GroupT kt vt)
    Fun (BuiltinMap MapLookup)
     -> f2 $ \k kt v vt -> FunctionType [k, v] [] [kt, GroupT kt vt] (OptionT vt)

    PrimCon ConSome
     -> f1 $ \a at -> FunctionType [a] [] [at] (OptionT at)

    PrimCon ConNone
     -> f1 $ \a at -> FunctionType [a] [] [] (OptionT at)

    PrimCon ConTuple
     -> f2 $ \a at b bt -> FunctionType [a, b] [] [at, bt] (PairT at bt)

    PrimCon ConTrue
     -> f0 [] BoolT
    PrimCon ConFalse
     -> f0 [] BoolT

    PrimCon ConLeft
     -> f2 $ \a at b bt -> FunctionType [a, b] [] [at] (SumT at bt)
    PrimCon ConRight
     -> f2 $ \a at b bt -> FunctionType [a, b] [] [bt] (SumT at bt)

    PrimCon (ConError _)
     -> f0 [] ErrorT

 where

  f0 argsT resT
   = return $ FunctionType [] [] argsT resT

  fNum f
   = f1 (\a at -> uncurry (FunctionType [a] [CIsNum at]) (f at))

  f1 f
   = do n <- Fresh.fresh
        return $ f n (TypeVar n)

  f2 f
   = do n1 <- Fresh.fresh
        n2 <- Fresh.fresh
        return $ f n1 (TypeVar n1) n2 (TypeVar n2)
