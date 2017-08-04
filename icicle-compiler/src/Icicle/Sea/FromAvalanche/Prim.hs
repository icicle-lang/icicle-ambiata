{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Prim (
    seaOfXPrim
  , seaOfPrimDocApps
  , PrimDoc (..)
  ) where

import qualified Data.List as List

import           Icicle.Avalanche.Prim.Flat

import qualified Icicle.Common.Exp.Prim.Minimal as M
import           Icicle.Common.Type

import           Icicle.Internal.Pretty

import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Type

import           P

data ArgConv
 = ArgVal
 | ArgRef

data PrimDoc
 = PDFun   Doc (Maybe [ArgConv])
 | PDAlloc Doc (Maybe [ArgConv])

seaOfPrimDocApps :: PrimDoc -> [Doc] -> Doc
seaOfPrimDocApps p xs
 = case p of
    PDFun   d cs -> d <+> seaOfPrimArgs (go cs) xs
    PDAlloc d cs -> d <+> seaOfPrimArgs (ArgVal : go cs) ("mempool" : xs)
 where
  go Nothing   = List.cycle [ArgVal]
  go (Just cs) = cs

seaOfPrimArgs :: [ArgConv] -> [Doc] -> Doc
seaOfPrimArgs cs xs
 = tuple (List.zipWith go cs xs)
 where
  go ArgVal x = x
  go ArgRef x = "&" <> x

seaOfXPrim :: Prim -> PrimDoc
seaOfXPrim p
 = case p of
     PrimMinimal (M.PrimArithUnary op t)
      -> PDFun
       ( prefixOfArithType t <> seaOfPrimArithUnary op ) Nothing

     PrimMinimal (M.PrimArithBinary op t)
      -> PDFun
       ( prefixOfArithType t <> seaOfPrimArithBinary op ) Nothing

     PrimMinimal (M.PrimBuiltinFun (M.PrimBuiltinMath fun))
      -> PDFun (seaOfPrimBuiltinMath fun) Nothing

     PrimMinimal (M.PrimTime op)
      -> PDFun
       ( prefixOfValType TimeT <> seaOfPrimTime op ) Nothing

     PrimMinimal (M.PrimRelation op t)
      -> PDFun
       ( prefixOfValType t <> seaOfPrimRelation op ) Nothing

     PrimMinimal (M.PrimLogical op)
      -> PDFun
       ( prefixOfValType BoolT <> seaOfPrimLogical op ) Nothing

     PrimProject op
      -> PDFun (seaOfPrimProject op) Nothing

     PrimUnsafe op
      -> seaOfPrimUnsafe op

     PrimArray op
      -> seaOfPrimArray op

     PrimBuf   op
      -> seaOfPrimBuf op

     _
      -> PDFun (seaError "seaOfXPrim" p) Nothing

seaOfPrimArithUnary :: M.PrimArithUnary -> Doc
seaOfPrimArithUnary p
 = case p of
     M.PrimArithNegate   -> "neg"
     M.PrimArithAbsolute -> "abs"

seaOfPrimArithBinary :: M.PrimArithBinary -> Doc
seaOfPrimArithBinary p
 = case p of
     M.PrimArithPlus  -> "add"
     M.PrimArithMinus -> "sub"
     M.PrimArithMul   -> "mul"

seaOfPrimTime :: M.PrimTime -> Doc
seaOfPrimTime p
 = case p of
     M.PrimTimeDaysDifference     -> "days_diff"
     M.PrimTimeSecondsDifference  -> "seconds_diff"
     M.PrimTimeDaysJulianEpoch    -> "to_epoch_days"
     M.PrimTimeSecondsJulianEpoch -> "to_epoch_seconds"
     M.PrimTimeMinusSeconds       -> "minus_seconds"
     M.PrimTimeMinusDays          -> "minus_days"
     M.PrimTimeMinusMonths        -> "minus_months"
     M.PrimTimeProjectDay         -> "day_of"
     M.PrimTimeProjectMonth       -> "month_of"
     M.PrimTimeProjectYear        -> "year_of"

seaOfPrimBuiltinMath :: M.PrimBuiltinMath -> Doc
seaOfPrimBuiltinMath p = case p of
  M.PrimBuiltinDiv             -> idouble <> "div"
  M.PrimBuiltinPow             -> idouble <> "pow"
  M.PrimBuiltinLog             -> idouble <> "log"
  M.PrimBuiltinExp             -> idouble <> "exp"
  M.PrimBuiltinSqrt            -> idouble <> "sqrt"
  M.PrimBuiltinFloor           -> "idouble_floor"
  M.PrimBuiltinCeiling         -> "idouble_ceil"
  M.PrimBuiltinRound           -> "idouble_round"
  M.PrimBuiltinTruncate        -> "idouble_trunc"
  M.PrimBuiltinToDoubleFromInt -> "iint_extend"
  M.PrimBuiltinDoubleIsValid   -> "idouble_is_valid"

seaOfPrimRelation :: M.PrimRelation -> Doc
seaOfPrimRelation p
 = case p of
     M.PrimRelationGt -> "gt"
     M.PrimRelationGe -> "ge"
     M.PrimRelationLt -> "lt"
     M.PrimRelationLe -> "le"
     M.PrimRelationEq -> "eq"
     M.PrimRelationNe -> "ne"

seaOfPrimLogical :: M.PrimLogical -> Doc
seaOfPrimLogical p
 = case p of
     M.PrimLogicalAnd -> "and"
     M.PrimLogicalOr  -> "or"
     M.PrimLogicalNot -> "not"

seaOfPrimProject :: PrimProject -> Doc
seaOfPrimProject p
 = case p of
     PrimProjectArrayLength t
      -> prefixOfValType (ArrayT t) <> "length"
     _
      -> seaError "seaOfPrimProject" p

seaOfPrimUnsafe :: PrimUnsafe -> PrimDoc
seaOfPrimUnsafe p
 = case p of
     PrimUnsafeArrayIndex t
      -> PDFun   (prefixOfValType (ArrayT t) <> "index") Nothing
     PrimUnsafeArrayCreate t
      -> PDAlloc (prefixOfValType (ArrayT t) <> "create") Nothing
     _
      -> PDFun   (seaError "seaOfPrimUnsafe" p) Nothing

seaOfPrimArray :: PrimArray -> PrimDoc
seaOfPrimArray p
 = case p of
     PrimArrayPutMutable t
      -> PDAlloc (prefixOfValType (ArrayT t) <> "put_mutable") Nothing
     PrimArrayPutImmutable t
      -> PDAlloc (prefixOfValType (ArrayT t) <> "put_immutable") Nothing
     PrimArraySwap t
      -> PDFun   (prefixOfValType (ArrayT t) <> "swap") Nothing
     PrimArrayDel t
      -> PDAlloc   (prefixOfValType (ArrayT t) <> "delete") Nothing
     PrimArrayZip _ _
      -> PDFun (seaError "seaOfPrimArray" p) Nothing

seaOfPrimBuf :: PrimBuf -> PrimDoc
seaOfPrimBuf p
 = case p of
     PrimBufMake i t
      -> PDFun   (prefixOfValType (BufT i t) <> "make")
                 (Just [])
     PrimBufPush i t
      -> PDFun   (prefixOfValType (BufT i t) <> "push")
                 (Just [ArgRef, ArgVal])
     PrimBufRead i t
      -> PDAlloc (prefixOfValType (BufT i t) <> "read")
                 (Just [ArgRef])

--------------------------------------------------------------------------------

idouble :: Doc
idouble = prefixOfValType DoubleT


