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

     PrimMinimal (M.PrimDouble op)
      -> PDFun
       ( prefixOfValType DoubleT <> seaOfPrimDouble op ) Nothing

     PrimMinimal (M.PrimTime op)
      -> PDFun
       ( prefixOfValType TimeT <> seaOfPrimTime op ) Nothing

     PrimMinimal (M.PrimToDouble op)
      -> PDFun (seaOfPrimToDouble op) Nothing

     PrimMinimal (M.PrimToInt op)
      -> PDFun (seaOfPrimToInt op) Nothing

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

     PrimUpdate op
      -> seaOfPrimUpdate op

     PrimArray op
      -> PDFun (seaOfPrimArray op) Nothing

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
     M.PrimArithPow   -> "pow"

seaOfPrimDouble :: M.PrimDouble -> Doc
seaOfPrimDouble p
 = case p of
     M.PrimDoubleDiv  -> "div"
     M.PrimDoubleLog  -> "log"
     M.PrimDoubleExp  -> "exp"
     M.PrimDoubleSqrt -> "sqrt"

seaOfPrimTime :: M.PrimTime -> Doc
seaOfPrimTime p
 = case p of
     M.PrimTimeDaysDifference -> "days_diff"
     M.PrimTimeDaysEpoch      -> "to_epoch"
     M.PrimTimeMinusDays      -> "minus_days"
     M.PrimTimeMinusMonths    -> "minus_months"

seaOfPrimToDouble :: M.PrimToDouble -> Doc
seaOfPrimToDouble p
 = case p of
     M.PrimToDoubleFromInt -> "iint_extend"

seaOfPrimToInt :: M.PrimToInt -> Doc
seaOfPrimToInt p
 = case p of
     M.PrimToIntFloor    -> "idouble_floor"
     M.PrimToIntCeiling  -> "idouble_ceil"
     M.PrimToIntRound    -> "idouble_round"
     M.PrimToIntTruncate -> "idouble_trunc"

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

seaOfPrimUpdate :: PrimUpdate -> PrimDoc
seaOfPrimUpdate p
 = case p of
     PrimUpdateArrayPut t
      -> PDAlloc (prefixOfValType (ArrayT t) <> "put") Nothing

seaOfPrimArray :: PrimArray -> Doc
seaOfPrimArray p
 = case p of
     _
      -> seaError "seaOfPrimArray" p

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


