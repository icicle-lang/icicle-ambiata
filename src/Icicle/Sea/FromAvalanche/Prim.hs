{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Prim (
    seaOfXPrim
  , seaOfPrimDocApps
  , PrimDoc (..)
  ) where

import           Icicle.Avalanche.Prim.Flat

import qualified Icicle.Common.Exp.Prim.Minimal as M
import           Icicle.Common.Type

import           Icicle.Internal.Pretty

import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Type

import           P

data PrimDoc
 = PDFun   Doc
 | PDMeth  Doc
 | PDMethAlloc  Doc
 | PDAlloc Doc

seaOfPrimDocApps :: PrimDoc -> [Doc] -> Doc
seaOfPrimDocApps p xs
 = case p of
    PDFun   d -> d <+> tuple xs
    PDMeth  d
     | x:xs' <- xs
     -> x <> "." <> d <+> tuple xs'
     | otherwise
     -> seaError "Method with no target" (d,xs)
    PDMethAlloc  d
     | x:xs' <- xs
     -> x <> "." <> d <+> tuple (alloc xs')
     | otherwise
     -> seaError "Method with no target" (d,xs)
    PDAlloc d -> d <+> tuple (alloc xs)
 where
  alloc = ("&s->mempool" :)

seaOfXPrim :: Prim -> PrimDoc
seaOfXPrim p
 = case p of
     PrimMinimal (M.PrimArithUnary op t)
      -> PDFun
       ( prefixOfArithType t <> seaOfPrimArithUnary op )

     PrimMinimal (M.PrimArithBinary op t)
      -> PDFun
       ( prefixOfArithType t <> seaOfPrimArithBinary op )

     PrimMinimal (M.PrimDouble op)
      -> PDFun
       ( prefixOfValType DoubleT <> seaOfPrimDouble op )

     PrimMinimal (M.PrimDateTime op)
      -> PDFun
       ( prefixOfValType DateTimeT <> seaOfPrimDateTime op )

     PrimMinimal (M.PrimCast op)
      -> PDFun $ seaOfPrimCast op

     PrimMinimal (M.PrimRelation op t)
      -> PDFun
       ( seaOfPrimRelation op <> string (templateOfValType [t]))

     PrimMinimal (M.PrimConst op)
      -> seaOfPrimConst op

     PrimMinimal (M.PrimLogical op)
      -> seaOfPrimLogical op

     PrimMinimal (M.PrimPair op)
      -> seaOfPrimPair op

     PrimMinimal (M.PrimStruct op)
      -> seaOfPrimStruct op


     PrimProject op
      -> seaOfPrimProject op

     PrimUnsafe op
      -> seaOfPrimUnsafe op

     PrimUpdate op
      -> seaOfPrimUpdate op

     PrimBuf   op
      -> seaOfPrimBuf op

     PrimPack op
      -> seaOfPrimPack op

     -- _
     -- -> PDFun $ seaError "seaOfXPrim" p

seaOfPrimArithUnary :: M.PrimArithUnary -> Doc
seaOfPrimArithUnary p
 = case p of
     M.PrimArithNegate -> "neg"

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
     M.PrimDoubleDiv -> "div"
     M.PrimDoubleLog -> "log"
     M.PrimDoubleExp -> "exp"

seaOfPrimDateTime :: M.PrimDateTime -> Doc
seaOfPrimDateTime p
 = case p of
     M.PrimDateTimeDaysDifference -> "days_diff"
     M.PrimDateTimeDaysEpoch      -> "to_epoch"
     M.PrimDateTimeMinusDays      -> "minus_days"
     M.PrimDateTimeMinusMonths    -> "minus_months"

seaOfPrimCast :: M.PrimCast -> Doc
seaOfPrimCast p
 = case p of
     M.PrimCastDoubleOfInt -> "iint_extend"
     M.PrimCastIntOfDouble -> "idouble_trunc"
     _                     -> seaError "seaOfPrimCast" p

seaOfPrimConst :: M.PrimConst -> PrimDoc
seaOfPrimConst p
 = case p of
     M.PrimConstPair a b
      -> PDFun ("ipair_t" <> string (templateOfValType [a,b]))
     M.PrimConstSome t
      -> PDFun ("ioption_some" <> string (templateOfValType [t]))
     M.PrimConstLeft a b
      -> PDFun ("isum_left" <> string (templateOfValType [a,b]))
     M.PrimConstRight a b
      -> PDFun ("isum_right" <> string (templateOfValType [a,b]))

seaOfPrimLogical :: M.PrimLogical -> PrimDoc
seaOfPrimLogical p
 = case p of
     M.PrimLogicalNot
      -> PDFun "ibool_not"
     M.PrimLogicalAnd
      -> PDFun "ibool_and"
     M.PrimLogicalOr
      -> PDFun "ibool_or"

seaOfPrimPair :: M.PrimPair -> PrimDoc
seaOfPrimPair p
 = case p of
     M.PrimPairFst _ _
      -> PDMeth "fst"
     M.PrimPairSnd _ _
      -> PDMeth "snd"

seaOfPrimStruct :: M.PrimStruct -> PrimDoc
seaOfPrimStruct p
 = case p of
     M.PrimStructGet f _ _
      -> PDMeth ("getField__" <> pretty f)


seaOfPrimRelation :: M.PrimRelation -> Doc
seaOfPrimRelation p
 = case p of
     M.PrimRelationGt -> "igt"
     M.PrimRelationGe -> "ige"
     M.PrimRelationLt -> "ilt"
     M.PrimRelationLe -> "ile"
     M.PrimRelationEq -> "ieq"
     M.PrimRelationNe -> "ine"

seaOfPrimProject :: PrimProject -> PrimDoc
seaOfPrimProject p
 = case p of
     PrimProjectArrayLength _
      -> PDMeth "length"
     PrimProjectMapLength _ _
      -> PDMeth "length"
     PrimProjectMapLookup _ _
      -> PDMeth "lookup"
     PrimProjectOptionIsSome _
      -> PDMeth "isSome"
     PrimProjectSumIsRight _ _
      -> PDMeth "isRight"

seaOfPrimUnsafe :: PrimUnsafe -> PrimDoc
seaOfPrimUnsafe p
 = case p of
     PrimUnsafeArrayIndex _
      -> PDMeth  "index"
     PrimUnsafeArrayCreate t
      -> PDAlloc ("iarray_t" <> string (templateOfValType [t]))
     PrimUnsafeOptionGet _
      -> PDMeth "unsafeGet"
     PrimUnsafeSumGetLeft _ _
      -> PDMeth "unsafeLeft"
     PrimUnsafeSumGetRight _ _
      -> PDMeth "unsafeRight"
     PrimUnsafeMapIndex _ _
      -> PDMeth "index"

seaOfPrimUpdate :: PrimUpdate -> PrimDoc
seaOfPrimUpdate p
 = case p of
     PrimUpdateArrayPut _
      -> PDMeth "put"
     PrimUpdateMapPut _ _
      -> PDMethAlloc "put"

seaOfPrimBuf :: PrimBuf -> PrimDoc
seaOfPrimBuf p
 = case p of
     PrimBufMake t
      -> PDAlloc ("ibuf_t" <> string (templateOfValType [t]))
     PrimBufPush _
      -> PDMeth  "push"
     PrimBufRead _
      -> PDMethAlloc "read"


seaOfPrimPack :: PrimPack -> PrimDoc
seaOfPrimPack p
 = case p of
     PrimSumPack a b
      -> PDFun ("isum_t" <> string (templateOfValType [a, b]))
     PrimOptionPack a
      -> PDFun ("ioption_t" <> string (templateOfValType [a]))
     PrimStructPack fs
      -> PDFun (noPadSeaOfValType $ StructT fs)



