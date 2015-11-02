{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Type (
    prefixOfArithType
  , prefixOfValType
  , seaOfValType
  , valTypeOfExp
  ) where

import           Icicle.Common.Annot
import           Icicle.Common.Exp
import           Icicle.Common.Type

import           Icicle.Internal.Pretty

import           Icicle.Sea.FromAvalanche.Base

import           P




prefixOfArithType :: ArithType -> Doc
prefixOfArithType t
 = case t of
     ArithIntT    -> prefixOfValType IntT
     ArithDoubleT -> prefixOfValType DoubleT

prefixOfValType :: ValType -> Doc
prefixOfValType t
 = let nope = seaError "prefixOfValType" . string
   in case t of
     UnitT     -> "iunit_"
     BoolT     -> "ibool_"
     IntT      -> "iint_"
     DoubleT   -> "idouble_"
     DateTimeT -> "idate_"
     ErrorT    -> "ierror_"

     StringT   -> "istring_"
     BufT   t' -> "ibuf__"   <> prefixOfValType t'
     ArrayT t' -> "iarray__" <> prefixOfValType t'
     MapT{}    -> nope "maps not implemented"

     StructT{} -> nope "structs should have been melted"
     OptionT{} -> nope "options should have been melted"
     PairT{}   -> nope "pairs should have been melted"
     SumT{}    -> nope "sums should have been melted"

------------------------------------------------------------------------

seaOfValType :: ValType -> Doc
seaOfValType t
 = let nope = seaError "seaOfValType" . string
   in case t of
     UnitT     -> "iunit_t   "
     BoolT     -> "ibool_t   "
     IntT      -> "iint_t    "
     DoubleT   -> "idouble_t "
     DateTimeT -> "idate_t   "
     ErrorT    -> "ierror_t  "

     StringT   -> "istring_t "
     BufT   t' -> "BUF_OF("   <> seaOfValType t' <> ")"
     ArrayT t' -> "ARRAY_OF(" <> seaOfValType t' <> ")"
     MapT{}    -> nope "maps not implemented"

     StructT{} -> nope "structs should have been melted"
     OptionT{} -> nope "options should have been melted"
     PairT{}   -> nope "pairs should have been melted"
     SumT{}    -> nope "sums should have been melted"

valTypeOfExp :: Exp (Annot a) n p -> Maybe ValType
valTypeOfExp = unFun . annType . annotOfExp
  where
    unFun (FunT [] t) = Just t
    unFun _           = Nothing

