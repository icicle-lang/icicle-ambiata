{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Type (
    prefixOfArithType
  , prefixOfValType
  , defOfVar
  , defOfVar'
  , seaOfValType
  , valTypeOfExp
  ) where

import qualified Data.List as List

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
     TimeT     -> "itime_"
     ErrorT    -> "ierror_"

     StringT   -> "istring_"
     BufT _ t' -> "ibuf__"   <> prefixOfValType t'
     ArrayT t' -> "iarray__" <> prefixOfValType t'
     MapT{}    -> nope "maps not implemented"

     StructT{} -> nope "structs should have been melted"
     OptionT{} -> nope "options should have been melted"
     PairT{}   -> nope "pairs should have been melted"
     SumT{}    -> nope "sums should have been melted"

------------------------------------------------------------------------

defOfVar :: Int -> ValType -> Doc -> Doc
defOfVar nptrs typ var
 = defOfVar' nptrs (seaOfValType typ) var

defOfVar' :: Int -> Doc -> Doc -> Doc
defOfVar' nptrs typ var
 = let tystr   = show typ
       nspaces = max 1 (17 - length tystr - nptrs)
   in string (tystr <> List.replicate nspaces ' ' <> List.replicate nptrs '*') <> var

seaOfValType :: ValType -> Doc
seaOfValType t
 = let nope = seaError "seaOfValType" . string
   in case t of
     UnitT     -> "iunit_t"
     BoolT     -> "ibool_t"
     IntT      -> "iint_t"
     DoubleT   -> "idouble_t"
     TimeT     -> "itime_t"
     ErrorT    -> "ierror_t"
     StringT   -> "istring_t"
     BufT _ t' -> "ibuf_t__"   <> seaOfValType t'
     ArrayT t' -> "iarray_t__" <> seaOfValType t'

     MapT{}    -> nope "maps not implemented"
     StructT{} -> nope "structs should have been melted"
     OptionT{} -> nope "options should have been melted"
     PairT{}   -> nope "pairs should have been melted"
     SumT{}    -> nope "sums should have been melted"

valTypeOfExp :: Exp (Annot a) n p -> ValType
valTypeOfExp = functionReturns . annType . annotOfExp
