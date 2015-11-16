{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Type (
    prefixOfArithType
  , prefixOfValType
  , seaOfValType
  , noPadSeaOfValType
  , valTypeOfExp
  ) where

import qualified Data.List as List
import           Data.String (String)

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
     BufT _ t' -> "ibuf__"   <> prefixOfValType t'
     ArrayT t' -> "iarray__" <> prefixOfValType t'
     MapT{}    -> nope "maps not implemented"

     StructT{} -> nope "structs should have been melted"
     OptionT{} -> nope "options should have been melted"
     PairT{}   -> nope "pairs should have been melted"
     SumT{}    -> nope "sums should have been melted"

------------------------------------------------------------------------

seaOfValType :: ValType -> Doc
seaOfValType t
 = let nope   = seaError "seaOfValType" . string
       pad xs = string (xs <> List.replicate (16 - length xs) ' ')
   in either nope pad (stringOfValType t)

noPadSeaOfValType :: ValType -> Doc
noPadSeaOfValType t
 = let nope = seaError "seaOfValType" . string
   in either nope string (stringOfValType t)

stringOfValType :: ValType -> Either String String
stringOfValType t
 = case t of
     UnitT     -> Right "iunit_t"
     BoolT     -> Right "ibool_t"
     IntT      -> Right "iint_t"
     DoubleT   -> Right "idouble_t"
     DateTimeT -> Right "idate_t"
     ErrorT    -> Right "ierror_t"
     StringT   -> Right "istring_t"
     BufT _ t' -> fmap (\x -> "ibuf_t__"   <> x <> "") (stringOfValType t')
     ArrayT t' -> fmap (\x -> "iarray_t__" <> x <> "") (stringOfValType t')

     MapT{}    -> Left "maps not implemented"
     StructT{} -> Left "structs should have been melted"
     OptionT{} -> Left "options should have been melted"
     PairT{}   -> Left "pairs should have been melted"
     SumT{}    -> Left "sums should have been melted"

valTypeOfExp :: Exp (Annot a) n p -> ValType
valTypeOfExp = functionReturns . annType . annotOfExp
