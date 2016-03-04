{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Type (
    seaOfDefinitions
  , prefixOfArithType
  , prefixOfValType
  , baseOfValType
  , defOfVar
  , defOfVar'
  , seaOfValType
  , valTypeOfExp
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program

import           Icicle.Common.Annot
import           Icicle.Common.Exp
import           Icicle.Common.Type

import           Icicle.Internal.Pretty

import           Icicle.Sea.FromAvalanche.Analysis
import           Icicle.Sea.FromAvalanche.Base

import           P

------------------------------------------------------------------------

seaOfDefinitions :: [Program (Annot a) n Prim] -> Doc
seaOfDefinitions programs
 = vsep
 [ ""
 , "#line 1 \"type definitions\""
 , vsep . mapMaybe seaOfDefinition
        . List.sortBy (comparing defDepth)
        . Set.toList
        . Set.unions
        . fmap expandedTypesOfProgram
        $ programs
 , ""
 , ""
 ]

seaOfDefinition :: ValType -> Maybe Doc
seaOfDefinition t
 = case t of
     BufT n t' -> Just ("MAKE_BUF   (" <> int n <> ", " <> baseOfValType t' <> ")")
     ArrayT t' -> Just ("MAKE_ARRAY ("                  <> baseOfValType t' <> ")")
     _         -> Nothing

defDepth :: ValType -> Int
defDepth t
 = case t of
     BufT _ t' -> 1 + defDepth t'
     ArrayT t' -> 1 + defDepth t'
     _         -> 1

-- If we have a program that has an empty 'ArrayT (ArrayT a)' but it is never
-- used then the type 'ArrayT a' may not be mentioned anywhere in the program,
-- even though its definition is required to construct an 'ArrayT (ArrayT a)'.
expandType :: ValType -> [ValType]
expandType t
 = case t of
     BufT  _ t' -> t : expandType t'
     ArrayT  t' -> t : expandType t'
     _          -> [t]

expandedTypesOfProgram :: Program (Annot a) n Prim -> Set ValType
expandedTypesOfProgram
 = Set.fromList
 . concatMap expandType
 . Set.toList
 . typesOfProgram

------------------------------------------------------------------------

prefixOfArithType :: ArithType -> Doc
prefixOfArithType t
 = case t of
     ArithIntT    -> prefixOfValType IntT
     ArithDoubleT -> prefixOfValType DoubleT

prefixOfValType :: ValType -> Doc
prefixOfValType t
 = baseOfValType t <> "_"

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
 = baseOfValType t <> "_t"

valTypeOfExp :: Exp (Annot a) n p -> ValType
valTypeOfExp = functionReturns . annType . annotOfExp

------------------------------------------------------------------------

baseOfValType :: ValType -> Doc
baseOfValType t
 = let nope = seaError "prefixOfValType" . string
   in case t of
     UnitT     -> "iunit"
     BoolT     -> "ibool"
     IntT      -> "iint"
     DoubleT   -> "idouble"
     TimeT     -> "itime"
     ErrorT    -> "ierror"
     FactIdentifierT
                -> "iint"

     StringT   -> "istring"
     BufT n t' -> "ibuf_" <> int n <> "_" <> baseOfValType t'
     ArrayT t' -> "iarray_"               <> baseOfValType t'
     MapT{}    -> nope "maps not implemented"

     StructT{} -> nope "structs should have been melted"
     OptionT{} -> nope "options should have been melted"
     PairT{}   -> nope "pairs should have been melted"
     SumT{}    -> nope "sums should have been melted"

