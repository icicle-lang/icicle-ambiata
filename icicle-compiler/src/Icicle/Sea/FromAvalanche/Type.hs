{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Icicle.Sea.FromAvalanche.Type (
    seaOfDefinitions
  , prefixOfArithType
  , prefixOfValType
  , baseOfValType
  , defOfVar
  , defOfVar'
  , defOfVar_
  , seaOfValType
  , valTypeOfExp
  ) where

import           Data.Set (Set)
import qualified Data.Set           as Set
import qualified Data.List          as List
import           Data.Functor.Identity

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program

import           Icicle.Common.Annot
import           Icicle.Common.Exp
import           Icicle.Common.Type
import           Icicle.Common.FixT

import           Icicle.Internal.Pretty

import           Icicle.Sea.FromAvalanche.Analysis

import           P
import qualified Prelude as Savage


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
seaOfDefinition expandedType
 = case expandedType of
     BufT n t' ->
       Just $
         "MAKE_BUF (" <> pretty n <> ", " <> baseOfValType t' <> ")"
     ArrayT t' ->
       Just $
         "MAKE_ARRAY (" <> baseOfValType t' <> ")"
     _  -> Nothing

defDepth :: ValType -> Int
defDepth t
 = case t of
     BufT _ t' -> 1 + defDepth t'
     ArrayT t' -> 1 + defDepth t'
     _         -> 1

expandedTypesOfProgram :: Program (Annot a) n Prim -> Set ValType
expandedTypesOfProgram =
 expandedTypesOf . typesOfProgram

-- | Expand the set of types mentioned in a Flat program.
--
--   1. Every definition of (ArrayT (ArrayT a)) requires a definition for
--      (ArrayT a) even if (ArrayT a) is not mentioned by itself in
--      the program.
--
--   2. Every definition of (BufT i a) requires a definition for
--      (ArrayT a), since (Read : BufT i a -> ArrayT a).
--
--      Every definition of (ArrayT (BufT i a)) requires a definiton for
--      ArrayT (ArrayT a) because generated PSV output Sea code uses
--      iarray_iarray_*_index to access the buffers inside.
--
--      This means any type that contains a BufT needs an ArrayT equivalent,
--      including arbitrarily nested ones, e.g. ArrayT (ArrayT (ArrayT BufT))
--
--   Since each of these might introduce more types, we need to apply
--   them to a fixpoint.
--
expandedTypesOf :: Set ValType -> Set ValType
expandedTypesOf set =
  runIdentity . flip fixpoint set $ \ts ->
  let
    fixup t =
      t : case t of
        -- Look inside ArrayT for any BufT.
        ArrayT e ->
          e : fmap ArrayT (fixup e)

        -- We don't generate any nested BufTs so there is no need to look inside.
        BufT _ e ->
          [ArrayT e]
        _ ->
          []

    ts' =
      Set.fromList .
      concatMap fixup .
      Set.toList $
        ts

  in
    if ts' == ts
    then return ts
    else progress ts'

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

defOfVar_ :: Int -> Doc -> Doc -> Doc
defOfVar_ nptrs typ var
 = defOfVar' nptrs typ var

defOfVar' :: Int -> Doc -> Doc -> Doc
defOfVar' nptrs typ var
 = let tystr   = show typ
       nspaces = max 1 (17 - length tystr - nptrs)
   in pretty (tystr <> List.replicate nspaces ' ' <> List.replicate nptrs '*') <> var

seaOfValType :: ValType -> Doc
seaOfValType t
 = baseOfValType t <> "_t"

valTypeOfExp :: Exp (Annot a) n p -> ValType
valTypeOfExp = functionReturns . annType . annotOfExp

------------------------------------------------------------------------

baseOfValType :: ValType -> Doc
baseOfValType t =
 case t of
  UnitT     -> "iunit"
  BoolT     -> "ibool"
  IntT      -> "iint"
  DoubleT   -> "idouble"
  TimeT     -> "itime"
  ErrorT    -> "ierror"

  StringT   -> "istring"
  BufT n t' -> "ibuf_" <> pretty n <> "_" <> baseOfValType t'
  ArrayT t' -> "iarray_" <> baseOfValType t'

  MapT{}    -> Savage.error "maps should have been melted"
  StructT{} -> Savage.error "structs should have been melted"
  OptionT{} -> Savage.error "options should have been melted"
  PairT{}   -> Savage.error "pairs should have been melted"
  SumT{}    -> Savage.error "sums should have been melted"

