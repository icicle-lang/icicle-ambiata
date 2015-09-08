{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -w #-}
module Icicle.Avalanche.ToSea (
    seaOfProgram
  ) where

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Exp
import qualified Icicle.Common.Exp.Prim.Minimal as M
import           Icicle.Common.Type


import           Icicle.Internal.Pretty

import           P
import           Prelude (undefined)

import           Data.Functor.Identity

import qualified Data.Map as Map


seaOfProgram :: (Show a, Show n, Pretty n, Ord n)
             => Program (Annot a) n Prim -> Doc
seaOfProgram program = vcat [
    text "#include <stdbool.h>"
  , text "#include <stdint.h>"
  , text ""
  , text "typedef int64_t     date_t;"
  , text "typedef const char *error_t;"
  , text ""
  , text "void compute(struct icicle_state *s)"
  , text "{"
  , indent 4 (seaOfStatement (statements program))
  , text "}"
  ]

-- data Accumulator a n p
--  = Accumulator
--  { accName      :: Name n
--  , accKind      :: AccumulatorType
--  , accValType   :: ValType
--  , accInit      :: Exp a n p
--  }

seaOfStatement :: (Show a, Show n, Pretty n, Ord n)
               => Statement (Annot a) n Prim -> Doc
seaOfStatement stmt =
  case stmt of
    InitAccumulator acc stmt'
     | Accumulator n Mutable vt ixx <- acc
     -> text "acc";
    
    _
     -> text ("printf(\"" <> show stmt <> "\");")


--initVal :: ValType -> Doc
--initVal t
-- = case t of
--     IntT       -> "Integer"
--     DoubleT    -> "Double"
--     UnitT      -> "Integer"
--     BoolT      -> "Boolean"
--     DateTimeT  -> "Integer"
--     ArrayT a   -> "ArrayList" <> angled (boxedType a)
--     MapT a b   -> "HashMap" <> angled (commas [boxedType a, boxedType b])
--     OptionT a  -> boxedType a
