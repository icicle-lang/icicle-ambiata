-- | Statements and mutable accumulators (variables) for Avalanche
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.ToJava (
    programToJava
  ) where

import              Icicle.Avalanche.Program
import              Icicle.Avalanche.Prim.Flat
import qualified    Icicle.Avalanche.Statement.Statement as S
import              Icicle.Avalanche.Statement.Scoped

import              Icicle.Common.Base
import              Icicle.Common.Exp
import qualified    Icicle.Common.Exp.Prim.Minimal as M
import              Icicle.Common.Type

import              Icicle.Internal.Pretty

import              P

programToJava :: Pretty n => Program n Prim -> Doc
programToJava p
 = "class Feature"
 <> block
 -- TODO: need the concrete feature type here
 [ "public void compute(IcicleState" <> angled (boxedType IntT) <> " icicle)"
 <> block
    [ local DateTimeT (binddate p) <> " = icicle.now();"
    , ""
    , statementsToJava (scopedOfStatement $ statements p)
    ]
 ]

statementsToJava :: Pretty n => Scoped n Prim -> Doc
statementsToJava ss
 = case ss of
    If x t e
     -> "if (" <> expToJava x <> ")"
     <> go t
     <> "else"
     <> go e

    -- NB. it looks like we are recomputing "to" on every iteration,
    -- however if input program is in a-normal form, it will already
    -- be pulled out to a variable binding.
    ForeachInts n from to s
     -> "for (" <> local IntT n <> " = " <> expToJava from <> "; "
                <> name n <> " < " <> expToJava to <> "; "
                <> name n <> "++)" <> go s

    ForeachFacts n t f s
     -> (case f of
          S.FactLoopHistory -> "icicle.startHistory();"
          S.FactLoopNew     -> "icicle.startNew();")
        <> line
        <> "while (icicle.nextRow())"
        <> block [local (PairT t DateTimeT) n <> " = icicle.currentRow();", go s]
    Block bs
     -> block $ fmap (either bindingToJava go) bs
    Write n x
     -> acc_name n <> " = " <> expToJava x <> ";"
    Push n x
     -> "icicle.pushLatest(" <> acc_name n <> ", " <> expToJava x <> ");"
    Return x
     -> "return " <> expToJava x
    KeepFactInHistory
     -> "icicle.KeepFactInHistory();"

 where
  go  = statementsToJava

bindingToJava :: Pretty n => Binding n Prim -> Doc
bindingToJava bb
 = case bb of
    InitAccumulator acc
     -- TODO: resumables also need to load and save
     -- latests need to call icicle.makeLatest
     -> unboxedType (S.accValType acc) <+> (acc_name $ S.accName acc)
     <> " = " <> expToJava (S.accInit acc) <> ";"

    Let n x
     -- TODO: need the type here
     -> local IntT n <> " = " <> expToJava x <> ";"

    Read n acc
     -- TODO: need the type here
     -> local IntT n <> " = " <> acc_name acc <> ";"

expToJava :: Pretty n => Exp n Prim -> Doc
expToJava xx
 = case xx of
    XVar n
     -> name n
    XValue _ v
     -> case v of
         VInt i -> pretty i
         _      -> "$#@! TODO VALUE " <> pretty v
    XPrim p
     -> primApp p []
    XApp{}
     | Just (p, xs) <- takePrimApps xx
     -> primApp p xs

     -- TODO: better error handling.
     -- These should not appear here.
     -- Applications should only be to prims
     | otherwise
     -> "$#@!BAD APPLICATION"
    -- We should not have any lambdas after flattening
    XLam{}
     -> "$#@!LAMBDA NOT ALLOWED"
    -- A-normalisation should have lifted these to statements
    XLet{}
     -> "$#@!LET NOT ALLOWED"

 where
  primApp
   = \p args
   -> prettyPrimType (primTypeOfPrim p)
                     (fmap expToJava args)

data PrimType
 = Infix Doc
 | Prefix Doc
 | Method Doc
 | Function Doc

prettyPrimType :: PrimType -> [Doc] -> Doc
prettyPrimType pt args
 = case pt of
    Infix o
     -> "(" <> hcat (punctuate (" "<>o<>" ") args) <> ")"
    Prefix o
     -> "(" <> o <> hcat args <> ")"
    Method m
     | (a:as) <- args
     -> a <> "." <> m <> tupled as
     | otherwise
     -> "$#!@ METHOD NO ARGUMENTS"
    Function f
     -> f <> tupled args

primTypeOfPrim :: Prim -> PrimType
primTypeOfPrim p
 = case p of
    PrimMinimal pm
     -> min' pm
    _
     -> todo
 where
  min' (M.PrimArith ar) = ari ar
  min' (M.PrimRelation re _) = rel re
  min' (M.PrimLogical lo) = log' lo

  min' (M.PrimConst (M.PrimConstPair _ _))
   = Function "Pair.create"
  min' (M.PrimConst (M.PrimConstSome _))
   = Function ""
  min' (M.PrimDateTime M.PrimDateTimeDaysDifference)
   = Function "icicle.daysDifference"

  min' (M.PrimPair (M.PrimPairFst _ _))
   = Method "fst"
  min' (M.PrimPair (M.PrimPairSnd _ _))
   = Method "snd"

  min' _ = todo

  ari   M.PrimArithPlus   = Infix     "+"
  ari   M.PrimArithMinus  = Infix     "-"
  ari   M.PrimArithDiv    = Infix     "/"
  ari   M.PrimArithMul    = Infix     "*"
  ari   M.PrimArithNegate = Prefix    "-"

  rel   M.PrimRelationGt  = Infix     ">"
  rel   M.PrimRelationGe  = Infix     ">="
  rel   M.PrimRelationLt  = Infix     "<"
  rel   M.PrimRelationLe  = Infix     "<="
  rel   M.PrimRelationEq  = Infix     "=="
  rel   M.PrimRelationNe  = Infix     "!="

  log'   M.PrimLogicalNot  = Prefix    "!"
  log'   M.PrimLogicalAnd  = Infix     "&&"
  log'   M.PrimLogicalOr   = Infix     "||"

  todo = Method ("$#@! TODO OPERATOR " <> pretty p)


local :: Pretty n => ValType -> Name n -> Doc
local t n
 = unboxedType t <+> name n

name :: Pretty n => Name n -> Doc
-- TODO munge and replace bad characters
name = pretty

acc_name :: Pretty n => Name n -> Doc
acc_name n
 = "ACCUMULATOR$" <> name n

boxedType :: ValType -> Doc
boxedType t
 = case t of
     IntT       -> "Integer"
     UnitT      -> "Integer"
     BoolT      -> "Boolean"
     DateTimeT  -> "Day"
     ArrayT a   -> "ArrayList" <> angled (boxedType a)
     MapT a b   -> "HashMap" <> angled (commas [boxedType a, boxedType b])
     OptionT a  -> boxedType a
     PairT a b  -> "Pair" <> angled (commas [boxedType a, boxedType b])
     -- ???
     StructT _  -> "HashMap" <> angled (commas ["String", "Object"])
     StringT    -> "String"

unboxedType :: ValType -> Doc
unboxedType t
 = case t of
     IntT       -> "int"
     UnitT      -> "int"
     BoolT      -> "boolean"
     DateTimeT  -> "int"
     _    -> boxedType t



block :: [Doc] -> Doc
block ds
 = " {" <> line
 <> indent 2 (vcat ds)
 <> line <> "}" <> line

angled :: Doc -> Doc
angled = enclose "<" ">"

commas :: [Doc] -> Doc
commas = hcat . punctuate ", "

