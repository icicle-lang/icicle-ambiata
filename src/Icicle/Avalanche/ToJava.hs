{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.ToJava (
    programToJava
  ) where

import              Icicle.Avalanche.Check
import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Program
import qualified    Icicle.Avalanche.Statement.Statement as S
import              Icicle.Avalanche.Statement.Scoped

import              Icicle.Common.Base
import              Icicle.Common.Exp
import qualified    Icicle.Common.Exp.Prim.Minimal as M
import              Icicle.Common.Type

import              Icicle.Internal.Pretty

import              P

import              Data.Functor.Identity


programToJava :: (Pretty n, Ord n) => Program n Prim -> Doc
programToJava p
 = "class Feature"
 <> block
 -- TODO: need the concrete feature type here
 [ "public void compute(IcicleState" <> angled (maybe "$#@! NO FEATURE LOOP" boxedType $ concreteFeatureType $ statements p) <> " icicle)"
 <> block
    [ local DateTimeT (binddate p) <> " = icicle.now();"
    , ""
    , statementsToJava (initialContext p) (scopedOfStatement $ statements p)
    ]
 ]

concreteFeatureType :: S.Statement n p -> Maybe ValType
concreteFeatureType ss
 = runIdentity
 $ S.foldStmt (\_ _ -> return ()) up orl () Nothing ss
 where
  up _ r s
   = case s of
      S.ForeachFacts _ _ ty _ _
       -> return (Just ty)
      _
       -> return r

  orl (Just l) _ = Just l
  orl _ r = r


statementsToJava :: (Pretty n, Ord n) => Context n -> Scoped n Prim -> Doc
statementsToJava ctx ss
 = case ss of
    If x t e
     -> "if (" <> expToJava x <> ")"
     <> block [go t]
     <> (case e of
          Block [] -> ""
          If{}     -> line <> "else " <>        go e
          _        -> line <> "else"  <> block [go e])

    -- NB. it looks like we are recomputing "to" on every iteration,
    -- however if input program is in a-normal form, it will already
    -- be pulled out to a variable binding.
    ForeachInts n from to s
     -> "for (" <> local IntT n <> " = " <> expToJava from <> "; "
                <> name n <> " < " <> expToJava to <> "; "
                <> name n <> "++)" <> block [go s]

    ForeachFacts n n' t f s
     -> (case f of
          S.FactLoopHistory -> "icicle.startHistory();"
          S.FactLoopNew     -> "icicle.startNew();")
        <> line
        <> "while (icicle.nextRow())"
        <> block [ local t n <> " = icicle.currentRow();"
                 , local DateTimeT n' <> " = icicle.currentRowDate();"
                 , go s]
    Block bs
     -> vcat $ fmap (either goB go) bs
    Write n x
     -> acc_name n <> " = " <> expToJava x <> ";"
    Push n x
     -> "icicle.pushLatest(" <> acc_name n <> ", " <> expToJava x <> ");"
    Return x
     -> "return " <> expToJava x
    KeepFactInHistory
     -> "icicle.KeepFactInHistory();"
    LoadResumable n
     -> acc_name n <> " = icicle.loadResumable(" <> stringy n <> ");"
    SaveResumable n
     -> "icicle.saveResumable(" <> stringy n <> ", " <> acc_name n <> ");"

 where
  go  = statementsToJava $ tc $ statementOfScoped ss
  goB b = bindingToJava   (tc $ statementOfScoped $ Block [Left b]) b

  tc s'
   = case statementContext flatFragment ctx s' of
      Left _ -> ctx
      Right c' -> c'

bindingToJava :: Pretty n => Context n -> Binding n Prim -> Doc
bindingToJava _ctx bb
 = case bb of
    InitAccumulator acc@(S.Accumulator { S.accKind = S.Latest })
     -> "Latest" <> angled (boxedType $ S.accValType acc)
     <+> (acc_name $ S.accName acc)
     <> " = icicle.makeLatest"
     <> angled (boxedType $ S.accValType acc)
     <> "(" <> expToJava (S.accInit acc) <> ");"

    InitAccumulator acc@(S.Accumulator { S.accKind = S.Mutable })
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
  -- Monomorphise THIS!
  -- so it works with [] as well
  primApp
   = \p args
   -> prettyPrimType (primTypeOfPrim p)
                     (fmap expToJava args)

data PrimType
 = Infix Doc
 | Prefix Doc
 | Method Doc
 | Function Doc
 | Special1 (Doc -> Doc)

prettyPrimType :: PrimType -> [Doc] -> Doc
prettyPrimType pt args
 = case pt of
    -- We don't need parens around operators, or to worry about precedence.
    -- Since the input is in a-normal form the only arguments can be variables.
    Infix o
     -> hcat (punctuate (" "<>o<>" ") args)
    Prefix o
     -> o <> hcat args
    Method m
     | (a:as) <- args
     -> a <> "." <> m <> "(" <> hcat (punctuate ", " as) <> ")"
     | otherwise
     -> "$#!@ METHOD NO ARGUMENTS"
    Function f
     -> f <> "(" <> hcat (punctuate ", " args) <> ")"
    Special1 f
     | (a:_)  <- args
     -> f a
     | otherwise
     -> "$#!@ SPECIAL NO ARGUMENTS"

primTypeOfPrim :: Prim -> PrimType
primTypeOfPrim p
 = case p of
    PrimMinimal pm
     -> min' pm
    PrimProject pp
     -> proj pp
    PrimUnsafe pu
     -> unsa pu
    PrimUpdate pu
     -> upda pu

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

  min' (M.PrimStruct (M.PrimStructGet f t _))
   = Special1 $ \a -> a <> ".getField" <> angled (boxedType t) <> "(" <> stringy f <> ")"

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

  proj (PrimProjectArrayLength _) = Method "size"
  proj (PrimProjectMapLength _ _) = Method "size"
  proj (PrimProjectMapLookup _ _) = Method "get"
  proj (PrimProjectOptionIsSome _)= Special1 $ \a -> a <> " != null"

  unsa (PrimUnsafeArrayIndex _)    = Method "get"
  unsa (PrimUnsafeArrayCreate t)   = Function ("new ArrayList" <> angled (boxedType t))
  unsa (PrimUnsafeMapIndex _ _)    = Function "Map.getByIndex"
  unsa (PrimUnsafeOptionGet _)      = Special1 $ \a -> a

  upda (PrimUpdateMapPut _ _)      = Function "Map.put"
  upda (PrimUpdateArrayPut _)      = Function "Array.put"


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
 = vcat
 [ ""
 , "{"
 , indent 2 (vcat ds)
 , "}"
 ]

angled :: Doc -> Doc
angled = enclose "<" ">"

commas :: [Doc] -> Doc
commas = hcat . punctuate ", "


-- double show to add quotes and escape slashes and quotes.
-- not that there should be any slashes or quotes
stringy :: Pretty n => n -> Doc
stringy = text . show . show . pretty
