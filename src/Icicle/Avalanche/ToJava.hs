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

import qualified    Data.Map    as Map


programToJava :: (Pretty n, Ord n, Show n) => Program n Prim -> Doc
programToJava p
 = "import java.util.*;"
 <> line
 <> "class Feature"
 <> block
 [ "public void compute(IcicleState" <> angled (maybe "$#@! NO FEATURE LOOP" boxedType $ concreteFeatureType $ statements p) <> " icicle)"
 <> block
    [ local DateTimeT (binddate p) <> " = icicle.snapshotDate();"
    , ""
    , case statementContext flatFragment (initialContext p) (statements p) of
       Left err -> "$#@! " <> text (show err)
       Right ctx -> statementsToJava ctx (scopedOfStatement $ statements p)
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


statementsToJava :: (Pretty n, Ord n, Show n) => Context n -> Scoped n Prim -> Doc
statementsToJava ctx ss
 = case ss of
    If x t e
     -> "if (" <> expToJava ctx Unboxed x <> ")"
     <> block [go t]
     <> (case e of
          Block [] -> ""
          If{}     -> line <> "else " <>        go e
          _        -> line <> "else"  <> block [go e])

    -- NB. it looks like we are recomputing "to" on every iteration,
    -- however if input program is in a-normal form, it will already
    -- be pulled out to a variable binding.
    ForeachInts n from to s
     -> "for (" <> local IntT n <> " = " <> expToJava ctx Unboxed from <> "; "
                <> name n <> " < " <> expToJava ctx Unboxed to <> "; "
                <> name n <> "++)" <> block [go s]

    ForeachFacts n n' t f s
     -> (case f of
          S.FactLoopHistory -> "icicle.startHistory();"
          S.FactLoopNew     -> "icicle.startNew();")
        <> line
        <> "while (icicle.nextRow())"
        <> block [ local t n <> " = " <> unbox t "icicle.currentRow()" <> ";"
                 , local DateTimeT n' <> " = icicle.currentRowDate();"
                 , go s]
    Block blocks
     -> let iter _ []     = []
            iter c (b:bs)
              = case either (tcB c) (tcS c) b of
                 Left e   -> ["$#@! " <> text (show e)]
                 Right c' -> either (goB c') (goS c') b
                           : iter c' bs
        in vcat $ iter ctx blocks
    Write n x
     -> acc_name n <> " = " <> expToJava ctx Unboxed x <> ";"
    Push n x
     -> "icicle.pushLatest(" <> acc_name n <> ", " <> expToJava ctx Boxed x <> ");"
    Return x
     -> "icicle.output(" <> expToJava ctx Unboxed x <> ");"
    KeepFactInHistory
     -> "icicle.keepFactInHistory();"
    LoadResumable n
     | Just (ATUpdate t) <- Map.lookup n (ctxAcc ctx)
     -> let nm = "LOAD$" <> name n
        in   boxedType t <> " " <> nm <> " = " <> "icicle." <> angled (boxedType t) <> "loadResumable(\"feature\", " <> stringy n <> ");"
          <> line
          <> "if (" <> nm <> " != null)"
          <> block [ acc_name n <> " = " <> unbox t nm <> ";" ]
     | otherwise
     -> "$#!@ no such accumulator " <> acc_name n
    SaveResumable n
     | Just (ATUpdate t) <- Map.lookup n (ctxAcc ctx)
     -> "icicle.saveResumable(\"feature\", " <> stringy n <> ", " <> box t (acc_name n) <> ");"
     | otherwise
     -> "$#!@ no such accumulator " <> acc_name n

 where
  go = goS ctx

  goS c s
   = either (text.show) id
   ( flip statementsToJava s <$> tcS c s )

  goB c b
   = either (text.show) id
   ( flip bindingToJava  b <$> tcB c b )

  tcB c b = tc c (statementOfScoped $ Block [Left b])
  tcS c s = tc c (statementOfScoped s)

  tc c s = statementContext flatFragment c s

bindingToJava :: (Pretty n, Ord n, Show n) => Context n -> Binding n Prim -> Doc
bindingToJava ctx bb
 = case bb of
    InitAccumulator acc@(S.Accumulator { S.accKind = S.Latest })
     -> "Latest" <> angled (boxedType $ S.accValType acc)
     <+> (acc_name $ S.accName acc)
     <> " = icicle."
     <> "makeLatest"
     <> "(" <> expToJava ctx Unboxed (S.accInit acc) <> ");"

    InitAccumulator acc@(S.Accumulator { S.accKind = S.Mutable })
     -> unboxedType (S.accValType acc) <+> (acc_name $ S.accName acc)
     <> " = " <> expToJava ctx Unboxed (S.accInit acc) <> ";"

    Let n x
     | Just (FunT [] t) <- Map.lookup n (ctxExp ctx)
     -> local t n <> " = " <> expToJava ctx Unboxed x <> ";"
     | otherwise
     -> "$#@! no variable " <> name n

    Read n acc
     | Just (ATPush t) <- Map.lookup acc (ctxAcc ctx)
     -> local (ArrayT t) n <> " = icicle.readLatest(" <> acc_name acc <> ");"
     | Just (ATUpdate t) <- Map.lookup acc (ctxAcc ctx)
     -> local t n <> " = " <> acc_name acc <> ";"
     | otherwise
     -> "$#@! no accumulator " <> acc_name acc <> line <> text (show ctx)

expToJava :: (Pretty n, Show n, Ord n) => Context n -> Boxy -> Exp n Prim -> Doc
expToJava ctx b xx
 = case checkExp flatFragment (ctxExp ctx) xx of
    Left err -> "$#!@ type error " <> text (show err)
    Right (FunT _ t)
     -> case xx of
            XVar n
             -> boxy b Unboxed t
              $ name n
            XValue _ v
             -> case v of
                 VInt i -> boxy b Unboxed t $ pretty i
                 VUnit  -> boxy b Unboxed t $ "13013"
                 VMap m
                  | Map.null m
                  -> "IcicleMap.empty()"
                 VArray[]-> "Array.empty()"
                 VNone   -> "null"
                 VSome v'
                  | OptionT t' <- t
                  -> expToJava ctx Boxed (XValue t' v')
                 _      -> "$#@! TODO VALUE " <> pretty v
            XPrim p
             -> primApp t p []
            XApp{}
             | Just (p, xs) <- takePrimApps xx
             -> primApp t p xs

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
   = \t p args
   -> boxy b (boxyOfPrimReturn p) t
    $ prettyPrimType (primTypeOfPrim p)
                     (fmap (expToJava ctx $ boxyOfPrimArgs p) args)

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
    PrimArray (PrimArrayZip _ _)
     -> Function "Array.zip"

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
   = Special1 $ \a -> a <> "." <> angled (boxedType t) <> "getField" <> "(" <> stringy f <> ")"

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
  unsa (PrimUnsafeMapIndex _ _)    = Function "IcicleMap.getByIndex"
  unsa (PrimUnsafeOptionGet _)      = Special1 $ \a -> a

  upda (PrimUpdateMapPut _ _)      = Function "IcicleMap.put"
  upda (PrimUpdateArrayPut _)      = Function "Array.put"


data Boxy = Boxed | Unboxed

boxy :: Boxy -> Boxy -> ValType -> Doc -> Doc
boxy Boxed Unboxed t d = box t d
boxy Unboxed Boxed t d = unbox t d
boxy _ _ _ d = d


unbox :: ValType -> Doc -> Doc
unbox t x
 = case t of
    IntT -> "(" <> x <> ").intValue()"
    DateTimeT -> unbox IntT x
    _    -> x

box :: ValType -> Doc -> Doc
box t x
 = case t of
    IntT -> "Integer.valueOf(" <> x <> ")"
    DateTimeT -> box IntT x
    _    -> x

boxyOfPrimReturn :: Prim -> Boxy
boxyOfPrimReturn p
 | PrimMinimal (M.PrimStruct _) <- p
 = Boxed
 | PrimMinimal (M.PrimPair _) <- p
 = Boxed
 | PrimProject (PrimProjectMapLookup _ _) <- p
 = Boxed
 | PrimUnsafe (PrimUnsafeArrayIndex _) <- p
 = Boxed
 | PrimUnsafe (PrimUnsafeMapIndex _ _) <- p
 = Boxed
 | PrimMinimal (M.PrimConst (M.PrimConstSome _)) <- p
 = Boxed
 | PrimUnsafe (PrimUnsafeOptionGet _) <- p
 = Boxed
 | otherwise
 = Unboxed

boxyOfPrimArgs :: Prim -> Boxy
boxyOfPrimArgs p
 | PrimMinimal (M.PrimConst _) <- p
 = Boxed
 | PrimProject (PrimProjectMapLookup _ _) <- p
 = Boxed
 | PrimMinimal (M.PrimStruct _) <- p
 = Boxed
 | PrimUpdate _ <- p
 = Boxed
 | otherwise
 = Unboxed

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
     DateTimeT  -> "Integer"
     ArrayT a   -> "ArrayList" <> angled (boxedType a)
     MapT a b   -> "HashMap" <> angled (commas [boxedType a, boxedType b])
     OptionT a  -> boxedType a
     PairT a b  -> "Pair" <> angled (commas [boxedType a, boxedType b])
     -- ???
     StructT _  -> "IcicleStruct"
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
