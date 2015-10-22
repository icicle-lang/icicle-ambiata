{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Icicle.Avalanche.ToJava (
    programToJava
  ) where

import              Icicle.Avalanche.Prim.Flat
import              Icicle.Avalanche.Program
import qualified    Icicle.Avalanche.Statement.Statement as S
import              Icicle.Avalanche.Statement.Scoped

import              Icicle.Common.Annot
import              Icicle.Common.Base
import              Icicle.Common.Exp
import qualified    Icicle.Common.Exp.Prim.Minimal as M
import              Icicle.Common.Type

import              Icicle.Internal.Pretty

import              P

import              Data.Functor.Identity

import qualified    Data.Map    as Map


programToJava :: (Show a, Show n, Pretty n, Ord n) => Program (Annot a) n Prim -> Doc
programToJava p
 = "import java.util.*;"
 <> line
 <> "class Feature"
 <> block
 [ "public void compute(IcicleState" <> angled (maybe "$#@! NO FEATURE LOOP" boxedType $ concreteFeatureType $ statements p) <> " icicle)"
 <> block
    [ local DateTimeT (binddate p) <> " = icicle.snapshotDate();"
    , ""
    , statementsToJava (scopedOfStatement $ statements p)
    ]
 ]

concreteFeatureType :: S.Statement a n p -> Maybe ValType
concreteFeatureType ss
 = runIdentity
 $ S.foldStmt (\_ _ -> return ()) up orl () Nothing ss
 where
  up _ r s
   = case s of
      S.ForeachFacts _ ty _ _
       -> return (Just ty)
      _
       -> return r

  orl (Just l) _ = Just l
  orl _ r = r


statementsToJava :: (Show a, Show n, Pretty n, Ord n) => Scoped (Annot a) n Prim -> Doc
statementsToJava ss
 = case ss of
    If x t e
     -> "if (" <> expToJava Unboxed x <> ")"
     <> block [statementsToJava t]
     <> (case e of
          Block [] -> ""
          If{}     -> line <> "else " <>        statementsToJava e
          _        -> line <> "else"  <> block [statementsToJava e])

    -- NB. it looks like we are recomputing "to" on every iteration,
    -- however if input program is in a-normal form, it will already
    -- be pulled out to a variable binding.
    ForeachInts n from to s
     -> "for (" <> local IntT n <> " = " <> expToJava Unboxed from <> "; "
                <> name n <> " < " <> expToJava Unboxed to <> "; "
                <> name n <> "++)" <> block [statementsToJava s]

    ForeachFacts ns _ f s
     -> let readVar (n, t) = local t n <> " = " <> unbox t ("icicle.currentRow(\"" <> pretty n <> "\")") <> ";"
        in (case f of
             S.FactLoopHistory -> "icicle.startHistory();"
             S.FactLoopNew     -> "icicle.startNew();")
           <> line
           <> "while (icicle.nextRow())"
           <> block (fmap readVar ns <> [statementsToJava s])
    Block blocks
     -> vcat (fmap (either bindingToJava statementsToJava) blocks)

    Write n x
     -> acc_name n <> " = " <> expToJava Unboxed x <> ";"

    Push n x
     -> "icicle.pushLatest(" <> acc_name n <> ", " <> expToJava Boxed x <> ");"

    Output n _ _
     -> "icicle.output(" <> stringy n <> ");"

    KeepFactInHistory
     -> "icicle.keepFactInHistory();"

    LoadResumable n t
     -> let nm = "LOAD$" <> name n
        in   boxedType t <> " " <> nm <> " = " <> "icicle." <> angled (boxedType t) <> "loadResumable(\"feature\", " <> stringy n <> ");"
          <> line
          <> "if (" <> nm <> " != null)"
          <> block [ acc_name n <> " = " <> unbox t nm <> ";" ]

    SaveResumable n t
     -> "icicle.saveResumable(\"feature\", " <> stringy n <> ", " <> box t (acc_name n) <> ");"


bindingToJava :: (Show a, Show n, Pretty n, Ord n) => Binding (Annot a) n Prim -> Doc
bindingToJava bb
 = case bb of
    InitAccumulator acc@(S.Accumulator { S.accKind = S.Latest })
     -> "Latest" <> angled (boxedType $ S.accValType acc)
     <+> (acc_name $ S.accName acc)
     <> " = icicle."
     <> "makeLatest"
     <> "(" <> expToJava Unboxed (S.accInit acc) <> ");"

    InitAccumulator acc@(S.Accumulator { S.accKind = S.Mutable })
     -> unboxedType (S.accValType acc) <+> (acc_name $ S.accName acc)
     <> " = " <> expToJava Unboxed (S.accInit acc) <> ";"

    Let n x
     -> local (typeOfExp x) n <> " = " <> expToJava Unboxed x <> ";"

    Read n acc S.Latest t
     -> local (ArrayT t) n <> " = icicle.readLatest(" <> acc_name acc <> ");"

    Read n acc S.Mutable t
     -> local t n <> " = " <> acc_name acc <> ";"


expToJava :: (Show a, Show n, Pretty n, Ord n) => Boxy -> Exp (Annot a) n Prim -> Doc
expToJava b xx
 = let FunT _ t = annType (annotOfExp xx)
   in  case xx of
            XVar _ n
             -> boxy b Unboxed t
              $ name n

            XValue a _ v
             -> case v of
                 VInt    i -> boxy b Unboxed t $ pretty i
                 VDouble i -> boxy b Unboxed t $ pretty i
                 VUnit     -> boxy b Unboxed t $ "13013"
                 VMap m
                  | Map.null m
                  -> "IcicleMap.empty()"
                 VArray[]-> "Array.empty()"
                 VNone   -> "null"
                 VSome v'
                  | OptionT t' <- t
                  -> let a' = Annot (annType a) ()
                         xv = XValue a' t' v' :: Exp (Annot ()) () Prim
                     in  expToJava Boxed xv
                 _      -> "$#@! TODO VALUE " <> pretty v

            XPrim _ p
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
                     (fmap (expToJava $ boxyOfPrimArgs p) args)


typeOfExp :: Exp (Annot a) n p -> ValType
typeOfExp x
 | FunT _ t <- annType (annotOfExp x)
 = t


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
    PrimPack (PrimOptionPack _)
     -> Function "Option.pack"
    PrimPack (PrimSumPack _ _)
     -> Function "Sum.pack"
    PrimBuf pb
     -> buf pb

 where
  min' (M.PrimArithUnary ar _) = unary ar
  min' (M.PrimArithBinary ar _) = binary ar
  min' (M.PrimDouble ar) = dble   ar
  min' (M.PrimCast ar) = cast ar
  min' (M.PrimRelation re _) = rel re
  min' (M.PrimLogical lo) = logic lo

  min' (M.PrimConst (M.PrimConstPair _ _))
   = Function "Pair.create"
  min' (M.PrimConst (M.PrimConstSome _))
   = Function ""
  min' (M.PrimConst (M.PrimConstLeft _ _))
   = Function "Either.left"
  min' (M.PrimConst (M.PrimConstRight _ _))
   = Function "Either.right"
  min' (M.PrimDateTime M.PrimDateTimeDaysDifference)
   = Function "icicle.daysDifference"
  min' (M.PrimDateTime M.PrimDateTimeDaysEpoch)
   = Function "icicle.daysEpoch"
  min' (M.PrimDateTime M.PrimDateTimeMinusDays)
   = Function "icicle.minusDays"
  min' (M.PrimDateTime M.PrimDateTimeMinusMonths)
   = Function "icicle.minusMonths"

  min' (M.PrimPair (M.PrimPairFst _ _))
   = Method "fst"
  min' (M.PrimPair (M.PrimPairSnd _ _))
   = Method "snd"

  min' (M.PrimStruct (M.PrimStructGet f t _))
   = Special1 $ \a -> a <> "." <> angled (boxedType t) <> "getField" <> "(" <> stringy f <> ")"

  unary   M.PrimArithNegate = Prefix    "-"

  binary   M.PrimArithPlus   = Infix     "+"
  binary   M.PrimArithMinus  = Infix     "-"
  binary   M.PrimArithPow    = Function     "Math.pow"
  binary   M.PrimArithMul    = Infix     "*"

  dble     M.PrimDoubleDiv  = Infix "/"
  dble     M.PrimDoubleLog  = Function "Math.log"
  dble     M.PrimDoubleExp  = Function "Math.exp"

  cast      M.PrimCastIntOfDouble = Function "(int)"
  cast      M.PrimCastDoubleOfInt = Function "(double)"
  cast      M.PrimCastStringOfInt = Function "Integer.toString"
  cast      M.PrimCastStringOfDouble = Function "Double.toString"

  rel   M.PrimRelationGt  = Infix     ">"
  rel   M.PrimRelationGe  = Infix     ">="
  rel   M.PrimRelationLt  = Infix     "<"
  rel   M.PrimRelationLe  = Infix     "<="
  rel   M.PrimRelationEq  = Infix     "=="
  rel   M.PrimRelationNe  = Infix     "!="

  logic   M.PrimLogicalNot  = Prefix    "!"
  logic   M.PrimLogicalAnd  = Infix     "&&"
  logic   M.PrimLogicalOr   = Infix     "||"

  proj (PrimProjectArrayLength _) = Method "size"
  proj (PrimProjectMapLength _ _) = Method "size"
  proj (PrimProjectMapLookup _ _) = Method "get"
  proj (PrimProjectOptionIsSome _)= Special1 $ \a -> a <> " != null"
  proj (PrimProjectSumIsRight _ _) = Method "isRight"

  unsa (PrimUnsafeArrayIndex _)    = Method "get"
  unsa (PrimUnsafeArrayCreate t)   = Function ("new ArrayList" <> angled (boxedType t))
  unsa (PrimUnsafeMapIndex _ _)    = Function "IcicleMap.getByIndex"
  unsa (PrimUnsafeOptionGet _)     = Special1 $ \a -> a
  unsa (PrimUnsafeSumGetLeft  _ _) = Method "left"
  unsa (PrimUnsafeSumGetRight _ _) = Method "right"

  upda (PrimUpdateMapPut _ _)      = Function "IcicleMap.put"
  upda (PrimUpdateArrayPut _)      = Function "Array.put"

  buf (PrimBufMake t) = Function $ "new IcicleBuf" <> angled (boxedType t)
  buf (PrimBufPush _) = Method "push"
  buf (PrimBufRead _) = Method "read"

data Boxy = Boxed | Unboxed

boxy :: Boxy -> Boxy -> ValType -> Doc -> Doc
boxy Boxed Unboxed t d = box t d
boxy Unboxed Boxed t d = unbox t d
boxy _ _ _ d = d


unbox :: ValType -> Doc -> Doc
unbox t x
 = case t of
    IntT -> "(" <> x <> ").intValue()"
    DoubleT -> "(" <> x <> ").doubleValue()"
    DateTimeT -> unbox IntT x
    _    -> x

box :: ValType -> Doc -> Doc
box t x
 = case t of
    IntT -> "Integer.valueOf(" <> x <> ")"
    DoubleT -> "Double.valueOf(" <> x <> ")"
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
     DoubleT    -> "Double"
     UnitT      -> "Integer"
     ErrorT     -> "Error"
     BoolT      -> "Boolean"
     DateTimeT  -> "Integer"
     ArrayT a   -> "ArrayList" <> angled (boxedType a)
     BufT   a   -> "IcicleBuf" <> angled (boxedType a)
     MapT a b   -> "HashMap" <> angled (commas [boxedType a, boxedType b])
     OptionT a  -> boxedType a
     PairT a b  -> "Pair" <> angled (commas [boxedType a, boxedType b])
     SumT  a b  -> "Either" <> angled (commas [boxedType a, boxedType b])
     -- ???
     StructT _  -> "IcicleStruct"
     StringT    -> "String"

unboxedType :: ValType -> Doc
unboxedType t
 = case t of
     IntT       -> "int"
     DoubleT    -> "double"
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
