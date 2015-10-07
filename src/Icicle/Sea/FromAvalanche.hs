{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche (
    seaOfProgram
  , stateWordsOfProgram
  , accumsOfProgram
  , outputsOfProgram
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
import qualified Icicle.Internal.Pretty as Pretty

import           P

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map


------------------------------------------------------------------------

seaOfProgram :: (Show a, Show n, Pretty n, Ord n)
             => Program (Annot a) n Prim -> Doc
seaOfProgram program = vsep
  [ "#include <stdbool.h>"
  , "#include <stdint.h>"
  , "#include <math.h>"
  , ""
  , "typedef uint64_t iunit_t;"
  , "typedef uint64_t ibool_t;"
  , "typedef  int64_t iint_t;"
  , "typedef   double idouble_t;"
  , "typedef  int64_t idate_t;"
  , ""
  , "typedef const char *ierror_t;"
  , ""
  , stateOfProgram program
  , ""
  , "static const iunit_t iunit  = 0x1c1c13;"
  , "static const ibool_t ifalse = 0;"
  , "static const ibool_t itrue  = 1;"
  , ""
  , "#define INLINE __attribute__((always_inline))"
  , ""
  , "static idouble_t INLINE iint_extend   (iint_t    x)              { return x; }"
  , "static iint_t    INLINE iint_add      (iint_t    x, iint_t    y) { return x +  y; }"
  , "static iint_t    INLINE iint_sub      (iint_t    x, iint_t    y) { return x -  y; }"
  , "static iint_t    INLINE iint_mul      (iint_t    x, iint_t    y) { return x *  y; }"
  , "static ibool_t   INLINE iint_gt       (iint_t    x, iint_t    y) { return x >  y; }"
  , "static ibool_t   INLINE iint_ge       (iint_t    x, iint_t    y) { return x >= y; }"
  , "static ibool_t   INLINE iint_lt       (iint_t    x, iint_t    y) { return x <  y; }"
  , "static ibool_t   INLINE iint_le       (iint_t    x, iint_t    y) { return x <= y; }"
  , "static ibool_t   INLINE iint_eq       (iint_t    x, iint_t    y) { return x == y; }"
  , "static ibool_t   INLINE iint_ne       (iint_t    x, iint_t    y) { return x != y; }"
  , ""
  , "static iint_t    INLINE idouble_trunc (idouble_t x)              { return (iint_t)x; }"
  , "static idouble_t INLINE idouble_add   (idouble_t x, idouble_t y) { return x + y; }"
  , "static idouble_t INLINE idouble_sub   (idouble_t x, idouble_t y) { return x - y; }"
  , "static idouble_t INLINE idouble_mul   (idouble_t x, idouble_t y) { return x * y; }"
  , "static idouble_t INLINE idouble_pow   (idouble_t x, idouble_t y) { return pow(x, y); }"
  , "static idouble_t INLINE idouble_div   (idouble_t x, idouble_t y) { return x / y; }"
  , "static idouble_t INLINE idouble_log   (idouble_t x)              { return log(x); }"
  , "static idouble_t INLINE idouble_exp   (idouble_t x)              { return exp(x); }"
  , "static ibool_t   INLINE idouble_gt    (idouble_t x, idouble_t y) { return x >  y; }"
  , "static ibool_t   INLINE idouble_ge    (idouble_t x, idouble_t y) { return x >= y; }"
  , "static ibool_t   INLINE idouble_lt    (idouble_t x, idouble_t y) { return x <  y; }"
  , "static ibool_t   INLINE idouble_le    (idouble_t x, idouble_t y) { return x <= y; }"
  , "static ibool_t   INLINE idouble_eq    (idouble_t x, idouble_t y) { return x == y; }"
  , "static ibool_t   INLINE idouble_ne    (idouble_t x, idouble_t y) { return x != y; }"
  , ""
  , "static iint_t INLINE iint_err (icicle_state_t *s, ierror_t error)  {"
  , "    s->error = error;"
  , "    return 0xBAD1c3;"
  , "}"
  , ""
  , "static idouble_t INLINE idouble_err (icicle_state_t *s, ierror_t error)  {"
  , "    s->error = error;"
  , "    return 0/0;"
  , "}"
  , ""
  , "void compute(icicle_state_t *s)"
  , "{"
  , indent 4 . vsep
             . fmap defOfAccumulator
             . Map.toList
             $ accumsOfProgram program `Map.union`
               readsOfProgram  program
  , ""
  , indent 4 (seaOfStatement (statements program))
  , "}"
  ]


------------------------------------------------------------------------

stateOfProgram :: (Show a, Show n, Pretty n, Ord n)
               => Program (Annot a) n Prim -> Doc
stateOfProgram program = vsep
   [ "typedef struct {"
   , "    /* inputs */"
   , "    idate_t    gen_date;"
   , "    iint_t     new_count;"
   , "    idate_t   *new_date;"
   , "    iint_t    *new_fact;"
   , ""
   , "    /* outputs */"
   , "    ierror_t   error;"
   , indent 4 . vsep
              . fmap defOfOutput
              . Map.toList
              . outputsOfProgram
              $ program
   , ""
   , "    /* resumables */"
   , indent 4 . vsep
              . fmap defOfAccumulator
              . Map.toList
              . accumsOfProgram
              $ program
   , "} icicle_state_t;"
   ]

stateWordsOfProgram :: Ord n => Program (Annot a) n Prim -> Int
stateWordsOfProgram program
 = 1 -- gen_date
 + 1 -- new_count
 + 1 -- new_date
 + 1 -- new_fact
 + 1 -- error
 + Map.size (outputsOfProgram program)
 + Map.size (accumsOfProgram  program)

defOfAccumulator :: (Show n, Pretty n, Ord n)
                  => (Name n, (AccumulatorType, ValType)) -> Doc
defOfAccumulator (n, (at, vt))
 = case at of
     Mutable
      -> seaOfValType vt <+> seaOfName n <> semi
     Latest
      -> seaError "defOfAccumulator" (n, at, vt)

defOfOutput :: (OutputName, ValType) -> Doc
defOfOutput (n, t)
 = seaOfValType t <+> seaOfName n <> semi

------------------------------------------------------------------------

seaOfStatement :: (Show a, Show n, Pretty n, Ord n)
               => Statement (Annot a) n Prim -> Doc
seaOfStatement stmt
 = case stmt of
     Block []
      -> Pretty.empty

     Block (s:[])
      -> seaOfStatement s

     Block (s:ss)
      -> seaOfStatement s <> line
      <> seaOfStatement (Block ss)

     Let n xx stmt'
      | Just xt <- valTypeOfExp xx
      -> assign (seaOfValType xt <+> seaOfName n) (seaOfExp xx) <> semi <> line
      <> seaOfStatement stmt'

     If ii tt (Block [])
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement tt)
              , "}"
              , ""
              ]

     If ii tt ee
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement tt)
              , "} else {"
              , indent 4 (seaOfStatement ee)
              , "}"
              , ""
              ]

     ForeachFacts n_fact n_date vt lt stmt'
      | FactLoopNew <- lt
      , dt          <- DateTimeT
      -> vsep [ ""
              , assign ("const " <> seaOfValType IntT
                                 <> "        new_count") "s->new_count;"
              , assign ("const " <> seaOfValType dt
                                 <> " *const new_date")  "s->new_date;"
              , assign ("const " <> seaOfValType vt
                                 <> " *const new_fact")  "s->new_fact;"
              , ""
              , "for (iint_t i = 0; i < new_count; i++) {"
              , indent 4 $ assign (seaOfValType dt <+> seaOfName n_date) "new_date[i]" <> semi <> line
                        <> assign (seaOfValType vt <+> seaOfName n_fact) "new_fact[i]" <> semi <> line
                        <> seaOfStatement stmt'
              , "}"
              , ""
              ]

     InitAccumulator acc stmt'
      | Accumulator n Mutable _ xx <- acc
      -> assign (seaOfName n) (seaOfExp xx) <> semi <> line
      <> seaOfStatement stmt'

     Read n_val n_acc at _ stmt'
      | Mutable <- at
      -> assign (seaOfName n_val) (seaOfName n_acc) <> semi <> line
      <> seaOfStatement stmt'

     Write n xx
      -> assign (seaOfName n) (seaOfExp xx) <> semi

     LoadResumable n _
      -> assign (seaOfName n) ("s->" <> seaOfName n) <> semi

     SaveResumable n _
      -> assign ("s->" <> seaOfName n) (seaOfName n) <> semi

     Output n xx
      -> assign ("s->" <> seaOfName n) (seaOfExp xx) <> semi

     _
      -> seaError "seaOfStatement" stmt


------------------------------------------------------------------------

seaOfExp :: (Show a, Show n, Pretty n, Ord n)
         => Exp (Annot a) n Prim -> Doc
seaOfExp xx
 = case xx of
     XValue _ _ v
      | Just t <- valTypeOfExp xx
      -> seaOfXValue t v

     XVar _ n
      -> seaOfName n

     XApp{}
      | Just (p, xs) <- takePrimApps xx
      -> seaOfXPrim p <+> tuple (fmap seaOfExp xs)

     _
      -> seaError "seaOfExp" xx

seaOfXValue :: ValType -> BaseValue -> Doc
seaOfXValue t v
 = case v of
     VBool   True  -> "itrue"
     VBool   False -> "ifalse"
     VInt    x     -> int x
     VDouble x     -> double x

     -- TODO C escapes /= Haskell escapes
     VString x     -> text (show x)

     VError msg
      | IntT <- t
      -> "iint_err (s, \"" <> text (show msg) <> "\")"

      | DoubleT <- t
      -> "idouble_err (s, \"" <> text (show msg) <> "\")"
     _
      -> seaError "seaOfXValue" v

seaOfXPrim :: Prim -> Doc
seaOfXPrim p
 = case p of
     PrimMinimal (M.PrimArithBinary op t)
      -> prefixOfArithType t <> seaOfPrimArithBinary op

     PrimMinimal (M.PrimDouble op)
      -> prefixOfValType DoubleT <> seaOfPrimDouble op

     PrimMinimal (M.PrimCast op)
      -> seaOfPrimCast op

     PrimMinimal (M.PrimRelation op t)
      -> prefixOfValType t <> seaOfPrimRelation op

     _
      -> seaError "seaOfXPrim" p

seaOfPrimArithBinary :: M.PrimArithBinary -> Doc
seaOfPrimArithBinary p
 = case p of
     M.PrimArithPlus  -> "add"
     M.PrimArithMinus -> "sub"
     M.PrimArithMul   -> "mul"
     M.PrimArithPow   -> "pow"

seaOfPrimDouble :: M.PrimDouble -> Doc
seaOfPrimDouble p
 = case p of
     M.PrimDoubleDiv -> "div"
     M.PrimDoubleLog -> "log"
     M.PrimDoubleExp -> "exp"

seaOfPrimCast :: M.PrimCast -> Doc
seaOfPrimCast p
 = case p of
     M.PrimCastDoubleOfInt -> "iint_extend"
     M.PrimCastIntOfDouble -> "idouble_trunc"
     _                     -> seaError "seaOfPrimCast" p

seaOfPrimRelation :: M.PrimRelation -> Doc
seaOfPrimRelation p
 = case p of
     M.PrimRelationGt -> "gt"
     M.PrimRelationGe -> "ge"
     M.PrimRelationLt -> "lt"
     M.PrimRelationLe -> "le"
     M.PrimRelationEq -> "eq"
     M.PrimRelationNe -> "ne"

prefixOfArithType :: ArithType -> Doc
prefixOfArithType t
 = case t of
     ArithIntT    -> prefixOfValType IntT
     ArithDoubleT -> prefixOfValType DoubleT

prefixOfValType :: ValType -> Doc
prefixOfValType t
 = case t of
     BoolT     -> "ibool_"
     IntT      -> "iint_"
     DoubleT   -> "idouble_"
     DateTimeT -> "idate_"
     _         -> seaError "prefixOfValType" t

------------------------------------------------------------------------

seaOfValType :: ValType -> Doc
seaOfValType t
 = case t of
     UnitT     -> "iunit_t   "
     BoolT     -> "ibool_t   "
     IntT      -> "iint_t    "
     DoubleT   -> "idouble_t "
     StringT   -> "istring_t "
     DateTimeT -> "idate_t   "
     StructT _ -> "istruct_t "
     _         -> seaError "seaOfValType" t

valTypeOfExp :: Exp (Annot a) n p -> Maybe ValType
valTypeOfExp = unFun . annType . annotOfExp
  where
    unFun (FunT [] t) = Just t
    unFun _           = Nothing

------------------------------------------------------------------------

seaOfName :: Pretty n => n -> Doc
seaOfName = string . fmap mangle . show . pretty
  where
    mangle '$' = '_'
    mangle  c  =  c

------------------------------------------------------------------------

seaError :: Show a => Doc -> a -> Doc
seaError msg x = line <> "#error Failed during codegen (" <> msg <> ": " <> str <> "..)" <> line
  where
    str = string (List.take 40 (show x))

assign :: Doc -> Doc -> Doc
assign x y = x <> column (\k -> indent (40-k) " =") <+> y

tuple :: [Doc] -> Doc
tuple []  = "()"
tuple [x] = "(" <> x <> ")"
tuple xs  = "(" <> go xs
  where
    go []     = ")" -- impossible
    go (y:[]) = y <> ")"
    go (y:ys) = y <> ", " <> go ys

------------------------------------------------------------------------
-- Analysis

accumsOfProgram :: Ord n => Program (Annot a) n Prim -> Map (Name n) (AccumulatorType, ValType)
accumsOfProgram = accumsOfStatement . statements

accumsOfStatement :: Ord n => Statement (Annot a) n Prim -> Map (Name n) (AccumulatorType, ValType)
accumsOfStatement stmt
 = case stmt of
     Block []                -> Map.empty
     Block (s:ss)            -> accumsOfStatement s `Map.union`
                                accumsOfStatement (Block ss)
     Let _ _ ss              -> accumsOfStatement ss
     If _ tt ee              -> accumsOfStatement tt `Map.union`
                                accumsOfStatement ee
     ForeachInts    _ _ _ ss -> accumsOfStatement ss
     ForeachFacts _ _ _ _ ss -> accumsOfStatement ss
     Read _ _ _ _ ss         -> accumsOfStatement ss
     Write _ _               -> Map.empty
     Push  _ _               -> Map.empty
     LoadResumable _ _       -> Map.empty
     SaveResumable _ _       -> Map.empty
     Output _ _              -> Map.empty
     KeepFactInHistory       -> Map.empty

     InitAccumulator (Accumulator n at avt _) ss
      -> Map.singleton n (at, avt) `Map.union`
         accumsOfStatement ss

------------------------------------------------------------------------

readsOfProgram :: Ord n => Program (Annot a) n Prim -> Map (Name n) (AccumulatorType, ValType)
readsOfProgram = readsOfStatement . statements

readsOfStatement :: Ord n => Statement (Annot a) n Prim -> Map (Name n) (AccumulatorType, ValType)
readsOfStatement stmt
 = case stmt of
     Block []                -> Map.empty
     Block (s:ss)            -> readsOfStatement s `Map.union`
                                readsOfStatement (Block ss)
     Let _ _ ss              -> readsOfStatement ss
     If _ tt ee              -> readsOfStatement tt `Map.union`
                                readsOfStatement ee
     ForeachInts    _ _ _ ss -> readsOfStatement ss
     ForeachFacts _ _ _ _ ss -> readsOfStatement ss
     InitAccumulator _ ss    -> readsOfStatement ss
     Write _ _               -> Map.empty
     Push  _ _               -> Map.empty
     LoadResumable _ _       -> Map.empty
     SaveResumable _ _       -> Map.empty
     Output _ _              -> Map.empty
     KeepFactInHistory       -> Map.empty

     Read n _ at vt ss
      -> Map.singleton n (at, vt) `Map.union`
         readsOfStatement ss

------------------------------------------------------------------------

outputsOfProgram :: Program (Annot a) n Prim -> Map OutputName ValType
outputsOfProgram = outputsOfStatement . statements

outputsOfStatement :: Statement (Annot a) n Prim -> Map OutputName ValType
outputsOfStatement stmt
 = case stmt of
     Block []                -> Map.empty
     Block (s:ss)            -> outputsOfStatement s `Map.union`
                                outputsOfStatement (Block ss)
     Let _ _ ss              -> outputsOfStatement ss
     If _ tt ee              -> outputsOfStatement tt `Map.union`
                                outputsOfStatement ee
     ForeachInts    _ _ _ ss -> outputsOfStatement ss
     ForeachFacts _ _ _ _ ss -> outputsOfStatement ss
     InitAccumulator _ ss    -> outputsOfStatement ss
     Read _ _ _ _ ss         -> outputsOfStatement ss
     Write _ _               -> Map.empty
     Push  _ _               -> Map.empty
     LoadResumable _ _       -> Map.empty
     SaveResumable _ _       -> Map.empty
     KeepFactInHistory       -> Map.empty

     Output n xx
      | Just t <- valTypeOfExp xx
      -> Map.singleton n t

      | otherwise
      -> Map.empty
