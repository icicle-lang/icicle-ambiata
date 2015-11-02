{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche (
    seaOfProgram
  , stateWordsOfProgram
  , factVarsOfProgram
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

import           Icicle.Sea.Preamble

import           P

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map


------------------------------------------------------------------------

seaOfProgram :: (Show a, Show n, Pretty n, Ord n)
             => Program (Annot a) n Prim -> Doc
seaOfProgram program
 =  seaPreamble
 <> vsep
  [ "#line 1 \"state definition\""
  , stateOfProgram program
  , ""
  , "#line 1 \"compute function\""
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
   , indent 4 . vsep
              . fmap defOfFactVar
              . maybe [] snd
              . factVarsOfProgram FactLoopNew
              $ program
   , ""
   , "    /* outputs */"
   , indent 4 . vsep
              . concat
              . fmap defsOfOutput
              . outputsOfProgram
              $ program
   , ""
   , "    /* resumables */"
   , indent 4 . vsep
              . fmap defOfResumable
              . Map.toList
              . resumablesOfProgram
              $ program
   , "} icicle_state_t;"
   ]

stateWordsOfProgram :: Ord n => Program (Annot a) n Prim -> Int
stateWordsOfProgram program
 = 1 -- gen_date
 + 1 -- new_count
 + length (maybe [] snd (factVarsOfProgram FactLoopNew program))
 + sum (fmap (length . snd . snd) (outputsOfProgram program))
 + 2 * Map.size (resumablesOfProgram program)

defOfAccumulator :: (Show n, Pretty n, Ord n)
                  => (Name n, (AccumulatorType, ValType)) -> Doc
defOfAccumulator (n, (at, vt))
 = case at of
     Mutable
      -> seaOfValType vt <+> seaOfName n <> semi
     Latest
      -> seaError "defOfAccumulator" (n, at, vt)

defOfResumable :: (Show n, Pretty n, Ord n) => (Name n, ValType) -> Doc
defOfResumable (n, t)
 =  seaOfValType BoolT <+> "has_" <> seaOfName n <> semi <> line
 <> seaOfValType t     <+> "res_" <> seaOfName n <> semi

defOfFactVar :: Pretty n => (Name n, ValType) -> Doc
defOfFactVar (n, t)
 = seaOfValType t <+> "*" <> "new_" <> seaOfName n <> semi

defsOfOutput :: (OutputName, (ValType, [ValType])) -> [Doc]
defsOfOutput (n, (_, ts))
 = List.zipWith (defOfOutputIx n) [0..] ts

defOfOutputIx :: OutputName -> Int -> ValType -> Doc
defOfOutputIx n ix t
 = seaOfValType t <+> seaOfNameIx n ix <> semi

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
      -> assign (seaOfValType xt <+> seaOfName n) (seaOfExp xx) <> semi <> suffix "let" <> line
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

     ForeachFacts ns _ lt stmt'
      | FactLoopNew  <- lt
      , structAssign <- \(n, t) -> assign ("const " <> seaOfValType t <> "*const new_" <> seaOfName n)
                                          ("s->new_" <> seaOfName n) <> semi
      , loopAssign   <- \(n, t) -> assign (seaOfValType t <+> seaOfName n)
                                          ("new_" <> seaOfName n <> "[i]") <> semi
      -> vsep $ [ ""
                , assign ("const " <> seaOfValType IntT <> "new_count") "s->new_count;"
                ] <> fmap structAssign ns <>
                [ ""
                , "for (iint_t i = 0; i < new_count; i++) {"
                , indent 4 $ vsep (fmap loopAssign ns) <> line <> seaOfStatement stmt'
                , "}"
                , ""
                ]

     InitAccumulator acc stmt'
      | Accumulator n Mutable _ xx <- acc
      -> assign (seaOfName n) (seaOfExp xx) <> semi <> suffix "init" <> line
      <> seaOfStatement stmt'

     Read n_val n_acc at _ stmt'
      | Mutable <- at
      -> assign (seaOfName n_val) (seaOfName n_acc) <> semi <> suffix "read" <> line
      <> seaOfStatement stmt'

     Write n xx
      -> assign (seaOfName n) (seaOfExp xx) <> semi <> suffix "write"

     LoadResumable n _
      -> vsep [ ""
              , "if (s->has_" <> seaOfName n <> ") {"
              , indent 4 $ assign (seaOfName n) ("s->res_" <> seaOfName n) <> semi <> suffix "load"
              , "}" ]

     SaveResumable n _
      -> assign ("s->has_" <> seaOfName n) "itrue"       <> semi <> suffix "save" <> line
      <> assign ("s->res_" <> seaOfName n) (seaOfName n) <> semi <> suffix "save" <> line

     Output n _ xts
      | ixAssign <- \ix xx -> assign ("s->" <> seaOfNameIx n ix) (seaOfExp xx) <> semi <> suffix "output"
      -> vsep (List.zipWith ixAssign [0..] (fmap fst xts))

     _
      -> seaError "seaOfStatement" stmt


------------------------------------------------------------------------

seaOfExp :: (Show a, Show n, Pretty n, Ord n)
         => Exp (Annot a) n Prim -> Doc
seaOfExp xx
 = case xx of
     XValue _ _ v
      -> seaOfXValue v

     XVar _ n
      -> seaOfName n

     XApp{}
      | Just (p, xs) <- takePrimApps xx
      -> seaOfXPrim p <+> tuple (fmap seaOfExp xs)

     _
      -> seaError "seaOfExp" xx

seaOfXValue :: BaseValue -> Doc
seaOfXValue v
 = case v of
     VError  err   -> seaOfError err
     VBool   True  -> "itrue"
     VBool   False -> "ifalse"
     VInt    x     -> int x
     VDouble x     -> double x

     -- TODO C escapes /= Haskell escapes
     VString x     -> text (show x)

     _
      -> seaError "seaOfXValue" v

seaOfError :: ExceptionInfo -> Doc
seaOfError e
 = case e of
     ExceptTombstone
      -> "ierror_tombstone"

     ExceptFold1NoValue
      -> "ierror_fold1_no_value"

     ExceptScalarVariableNotAvailable
      -> "ierror_variable_not_available"

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

     PrimProject op
      -> seaOfPrimProject op

     PrimUnsafe op
      -> seaOfPrimUnsafe op

     PrimUpdate op
      -> seaOfPrimUpdate op

     PrimArray op
      -> seaOfPrimArray op

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

seaOfPrimProject :: PrimProject -> Doc
seaOfPrimProject p
 = case p of
     PrimProjectArrayLength t
      -> prefixOfValType t <> "length"
     _
      -> seaError "seaOfPrimProject" p

seaOfPrimUnsafe :: PrimUnsafe -> Doc
seaOfPrimUnsafe p
 = case p of
     PrimUnsafeArrayIndex t
      -> prefixOfValType t <> "index"
     PrimUnsafeArrayCreate t
      -> prefixOfValType t <> "create"
     _
      -> seaError "seaOfPrimUnsafe" p

seaOfPrimUpdate :: PrimUpdate -> Doc
seaOfPrimUpdate p
 = case p of
     PrimUpdateArrayPut t
      -> prefixOfValType t <> "put"
     _
      -> seaError "seaOfPrimUpdate" p

seaOfPrimArray :: PrimArray -> Doc
seaOfPrimArray p
 = case p of
     _
      -> seaError "seaOfPrimArray" p



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

     StringT   -> nope "strings not implemented"
     BufT{}    -> nope "buffers not implemented"
     ArrayT{}  -> "ARRAY_PREFIX(" <> prefixOfValType t <> ")"
     MapT{}    -> nope "maps not implemented"

     StructT{} -> nope "structs should have been melted"
     OptionT{} -> nope "options should have been melted"
     PairT{}   -> nope "pairs should have been melted"
     SumT{}    -> nope "sums should have been melted"

------------------------------------------------------------------------

seaOfValType :: ValType -> Doc
seaOfValType t
 = let nope = seaError "seaOfValType" . string
   in case t of
     UnitT     -> "iunit_t   "
     BoolT     -> "ibool_t   "
     IntT      -> "iint_t    "
     DoubleT   -> "idouble_t "
     DateTimeT -> "idate_t   "
     ErrorT    -> "ierror_t  "

     StringT   -> nope "strings not implemented"
     BufT{}    -> nope "buffers not implemented"
     ArrayT t' -> "ARRAY_OF(" <> seaOfValType t' <> ")"
     MapT{}    -> nope "maps not implemented"

     StructT{} -> nope "structs should have been melted"
     OptionT{} -> nope "options should have been melted"
     PairT{}   -> nope "pairs should have been melted"
     SumT{}    -> nope "sums should have been melted"

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

seaOfNameIx :: Pretty n => n -> Int -> Doc
seaOfNameIx n ix = seaOfName (pretty n <> text "$ix$" <> int ix)

------------------------------------------------------------------------

seaError :: Show a => Doc -> a -> Doc
seaError subject x = seaError' subject (string (List.take 40 (show x)))

seaError' :: Doc -> Doc -> Doc
seaError' subject msg = line <> "#error Failed during codegen (" <> subject <> ": " <> msg <> "..)" <> line

assign :: Doc -> Doc -> Doc
assign x y = x <> column (\k -> indent (40-k) " =") <+> y

suffix :: Doc -> Doc
suffix annot = column (\k -> indent (80-k) (" /*" <+> annot <+> "*/"))

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

factVarsOfProgram :: Ord n
                  => FactLoopType
                  -> Program (Annot a) n Prim
                  -> Maybe (ValType, [(Name n, ValType)])

factVarsOfProgram loopType = factVarsOfStatement loopType . statements


factVarsOfStatement :: Ord n
                    => FactLoopType
                    -> Statement (Annot a) n Prim
                    -> Maybe (ValType, [(Name n, ValType)])

factVarsOfStatement loopType stmt
 = case stmt of
     Block []              -> Nothing
     Block (s:ss)          -> factVarsOfStatement loopType s <|>
                              factVarsOfStatement loopType (Block ss)
     Let _ _ ss            -> factVarsOfStatement loopType ss
     If _ tt ee            -> factVarsOfStatement loopType tt <|>
                              factVarsOfStatement loopType ee
     ForeachInts  _ _ _ ss -> factVarsOfStatement loopType ss
     InitAccumulator _ ss  -> factVarsOfStatement loopType ss
     Read _ _ _ _ ss       -> factVarsOfStatement loopType ss
     Write _ _             -> Nothing
     Push  _ _             -> Nothing
     LoadResumable _ _     -> Nothing
     SaveResumable _ _     -> Nothing
     Output _ _ _          -> Nothing
     KeepFactInHistory     -> Nothing

     ForeachFacts ns vt lt ss
      | lt == loopType
      -> Just (vt, ns)

      | otherwise
      -> factVarsOfStatement loopType ss

------------------------------------------------------------------------

resumablesOfProgram :: Ord n => Program (Annot a) n Prim -> Map (Name n) ValType
resumablesOfProgram = resumablesOfStatement . statements

resumablesOfStatement :: Ord n => Statement (Annot a) n Prim -> Map (Name n) ValType
resumablesOfStatement stmt
 = case stmt of
     Block []              -> Map.empty
     Block (s:ss)          -> resumablesOfStatement s `Map.union`
                              resumablesOfStatement (Block ss)
     Let _ _ ss            -> resumablesOfStatement ss
     If _ tt ee            -> resumablesOfStatement tt `Map.union`
                              resumablesOfStatement ee
     ForeachInts  _ _ _ ss -> resumablesOfStatement ss
     ForeachFacts _ _ _ ss -> resumablesOfStatement ss
     InitAccumulator  _ ss -> resumablesOfStatement ss
     Read _ _ _ _ ss       -> resumablesOfStatement ss
     Write _ _             -> Map.empty
     Push  _ _             -> Map.empty
     Output _ _ _          -> Map.empty
     KeepFactInHistory     -> Map.empty

     LoadResumable n t     -> Map.singleton n t
     SaveResumable n t     -> Map.singleton n t

------------------------------------------------------------------------

accumsOfProgram :: Ord n => Program (Annot a) n Prim -> Map (Name n) (AccumulatorType, ValType)
accumsOfProgram = accumsOfStatement . statements

accumsOfStatement :: Ord n => Statement (Annot a) n Prim -> Map (Name n) (AccumulatorType, ValType)
accumsOfStatement stmt
 = case stmt of
     Block []              -> Map.empty
     Block (s:ss)          -> accumsOfStatement s `Map.union`
                              accumsOfStatement (Block ss)
     Let _ _ ss            -> accumsOfStatement ss
     If _ tt ee            -> accumsOfStatement tt `Map.union`
                              accumsOfStatement ee
     ForeachInts  _ _ _ ss -> accumsOfStatement ss
     ForeachFacts _ _ _ ss -> accumsOfStatement ss
     Read _ _ _ _ ss       -> accumsOfStatement ss
     Write _ _             -> Map.empty
     Push  _ _             -> Map.empty
     LoadResumable _ _     -> Map.empty
     SaveResumable _ _     -> Map.empty
     Output _ _ _          -> Map.empty
     KeepFactInHistory     -> Map.empty

     InitAccumulator (Accumulator n at avt _) ss
      -> Map.singleton n (at, avt) `Map.union`
         accumsOfStatement ss

------------------------------------------------------------------------

readsOfProgram :: Ord n => Program (Annot a) n Prim -> Map (Name n) (AccumulatorType, ValType)
readsOfProgram = readsOfStatement . statements

readsOfStatement :: Ord n => Statement (Annot a) n Prim -> Map (Name n) (AccumulatorType, ValType)
readsOfStatement stmt
 = case stmt of
     Block []              -> Map.empty
     Block (s:ss)          -> readsOfStatement s `Map.union`
                              readsOfStatement (Block ss)
     Let _ _ ss            -> readsOfStatement ss
     If _ tt ee            -> readsOfStatement tt `Map.union`
                              readsOfStatement ee
     ForeachInts  _ _ _ ss -> readsOfStatement ss
     ForeachFacts _ _ _ ss -> readsOfStatement ss
     InitAccumulator _ ss  -> readsOfStatement ss
     Write _ _             -> Map.empty
     Push  _ _             -> Map.empty
     LoadResumable _ _     -> Map.empty
     SaveResumable _ _     -> Map.empty
     Output _ _ _          -> Map.empty
     KeepFactInHistory     -> Map.empty

     Read n _ at vt ss
      -> Map.singleton n (at, vt) `Map.union`
         readsOfStatement ss

------------------------------------------------------------------------

outputsOfProgram :: Program (Annot a) n Prim -> [(OutputName, (ValType, [ValType]))]
outputsOfProgram = Map.toList . outputsOfStatement . statements

outputsOfStatement :: Statement (Annot a) n Prim -> Map OutputName (ValType, [ValType])
outputsOfStatement stmt
 = case stmt of
     Block []              -> Map.empty
     Block (s:ss)          -> outputsOfStatement s `Map.union`
                              outputsOfStatement (Block ss)
     Let _ _ ss            -> outputsOfStatement ss
     If _ tt ee            -> outputsOfStatement tt `Map.union`
                              outputsOfStatement ee
     ForeachInts  _ _ _ ss -> outputsOfStatement ss
     ForeachFacts _ _ _ ss -> outputsOfStatement ss
     InitAccumulator _ ss  -> outputsOfStatement ss
     Read _ _ _ _ ss       -> outputsOfStatement ss
     Write _ _             -> Map.empty
     Push  _ _             -> Map.empty
     LoadResumable _ _     -> Map.empty
     SaveResumable _ _     -> Map.empty
     KeepFactInHistory     -> Map.empty

     Output n t xts
      -> Map.singleton n (t, fmap snd xts)
