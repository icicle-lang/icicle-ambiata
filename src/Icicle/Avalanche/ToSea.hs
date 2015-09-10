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
import qualified Icicle.Internal.Pretty as Pretty

import           P

import           Data.Functor.Identity
import           Data.List (take)

import qualified Data.Map as Map


------------------------------------------------------------------------

seaOfProgram :: (Show a, Show n, Pretty n, Ord n)
             => Program (Annot a) n Prim -> Doc
seaOfProgram program = vsep [
    "#include <stdbool.h>"
  , "#include <stdint.h>"
  , ""
  , "#define ICICLE_TRUE  (0x1c1cl3)"
  , "#define ICICLE_FALSE (0x000000)"
  , ""
  , "#define ICICLE_MUL(x,y) ((x)*(y))"
  , ""
  , "typedef int64_t      unit64_t;"
  , "typedef int64_t      bool64_t;"
  , "typedef int64_t      date64_t;"
  , "typedef const char  *error_t;"
  , ""
  , "void compute(struct icicle_state *s)"
  , "{"
  , indent 4 (seaOfStatement (statements program))
  , "}"
  ]


------------------------------------------------------------------------

-- data Accumulator a n p
--  = Accumulator
--  { accName      :: Name n
--  , accKind      :: AccumulatorType
--  , accValType   :: ValType
--  , accInit      :: Exp a n p
--  }

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

     If ii tt ee
      -> vsep [ ""
              , "if (" <> seaOfExp ii <> ") {"
              , indent 4 (seaOfStatement tt)
              , "} else {"
              , indent 4 (seaOfStatement ee)
              , "}"
              ]

     ForeachFacts n_fact n_date vt lt stmt'
      | FactLoopNew <- lt
      , dt          <- DateTimeT
      -> vsep [ ""
              , assign ("const " <> seaOfValType IntT
                                 <> "       new_count") "s->new_count;"
              , assign ("const " <> seaOfValType vt
                                 <> " *const new_fact") "s->new_fact;"
              , assign ("const " <> seaOfValType dt
                                 <> " *const new_date") "s->new_date;"
              , ""
              , "for (int64_t i = 0; i < new_count; i++) {"
              , indent 4 $ assign (seaOfValType vt <+> seaOfName n_fact) "new_fact[i]" <> semi <> line
                        <> assign (seaOfValType dt <+> seaOfName n_date) "new_date[i]" <> semi <> line
                        <> seaOfStatement stmt'
              , "}"
              , ""
              ]

     InitAccumulator acc stmt'
      | Accumulator n Mutable vt xx <- acc
      -> assign (seaOfValType vt <+> seaOfName n) (seaOfExp xx) <> semi <> line
      <> seaOfStatement stmt'

     Read n_val n_acc at avt stmt'
      | Mutable <- at
      -> assign (seaOfValType avt <+> seaOfName n_val) (seaOfName n_acc) <> semi <> line
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
      -> seaOfBaseValue v

     XVar _ n
      -> seaOfName n

     XApp{}
      | Just (p, xs) <- takePrimApps xx
      -> seaOfPrim p <> tupled (fmap seaOfExp xs)

     _
      -> seaError "seaOfExp" xx


seaOfPrim :: Prim -> Doc
seaOfPrim p
 = case p of
     PrimMinimal (M.PrimArithBinary M.PrimArithMul _)
      -> "ICICLE_MUL"
     _
      -> seaError "seaOfPrim" p

------------------------------------------------------------------------

seaOfBaseValue :: BaseValue -> Doc
seaOfBaseValue v
 = case v of
     VBool True     -> "ICICLE_TRUE"
     VBool False    -> "ICICLE_FALSE"
     VInt  x        -> int x
     VException msg -> "0xDEADBEEF /*" <+> string (show msg) <+> "*/"
     _              -> seaError "seaOfBaseValue" v

seaOfValType :: ValType -> Doc
seaOfValType t
 = case t of
     UnitT     -> "unit64_t"
     BoolT     -> "bool64_t"
     IntT      -> "int64_t "
     DoubleT   -> "double  "
     DateTimeT -> "date64_t"
     _         -> seaError "seaOfValType" t

seaOfName :: Pretty n => n -> Doc
seaOfName = string . fmap mangle . show . pretty
  where
    mangle '$' = '_'
    mangle  c  =  c


------------------------------------------------------------------------

valTypeOfExp :: Exp (Annot a) n p -> Maybe ValType
valTypeOfExp = unFun . annType . annotOfExp
  where
    unFun (FunT [] t) = Just t
    unFun _           = Nothing

seaError :: Show a => Doc -> a -> Doc
seaError msg x = line <> "#error Failed during codegen (" <> msg <> ": " <> str <> "..)" <> line
  where
    str = string (take 30 (show x))

assign :: Doc -> Doc -> Doc
--assign x y = fill 30 x <> text "=" <+> y
assign x y = x <> column (\k -> indent (40-k) " =") <+> y
