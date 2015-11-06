{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Icicle.Sea.FromAvalanche.Program (
    seaOfProgram
  , stateWordsOfProgram
  ) where

import           Icicle.Avalanche.Prim.Flat
import           Icicle.Avalanche.Program
import           Icicle.Avalanche.Statement.Statement

import           Icicle.Common.Annot
import           Icicle.Common.Base
import           Icicle.Common.Exp
import           Icicle.Common.Type

import           Icicle.Data.DateTime

import           Icicle.Internal.Pretty
import qualified Icicle.Internal.Pretty as Pretty

import           Icicle.Sea.FromAvalanche.Analysis
import           Icicle.Sea.FromAvalanche.Base
import           Icicle.Sea.FromAvalanche.Prim
import           Icicle.Sea.FromAvalanche.Type

import           P

import qualified Data.List as List
import qualified Data.Map as Map

import           Numeric (showHex)


------------------------------------------------------------------------

seaOfProgram :: (Show a, Show n, Pretty n, Ord n)
             => Program (Annot a) n Prim -> Doc
seaOfProgram program
 =  vsep
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
stateOfProgram program
 = vsep
 [ "typedef struct {"
 , "    /* runtime */"
 , "    imempool_t       mempool;"
 , ""
 , "    /* inputs */"
 , "    idate_t          gen_date;"
 , "    iint_t           new_count;"
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
 = 3 -- mempool
 + 1 -- gen_date
 + 1 -- new_count
 + length (maybe [] snd (factVarsOfProgram FactLoopNew program))
 + sum (fmap (length . snd . snd) (outputsOfProgram program))
 + 2 * Map.size (resumablesOfProgram program)

defOfAccumulator :: (Show n, Pretty n, Ord n)
                  => (Name n, ValType) -> Doc
defOfAccumulator (n, vt)
 = seaOfValType vt <+> seaOfName n <> semi

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
      | Accumulator n _ xx <- acc
      -> assign (seaOfName n) (seaOfExp xx) <> semi <> suffix "init" <> line
      <> seaOfStatement stmt'

     Read n_val n_acc _ stmt'
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
     XValue _ t v
      -> seaOfXValue v t

     XVar _ n
      -> seaOfName n

     XApp{}
      | Just (p, xs) <- takePrimApps xx
      -> seaOfPrimDocApps (seaOfXPrim p) (fmap seaOfExp xs)

     _
      -> seaError "seaOfExp" xx

seaOfXValue :: BaseValue -> ValType -> Doc
seaOfXValue v t
 = case v of
     VError    err   -> seaOfError err
     VBool     True  -> "itrue"
     VBool     False -> "ifalse"
     VInt      x     -> int x
     VDouble   x     -> double x
     VDateTime x     -> text ("0x" <> showHex (packedOfDate x) "")

     -- TODO C escapes /= Haskell escapes
     VString x     -> text (show x)

     VArray vs
      | ArrayT t' <- t
      -> let len = length vs
             writes arr (v',i)
              = prim (PrimUpdate $ PrimUpdateArrayPut t')
                     [arr, int i, seaOfXValue v' t']
             init
              = prim (PrimUnsafe $ PrimUnsafeArrayCreate t')
                     [int len]
        in  foldl writes init (vs `List.zip` [0..])

      | otherwise
      -> seaError "seaOfXValue: array of wrong type" (v,t)

     VBuf len vs
      | BufT t' <- t
      -> let writes buf v'
              = prim (PrimBuf $ PrimBufPush t')
                     [buf, seaOfXValue v' t']
             init
              = prim (PrimBuf $ PrimBufMake t')
                     [int len]
        in  foldl writes init vs
      | otherwise
      -> seaError "seaOfXValue: buffer of wrong type" (v,t)

     VMap _
      -> seaError "seaOfXValue: maps should be removed by flatten" v
     _
      -> seaError "seaOfXValue: this should be removed by melt" v
 where
  prim p args
   = seaOfPrimDocApps (seaOfXPrim p) args

seaOfError :: ExceptionInfo -> Doc
seaOfError e
 = case e of
     ExceptTombstone
      -> "ierror_tombstone"

     ExceptFold1NoValue
      -> "ierror_fold1_no_value"

     ExceptScalarVariableNotAvailable
      -> "ierror_variable_not_available"

